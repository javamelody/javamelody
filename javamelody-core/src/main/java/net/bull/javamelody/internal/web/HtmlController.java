/*
 * Copyright 2008-2017 by Emeric Vernat
 *
 *     This file is part of Java Melody.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package net.bull.javamelody.internal.web; // NOPMD

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import net.bull.javamelody.JdbcWrapper;
import net.bull.javamelody.SessionListener;
import net.bull.javamelody.internal.common.HttpParameter;
import net.bull.javamelody.internal.common.HttpPart;
import net.bull.javamelody.internal.common.I18N;
import net.bull.javamelody.internal.common.LOG;
import net.bull.javamelody.internal.common.Parameters;
import net.bull.javamelody.internal.model.Action;
import net.bull.javamelody.internal.model.CacheInformations;
import net.bull.javamelody.internal.model.Collector;
import net.bull.javamelody.internal.model.CollectorServer;
import net.bull.javamelody.internal.model.DatabaseInformations;
import net.bull.javamelody.internal.model.HeapHistogram;
import net.bull.javamelody.internal.model.JavaInformations;
import net.bull.javamelody.internal.model.JndiBinding;
import net.bull.javamelody.internal.model.MBeanNode;
import net.bull.javamelody.internal.model.MBeans;
import net.bull.javamelody.internal.model.Period;
import net.bull.javamelody.internal.model.ProcessInformations;
import net.bull.javamelody.internal.model.Range;
import net.bull.javamelody.internal.model.SamplingProfiler.SampledMethod;
import net.bull.javamelody.internal.model.SessionInformations;
import net.bull.javamelody.internal.model.VirtualMachine;
import net.bull.javamelody.internal.web.RequestToMethodMapper.RequestParameter;
import net.bull.javamelody.internal.web.RequestToMethodMapper.RequestPart;
import net.bull.javamelody.internal.web.html.HtmlReport;

/**
 * Contrôleur au sens MVC de l'ihm de monitoring pour la partie html.
 * @author Emeric Vernat
 */
public class HtmlController {
	static final String HTML_BODY_FORMAT = "htmlbody";
	private static final RequestToMethodMapper<HtmlController> REQUEST_TO_METHOD_MAPPER = new RequestToMethodMapper<HtmlController>(
			HtmlController.class);
	private final HttpCookieManager httpCookieManager = new HttpCookieManager();
	private final Collector collector;
	private final CollectorServer collectorServer;
	private final String messageForReport;
	private final String anchorNameForRedirect;
	private HtmlReport htmlReport;

	HtmlController(Collector collector, CollectorServer collectorServer, String messageForReport,
			String anchorNameForRedirect) {
		super();
		assert collector != null;
		this.collector = collector;
		this.collectorServer = collectorServer;
		this.messageForReport = messageForReport;
		this.anchorNameForRedirect = anchorNameForRedirect;
	}

	void doHtml(HttpServletRequest httpRequest, HttpServletResponse httpResponse,
			List<JavaInformations> javaInformationsList) throws IOException {
		final String part = HttpParameter.PART.getParameterFrom(httpRequest);
		if (!isFromCollectorServer() && isLocalCollectNeeded(part) && !collector.isStopped()) {
			// avant de faire l'affichage on fait une collecte, pour que les courbes
			// et les compteurs par jour soit à jour avec les dernières requêtes,
			// sauf si c'est un serveur de collecte
			// ou si la page de monitoring d'une webapp monitorée par un serveur de collecte est appelée par erreur
			collector.collectLocalContextWithoutErrors();
		}

		// simple appel de monitoring sans format
		final BufferedWriter writer = getWriter(httpResponse);
		try {
			final Range range = httpCookieManager.getRange(httpRequest, httpResponse);
			this.htmlReport = new HtmlReport(collector, collectorServer, javaInformationsList,
					range, writer);
			if (part == null) {
				htmlReport.toHtml(messageForReport, anchorNameForRedirect);
			} else if (HttpPart.THREADS_DUMP.isPart(httpRequest)) {
				httpResponse.setContentType("text/plain; charset=UTF-8");
				htmlReport.writeThreadsDump();
			} else {
				REQUEST_TO_METHOD_MAPPER.invoke(httpRequest, this);
			}
		} finally {
			writer.close();
		}
	}

	static boolean isLocalCollectNeeded(String part) {
		return part == null || HttpPart.CURRENT_REQUESTS.getName().equals(part)
				|| HttpPart.GRAPH.getName().equals(part)
				|| HttpPart.COUNTER_SUMMARY_PER_CLASS.getName().equals(part);
	}

	public static BufferedWriter getWriter(HttpServletResponse httpResponse) throws IOException {
		httpResponse.setContentType("text/html; charset=UTF-8");
		httpResponse.setHeader("X-Frame-Options", "SAMEORIGIN");
		return new BufferedWriter(new OutputStreamWriter(httpResponse.getOutputStream(), "UTF-8"));
	}

	@RequestPart(HttpPart.GRAPH)
	void doRequestGraphAndDetail(@RequestParameter(HttpParameter.GRAPH) String graphName)
			throws IOException {
		htmlReport.writeRequestAndGraphDetail(graphName);
	}

	@RequestPart(HttpPart.USAGES)
	void doRequestUsages(@RequestParameter(HttpParameter.GRAPH) String graphName)
			throws IOException {
		htmlReport.writeRequestUsages(graphName);
	}

	@RequestPart(HttpPart.CURRENT_REQUESTS)
	void doAllCurrentRequestsAsPart() throws IOException {
		htmlReport.writeAllCurrentRequestsAsPart();
	}

	@RequestPart(HttpPart.THREADS)
	void doAllThreadsAsPart() throws IOException {
		htmlReport.writeAllThreadsAsPart();
	}

	@RequestPart(HttpPart.COUNTER_SUMMARY_PER_CLASS)
	void doCounterSummaryPerClass(@RequestParameter(HttpParameter.COUNTER) String counterName,
			@RequestParameter(HttpParameter.GRAPH) String requestId) throws IOException {
		htmlReport.writeCounterSummaryPerClass(counterName, requestId);
	}

	@RequestPart(HttpPart.SOURCE)
	void doSource(@RequestParameter(HttpParameter.CLASS) String className) throws IOException {
		htmlReport.writeSource(className);
	}

	@RequestPart(HttpPart.DEPENDENCIES)
	void doDependencies() throws IOException {
		htmlReport.writeDependencies();
	}

	@RequestPart(HttpPart.SESSIONS)
	void doSessions(@RequestParameter(HttpParameter.SESSION_ID) String sessionId)
			throws IOException {
		// par sécurité
		Action.checkSystemActionsEnabled();
		final List<SessionInformations> sessionsInformations;
		if (!isFromCollectorServer()) {
			if (sessionId == null) {
				sessionsInformations = SessionListener.getAllSessionsInformations();
			} else {
				sessionsInformations = Collections.singletonList(
						SessionListener.getSessionInformationsBySessionId(sessionId));
			}
		} else {
			sessionsInformations = collectorServer.collectSessionInformations(getApplication(),
					sessionId);
		}
		if (sessionId == null || sessionsInformations.isEmpty()) {
			htmlReport.writeSessions(sessionsInformations, messageForReport,
					HttpPart.SESSIONS.getName());
		} else {
			final SessionInformations sessionInformation = sessionsInformations.get(0);
			htmlReport.writeSessionDetail(sessionId, sessionInformation);
		}
	}

	@RequestPart(HttpPart.HOTSPOTS)
	void doHotspots() throws IOException {
		// par sécurité
		Action.checkSystemActionsEnabled();
		if (!isFromCollectorServer()) {
			final List<SampledMethod> hotspots = collector.getHotspots();
			htmlReport.writeHotspots(hotspots);
		} else {
			final List<SampledMethod> hotspots = collectorServer.collectHotspots(getApplication());
			htmlReport.writeHotspots(hotspots);
		}
	}

	@RequestPart(HttpPart.HEAP_HISTO)
	void doHeapHisto() throws IOException {
		// par sécurité
		Action.checkSystemActionsEnabled();
		final HeapHistogram heapHistogram;
		try {
			if (!isFromCollectorServer()) {
				heapHistogram = VirtualMachine.createHeapHistogram();
			} else {
				heapHistogram = collectorServer.collectHeapHistogram(getApplication());
			}
		} catch (final Exception e) {
			LOG.warn("heaphisto report failed", e);
			htmlReport.writeMessageIfNotNull(String.valueOf(e.getMessage()), null);
			return;
		}
		htmlReport.writeHeapHistogram(heapHistogram, messageForReport,
				HttpPart.HEAP_HISTO.getName());
	}

	@RequestPart(HttpPart.PROCESSES)
	void doProcesses() throws IOException {
		// par sécurité
		Action.checkSystemActionsEnabled();
		try {
			if (!isFromCollectorServer()) {
				final List<ProcessInformations> processInformationsList = ProcessInformations
						.buildProcessInformations();
				htmlReport.writeProcesses(processInformationsList);
			} else {
				final Map<String, List<ProcessInformations>> processInformationsByTitle = collectorServer
						.collectProcessInformations(getApplication());
				htmlReport.writeProcesses(processInformationsByTitle);
			}
		} catch (final Exception e) {
			LOG.warn("processes report failed", e);
			htmlReport.writeMessageIfNotNull(String.valueOf(e.getMessage()), null);
		}
	}

	@RequestPart(HttpPart.DATABASE)
	void doDatabase(@RequestParameter(HttpParameter.REQUEST) String requestIndex)
			throws IOException {
		// par sécurité
		Action.checkSystemActionsEnabled();
		try {
			final int index = DatabaseInformations.parseRequestIndex(requestIndex);
			final DatabaseInformations databaseInformations;
			if (!isFromCollectorServer()) {
				databaseInformations = new DatabaseInformations(index);
			} else {
				databaseInformations = collectorServer.collectDatabaseInformations(getApplication(),
						index);
			}
			htmlReport.writeDatabase(databaseInformations);
		} catch (final Exception e) {
			LOG.warn("database report failed", e);
			htmlReport.writeMessageIfNotNull(String.valueOf(e.getMessage()), null);
		}
	}

	@RequestPart(HttpPart.CONNECTIONS)
	void doConnections(@RequestParameter(HttpParameter.FORMAT) String format) throws IOException {
		assert !isFromCollectorServer();
		// par sécurité
		Action.checkSystemActionsEnabled();
		final boolean withoutHeaders = HTML_BODY_FORMAT.equalsIgnoreCase(format);
		htmlReport.writeConnections(JdbcWrapper.getConnectionInformationsList(), withoutHeaders);
	}

	@RequestPart(HttpPart.JNDI)
	void doJndi(@RequestParameter(HttpParameter.PATH) String path) throws IOException {
		// par sécurité
		Action.checkSystemActionsEnabled();
		try {
			final List<JndiBinding> jndiBindings;
			if (!isFromCollectorServer()) {
				jndiBindings = JndiBinding.listBindings(path);
			} else {
				jndiBindings = collectorServer.collectJndiBindings(getApplication(), path);
			}
			htmlReport.writeJndi(jndiBindings, path);
		} catch (final Exception e) {
			LOG.warn("jndi report failed", e);
			htmlReport.writeMessageIfNotNull(String.valueOf(e.getMessage()), null);
		}
	}

	@RequestPart(HttpPart.MBEANS)
	void doMBeans() throws IOException {
		// par sécurité
		Action.checkSystemActionsEnabled();
		try {
			if (!isFromCollectorServer()) {
				final List<MBeanNode> nodes = MBeans.getAllMBeanNodes();
				htmlReport.writeMBeans(nodes);
			} else {
				final Map<String, List<MBeanNode>> allMBeans = collectorServer
						.collectMBeans(getApplication());
				htmlReport.writeMBeans(allMBeans);
			}
		} catch (final Exception e) {
			LOG.warn("mbeans report failed", e);
			htmlReport.writeMessageIfNotNull(String.valueOf(e.getMessage()), null);
		}
	}

	@RequestPart(HttpPart.CRASHES)
	void doCrashes() throws IOException {
		Action.checkSystemActionsEnabled();
		htmlReport.writeCrashes();
	}

	@RequestPart(HttpPart.SPRING_BEANS)
	void doSpringBeans() throws IOException {
		htmlReport.writeSpringContext();
	}

	@RequestPart(HttpPart.CACHE_KEYS)
	void doCacheKeys(@RequestParameter(HttpParameter.CACHE_ID) String cacheId,
			@RequestParameter(HttpParameter.FORMAT) String format) throws IOException {
		assert !isFromCollectorServer();
		final CacheInformations cacheInfo = CacheInformations
				.buildCacheInformationsWithKeys(cacheId);
		final boolean withoutHeaders = HTML_BODY_FORMAT.equalsIgnoreCase(format);
		final String cacheKeysPart = HttpPart.CACHE_KEYS.toString() + '&' + HttpParameter.CACHE_ID
				+ '=' + I18N.urlEncode(cacheId);
		htmlReport.writeCacheWithKeys(cacheId, cacheInfo, messageForReport, cacheKeysPart,
				withoutHeaders);
	}

	void writeHtmlToLastShutdownFile() {
		try {
			final File dir = Parameters.getStorageDirectory(getApplication());
			if (!dir.mkdirs() && !dir.exists()) {
				throw new IOException("JavaMelody directory can't be created: " + dir.getPath());
			}
			final File lastShutdownFile = new File(dir, "last_shutdown.html");
			final BufferedWriter writer = new BufferedWriter(new FileWriter(lastShutdownFile));
			try {
				final JavaInformations javaInformations = new JavaInformations(
						Parameters.getServletContext(), true);
				// on pourrait faire I18N.bindLocale(Locale.getDefault()), mais cela se fera tout seul
				final HtmlReport myHtmlReport = new HtmlReport(collector, collectorServer,
						Collections.singletonList(javaInformations), Period.JOUR, writer);
				myHtmlReport.writeLastShutdown();
			} finally {
				writer.close();
			}
		} catch (final IOException e) {
			LOG.warn("exception while writing the last shutdown report", e);
		}
	}

	private String getApplication() {
		return collector.getApplication();
	}

	private boolean isFromCollectorServer() {
		return collectorServer != null;
	}
}
