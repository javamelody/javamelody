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

import java.io.IOException;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import net.bull.javamelody.SessionListener;
import net.bull.javamelody.internal.common.HttpParameter;
import net.bull.javamelody.internal.common.HttpPart;
import net.bull.javamelody.internal.common.I18N;
import net.bull.javamelody.internal.model.Action;
import net.bull.javamelody.internal.model.Collector;
import net.bull.javamelody.internal.model.CollectorServer;
import net.bull.javamelody.internal.model.Counter;
import net.bull.javamelody.internal.model.CounterRequestContext;
import net.bull.javamelody.internal.model.DatabaseInformations;
import net.bull.javamelody.internal.model.HeapHistogram;
import net.bull.javamelody.internal.model.JavaInformations;
import net.bull.javamelody.internal.model.JndiBinding;
import net.bull.javamelody.internal.model.MBeanNode;
import net.bull.javamelody.internal.model.MBeans;
import net.bull.javamelody.internal.model.ProcessInformations;
import net.bull.javamelody.internal.model.Range;
import net.bull.javamelody.internal.model.SamplingProfiler.SampledMethod;
import net.bull.javamelody.internal.model.SessionInformations;
import net.bull.javamelody.internal.model.VirtualMachine;
import net.bull.javamelody.internal.web.RequestToMethodMapper.RequestAttribute;
import net.bull.javamelody.internal.web.RequestToMethodMapper.RequestParameter;
import net.bull.javamelody.internal.web.RequestToMethodMapper.RequestPart;
import net.bull.javamelody.internal.web.pdf.PdfOtherReport;
import net.bull.javamelody.internal.web.pdf.PdfReport;

/**
 * Contrôleur au sens MVC de l'ihm de monitoring pour la partie pdf.
 * @author Emeric Vernat
 */
class PdfController {
	private static final String RANGE_KEY = "range";
	private static final String JAVA_INFORMATIONS_LIST_KEY = "javaInformationsList";
	private static final RequestToMethodMapper<PdfController> REQUEST_TO_METHOD_MAPPER = new RequestToMethodMapper<PdfController>(
			PdfController.class);
	private final HttpCookieManager httpCookieManager = new HttpCookieManager();
	private final Collector collector;
	private final CollectorServer collectorServer;
	private PdfOtherReport pdfOtherReport;

	PdfController(Collector collector, CollectorServer collectorServer) {
		super();
		assert collector != null;
		this.collector = collector;
		this.collectorServer = collectorServer;
	}

	void doPdf(HttpServletRequest httpRequest, HttpServletResponse httpResponse,
			List<JavaInformations> javaInformationsList) throws IOException {
		addPdfContentTypeAndDisposition(httpRequest, httpResponse);
		try {
			final String part = HttpParameter.PART.getParameterFrom(httpRequest);
			final Range range = httpCookieManager.getRange(httpRequest, httpResponse);
			if (PdfReport.shouldUseEnglishInsteadOfUkrainian()) {
				I18N.bindLocale(Locale.ENGLISH);
			}
			if (part == null) {
				if (!isFromCollectorServer() && !collector.isStopped()) {
					// avant de faire l'affichage on fait une collecte,  pour que les courbes
					// et les compteurs par jour soit à jour avec les dernières requêtes
					// sauf si c'est un serveur de collecte
					// ou si la page de monitoring d'une webapp monitorée par un serveur de collecte est appelée par erreur
					collector.collectLocalContextWithoutErrors();
				}

				final PdfReport pdfReport = new PdfReport(collector, isFromCollectorServer(),
						javaInformationsList, range, httpResponse.getOutputStream());
				pdfReport.toPdf();
			} else {
				this.pdfOtherReport = new PdfOtherReport(getApplication(),
						httpResponse.getOutputStream());
				// range et javaInformationsList sont passés en attribut de la requête avec @RequestAttribute
				// au lieu d'être en paramètre de new PdfOtherReport
				httpRequest.setAttribute(RANGE_KEY, range);
				httpRequest.setAttribute(JAVA_INFORMATIONS_LIST_KEY, javaInformationsList);
				REQUEST_TO_METHOD_MAPPER.invoke(httpRequest, this);
			}
		} finally {
			httpResponse.getOutputStream().flush();
		}
	}

	@RequestPart(HttpPart.CURRENT_REQUESTS)
	void doCurrentRequests(
			@RequestAttribute(JAVA_INFORMATIONS_LIST_KEY) List<JavaInformations> javaInformationsList,
			@RequestAttribute(RANGE_KEY) Range range) throws IOException {
		final List<Counter> counters = collector.getRangeCountersToBeDisplayed(range);
		final Map<JavaInformations, List<CounterRequestContext>> currentRequests;
		if (!isFromCollectorServer()) {
			// on est dans l'application monitorée
			assert collectorServer == null;
			assert javaInformationsList.size() == 1;
			final JavaInformations javaInformations = javaInformationsList.get(0);
			final List<CounterRequestContext> rootCurrentContexts = collector
					.getRootCurrentContexts(counters);
			currentRequests = Collections.singletonMap(javaInformations, rootCurrentContexts);
		} else {
			currentRequests = collectorServer.collectCurrentRequests(collector.getApplication());
		}
		final long timeOfSnapshot = System.currentTimeMillis();
		pdfOtherReport.writeAllCurrentRequestsAsPart(currentRequests, collector, counters,
				timeOfSnapshot);
	}

	@RequestPart(HttpPart.THREADS)
	void doThreads(
			@RequestAttribute(JAVA_INFORMATIONS_LIST_KEY) List<JavaInformations> javaInformationsList)
			throws IOException {
		pdfOtherReport.writeThreads(javaInformationsList);
	}

	@RequestPart(HttpPart.RUNTIME_DEPENDENCIES)
	void doRuntimeDependencies(@RequestAttribute(RANGE_KEY) Range range,
			@RequestParameter(HttpParameter.COUNTER) String counterName) throws IOException {
		final Counter counter = collector.getRangeCounter(range, counterName);
		pdfOtherReport.writeRuntimeDependencies(counter, range);
	}

	@RequestPart(HttpPart.COUNTER_SUMMARY_PER_CLASS)
	void doCounterSummaryPerClass(@RequestAttribute(RANGE_KEY) Range range,
			@RequestParameter(HttpParameter.GRAPH) String requestId,
			@RequestParameter(HttpParameter.COUNTER) String counterName) throws IOException {
		final Counter counter = collector.getRangeCounter(range, counterName);
		pdfOtherReport.writeCounterSummaryPerClass(collector, counter, requestId, range);
	}

	@RequestPart(HttpPart.GRAPH)
	void doRequestAndGraphDetail(@RequestAttribute(RANGE_KEY) Range range,
			@RequestParameter(HttpParameter.GRAPH) String requestId) throws IOException {
		pdfOtherReport.writeRequestAndGraphDetail(collector, collectorServer, range, requestId);
	}

	@RequestPart(HttpPart.SESSIONS)
	void doSessions() throws IOException {
		// par sécurité
		Action.checkSystemActionsEnabled();
		final List<SessionInformations> sessionsInformations;
		if (!isFromCollectorServer()) {
			sessionsInformations = SessionListener.getAllSessionsInformations();
		} else {
			sessionsInformations = collectorServer.collectSessionInformations(getApplication(),
					null);
		}
		pdfOtherReport.writeSessionInformations(sessionsInformations);
	}

	@RequestPart(HttpPart.HOTSPOTS)
	void doHotspots() throws IOException {
		// par sécurité
		Action.checkSystemActionsEnabled();
		if (!isFromCollectorServer()) {
			final List<SampledMethod> hotspots = collector.getHotspots();
			pdfOtherReport.writeHotspots(hotspots);
		} else {
			final List<SampledMethod> hotspots = collectorServer.collectHotspots(getApplication());
			pdfOtherReport.writeHotspots(hotspots);
		}
	}

	@RequestPart(HttpPart.PROCESSES)
	void doProcesses() throws IOException {
		// par sécurité
		Action.checkSystemActionsEnabled();
		if (!isFromCollectorServer()) {
			final List<ProcessInformations> processInformations = ProcessInformations
					.buildProcessInformations();
			pdfOtherReport.writeProcessInformations(processInformations);
		} else {
			final Map<String, List<ProcessInformations>> processesByTitle = collectorServer
					.collectProcessInformations(getApplication());
			pdfOtherReport.writeProcessInformations(processesByTitle);
		}
	}

	@RequestPart(HttpPart.DATABASE)
	void doDatabase(@RequestParameter(HttpParameter.REQUEST) String requestIndex) throws Exception { // NOPMD
		// par sécurité
		Action.checkSystemActionsEnabled();
		final int index = DatabaseInformations.parseRequestIndex(requestIndex);
		final DatabaseInformations databaseInformations;
		if (!isFromCollectorServer()) {
			databaseInformations = new DatabaseInformations(index);
		} else {
			databaseInformations = collectorServer.collectDatabaseInformations(getApplication(),
					index);
		}
		pdfOtherReport.writeDatabaseInformations(databaseInformations);
	}

	@RequestPart(HttpPart.JNDI)
	void doJndi(@RequestParameter(HttpParameter.PATH) String path) throws Exception { // NOPMD
		// par sécurité
		Action.checkSystemActionsEnabled();
		final List<JndiBinding> jndiBindings;
		if (!isFromCollectorServer()) {
			jndiBindings = JndiBinding.listBindings(path);
		} else {
			jndiBindings = collectorServer.collectJndiBindings(getApplication(), path);
		}
		pdfOtherReport.writeJndi(jndiBindings, JndiBinding.normalizePath(path));
	}

	@RequestPart(HttpPart.MBEANS)
	void doMBeans() throws Exception { // NOPMD
		// par sécurité
		Action.checkSystemActionsEnabled();
		if (!isFromCollectorServer()) {
			final List<MBeanNode> nodes = MBeans.getAllMBeanNodes();
			pdfOtherReport.writeMBeans(nodes);
		} else {
			final Map<String, List<MBeanNode>> allMBeans = collectorServer
					.collectMBeans(getApplication());
			pdfOtherReport.writeMBeans(allMBeans);
		}
	}

	@RequestPart(HttpPart.HEAP_HISTO)
	void doHeapHisto() throws Exception { // NOPMD
		// par sécurité
		Action.checkSystemActionsEnabled();
		final HeapHistogram heapHistogram;
		if (!isFromCollectorServer()) {
			heapHistogram = VirtualMachine.createHeapHistogram();
		} else {
			heapHistogram = collectorServer.collectHeapHistogram(getApplication());
		}
		pdfOtherReport.writeHeapHistogram(heapHistogram);
	}

	void addPdfContentTypeAndDisposition(HttpServletRequest httpRequest,
			HttpServletResponse httpResponse) {
		httpResponse.setContentType("application/pdf");
		final String contentDisposition = encodeFileNameToContentDisposition(httpRequest,
				PdfReport.getFileName(getApplication()));
		// encoding des CRLF pour http://en.wikipedia.org/wiki/HTTP_response_splitting
		httpResponse.addHeader("Content-Disposition",
				contentDisposition.replace('\n', '_').replace('\r', '_'));
	}

	private String getApplication() {
		return collector.getApplication();
	}

	private boolean isFromCollectorServer() {
		return collectorServer != null;
	}

	/**
	 * Encode un nom de fichier avec des % pour Content-Disposition, avec téléchargement.
	 * (US-ASCII + Encode-Word : http://www.ietf.org/rfc/rfc2183.txt, http://www.ietf.org/rfc/rfc2231.txt
	 * sauf en MS IE qui ne supporte pas cet encodage et qui n'en a pas besoin)
	 * @param httpRequest HttpServletRequest
	 * @param fileName String
	 * @return String
	 */
	private static String encodeFileNameToContentDisposition(HttpServletRequest httpRequest,
			String fileName) {
		assert fileName != null;
		final String userAgent = httpRequest.getHeader("user-agent");
		if (userAgent != null && userAgent.contains("MSIE")) {
			return "attachment;filename=" + fileName;
		}
		return encodeFileNameToStandardContentDisposition(fileName);
	}

	private static String encodeFileNameToStandardContentDisposition(String fileName) {
		final int length = fileName.length();
		final StringBuilder sb = new StringBuilder(length + length / 4);
		// attachment et non inline pour proposer l'enregistrement (sauf IE6)
		// et non l'affichage direct dans le navigateur
		sb.append("attachment;filename=\"");
		char c;
		for (int i = 0; i < length; i++) {
			c = fileName.charAt(i);
			if (isEncodingNotNeeded(c)) {
				sb.append(c);
			} else {
				sb.append('%');
				if (c < 16) {
					sb.append('0');
				}
				sb.append(Integer.toHexString(c));
			}
		}
		sb.append('"');
		return sb.toString();
	}

	private static boolean isEncodingNotNeeded(char c) {
		return c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c >= '0' && c <= '9' || c == '.'
				|| c == '_';
	}
}
