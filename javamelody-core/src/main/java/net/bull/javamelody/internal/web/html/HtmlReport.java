/*
 * Copyright 2008-2019 by Emeric Vernat
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
package net.bull.javamelody.internal.web.html;

import java.io.IOException;
import java.io.InputStream;
import java.io.Writer;
import java.net.URL;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import net.bull.javamelody.Parameter;
import net.bull.javamelody.SpringContext;
import net.bull.javamelody.internal.common.I18N;
import net.bull.javamelody.internal.common.InputOutput;
import net.bull.javamelody.internal.common.Parameters;
import net.bull.javamelody.internal.model.CacheInformations;
import net.bull.javamelody.internal.model.Collector;
import net.bull.javamelody.internal.model.CollectorServer;
import net.bull.javamelody.internal.model.ConnectionInformations;
import net.bull.javamelody.internal.model.Counter;
import net.bull.javamelody.internal.model.CounterRequestContext;
import net.bull.javamelody.internal.model.DatabaseInformations;
import net.bull.javamelody.internal.model.HeapHistogram;
import net.bull.javamelody.internal.model.HsErrPid;
import net.bull.javamelody.internal.model.JCacheInformations;
import net.bull.javamelody.internal.model.JavaInformations;
import net.bull.javamelody.internal.model.JndiBinding;
import net.bull.javamelody.internal.model.MBeanNode;
import net.bull.javamelody.internal.model.MavenArtifact;
import net.bull.javamelody.internal.model.Period;
import net.bull.javamelody.internal.model.ProcessInformations;
import net.bull.javamelody.internal.model.Range;
import net.bull.javamelody.internal.model.SamplingProfiler.SampledMethod;
import net.bull.javamelody.internal.model.SessionInformations;

/**
 * Rapport html.
 * @author Emeric Vernat
 */
public class HtmlReport extends HtmlAbstractReport {
	private static final String SCRIPT_BEGIN = "<script type='text/javascript'>";
	private static final String SCRIPT_END = "</script>";
	private static final URL THEMED_MONITORING_CSS = HtmlReport.class
			.getResource("/net/bull/javamelody/resource/themedMonitoring.css");
	private static final URL THEMED_MONITORING_JS = HtmlReport.class
			.getResource("/net/bull/javamelody/resource/themedMonitoring.js");

	private final Collector collector;
	private final CollectorServer collectorServer;
	private final List<JavaInformations> javaInformationsList;
	private final Range range;
	private final HtmlCoreReport htmlCoreReport;

	public HtmlReport(Collector collector, CollectorServer collectorServer,
			List<JavaInformations> javaInformationsList, Range range, Writer writer) {
		super(writer);
		assert collector != null;
		assert javaInformationsList != null && !javaInformationsList.isEmpty();
		assert range != null;

		this.collector = collector;
		this.collectorServer = collectorServer;
		this.javaInformationsList = javaInformationsList;
		this.range = range;
		this.htmlCoreReport = new HtmlCoreReport(collector, collectorServer, javaInformationsList,
				range, writer);
	}

	public HtmlReport(Collector collector, CollectorServer collectorServer,
			List<JavaInformations> javaInformationsList, Period period, Writer writer) {
		this(collector, collectorServer, javaInformationsList, period.getRange(), writer);
	}

	@Override
	void toHtml() throws IOException {
		writeHtmlHeader();
		htmlCoreReport.toHtml();
		writeHtmlFooter();
	}

	public void toHtml(String message, String anchorNameForRedirect) throws IOException {
		writeHtmlHeader();
		htmlCoreReport.toHtml(message, anchorNameForRedirect);
		writeHtmlFooter();
	}

	public void writeLastShutdown() throws IOException {
		writeHtmlHeader(false, true);
		htmlCoreReport.toHtml(null, null);
		writeHtmlFooter();
	}

	public static void writeAddAndRemoveApplicationLinks(String currentApplication,
			Collection<String> applications, Writer writer) throws IOException {
		HtmlCoreReport.writeAddAndRemoveApplicationLinks(currentApplication, applications, writer);
	}

	public void writeAllCurrentRequestsAsPart() throws IOException {
		writeHtmlHeader();
		final List<Counter> counters = collector.getRangeCountersToBeDisplayed(range);
		final Map<JavaInformations, List<CounterRequestContext>> currentRequests;
		if (collectorServer == null) {
			assert javaInformationsList.size() == 1;
			final JavaInformations javaInformations = javaInformationsList.get(0);
			final List<CounterRequestContext> rootCurrentContexts = collector
					.getRootCurrentContexts(counters);
			currentRequests = Collections.singletonMap(javaInformations, rootCurrentContexts);
		} else {
			currentRequests = collectorServer.collectCurrentRequests(collector.getApplication());
			final List<CounterRequestContext> allCurrentRequests = new ArrayList<CounterRequestContext>();
			for (final List<CounterRequestContext> rootCurrentContexts : currentRequests.values()) {
				allCurrentRequests.addAll(rootCurrentContexts);
			}
			CounterRequestContext.replaceParentCounters(allCurrentRequests, counters);
		}
		htmlCoreReport.writeAllCurrentRequestsAsPart(currentRequests);
		writeHtmlFooter();
	}

	public void writeAllThreadsAsPart() throws IOException {
		writeHtmlHeader();
		htmlCoreReport.writeAllThreadsAsPart();
		writeHtmlFooter();
	}

	public void writeThreadsDump() throws IOException {
		htmlCoreReport.writeThreadsDump();
	}

	public void writeCounterSummaryPerClass(String counterName, String requestId)
			throws IOException {
		writeHtmlHeader();
		htmlCoreReport.writeCounterSummaryPerClass(counterName, requestId);
		writeHtmlFooter();
	}

	public void writeSource(String className) throws IOException {
		assert className != null;
		writeHtmlHeader();
		new HtmlSourceReport(className, getWriter()).toHtml();
		writeHtmlFooter();
	}

	public void writeDependencies() throws IOException {
		writeHtmlHeader();
		final Map<String, MavenArtifact> dependencies = MavenArtifact.getWebappDependencies();
		new HtmlDependenciesReport(dependencies, getWriter()).toHtml();
		writeHtmlFooter();
	}

	public void writeSpringContext() throws IOException {
		writeHtmlHeader();
		final SpringContext springContext = SpringContext.getSingleton();
		new HtmlSpringContextReport(springContext, getWriter()).toHtml();
		writeHtmlFooter();
	}

	public void writeHtmlHeader() throws IOException {
		writeHtmlHeader(false, false);
	}

	private void writeHtmlHeader(boolean includeSlider, boolean includeCssInline)
			throws IOException {
		writeln("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">");
		writeDirectly("<html lang='" + I18N.getCurrentLocale().getLanguage() + "'><head><title>"
				+ getFormattedString("Monitoring_sur", collector.getApplication()) + "</title>");
		writeln("");
		if (includeCssInline) {
			writeln("<style type='text/css'>");
			final InputStream in = getClass()
					.getResourceAsStream(Parameters.getResourcePath("monitoring.css"));
			try {
				final String monitoringCss = InputOutput.pumpToString(in, Charset.forName("UTF-8"));
				writeDirectly(monitoringCss);
			} finally {
				in.close();
			}
			writeln("</style>");
		} else {
			writeln("<link rel='stylesheet' href='?resource=monitoring.css' type='text/css'/>");
			if (THEMED_MONITORING_CSS != null) {
				writeln("<link rel='stylesheet' href='?resource=themedMonitoring.css' type='text/css'/>");
			}
			writeln("<link rel='stylesheet' href='?resource=customizableMonitoring.css' type='text/css'/>");
		}
		writeln("<link type='image/png' rel='shortcut icon' href='?resource=systemmonitor.png' />");
		writeln("<script type='text/javascript' src='?resource=resizable_tables.js'></script>");
		writeln("<script type='text/javascript' src='?resource=sorttable.js'></script>");
		// prototype.js nécessaire pour effects.js et slider.js
		writeln("<script type='text/javascript' src='?resource=prototype.js'></script>");
		// Effect slidedown/slideup décrit ici http://madrobby.github.com/scriptaculous/effect-slidedown/
		writeln("<script type='text/javascript' src='?resource=effects.js'></script>");
		// open dialog (for java sources), http://www.p51labs.com/lightwindow/
		writeln("<script type='text/javascript' src='?resource=lightwindow.js'></script>");
		if (includeSlider) {
			writeln("<script type='text/javascript' src='?resource=slider.js'></script>");
		}
		if (THEMED_MONITORING_JS != null) {
			writeln("<script type='text/javascript' src='?resource=themedMonitoring.js'></script>");
		}
		writeln("<script type='text/javascript' src='?resource=customizableMonitoring.js'></script>");
		writeJavaScript();
		writeln("</head><body>");
	}

	public void writeHtmlFooter() throws IOException {
		final String analyticsId = Parameter.ANALYTICS_ID.getValue();
		if (analyticsId != null && !"disabled".equals(analyticsId)) {
			writeDirectly(SCRIPT_BEGIN);
			writeDirectly(
					"var gaJsHost = (('https:' == document.location.protocol) ? 'https://ssl.' : 'http://www.');\n");
			writeDirectly(
					"document.write(unescape(\"%3Cscript src='\" + gaJsHost + \"google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E\"));\n");
			writeDirectly(SCRIPT_END);
			writeDirectly(SCRIPT_BEGIN);
			writeDirectly(" try{\n");
			writeDirectly("var pageTracker = _gat._getTracker('" + analyticsId + "');\n");
			writeDirectly("pageTracker._trackPageview();\n");
			writeDirectly("} catch(err) {}\n");
			writeDirectly(SCRIPT_END);
			writeDirectly("\n");
		}
		writeln("</body></html>");
	}

	private void writeJavaScript() throws IOException {
		writeln(SCRIPT_BEGIN);
		writeln("function showHide(id){");
		writeln("  if (document.getElementById(id).style.display=='none') {");
		writeln("    if (document.getElementById(id + 'Img') != null) {");
		writeln("      document.getElementById(id + 'Img').src='?resource=bullets/minus.png';");
		writeln("    }");
		writeln("    try {");
		writeln("      Effect.SlideDown(id, { duration: 0.5 });");
		writeln("    } catch (e) {");
		// si effects.js n'est pas chargé, par exemple dans last_shutdown.html
		writeln("      document.getElementById(id).style.display='inline';");
		writeln("    }");
		writeln("  } else {");
		writeln("    if (document.getElementById(id + 'Img') != null) {");
		writeln("      document.getElementById(id + 'Img').src='?resource=bullets/plus.png';");
		writeln("    }");
		writeln("    try {");
		writeln("      Effect.SlideUp(id, { duration: 0.5 });");
		writeln("    } catch (e) {");
		// si effects.js n'est pas chargé, par exemple dans last_shutdown.html
		writeln("      document.getElementById(id).style.display='none';");
		writeln("    }");
		writeln("  }");
		writeln("}");
		writeln(SCRIPT_END);
	}

	public void writeMessageIfNotNull(String message, String partToRedirectTo) throws IOException {
		htmlCoreReport.writeMessageIfNotNull(message, partToRedirectTo, null);
	}

	public void writeRequestAndGraphDetail(String graphName) throws IOException {
		writeHtmlHeader(true, false);

		writeln("<div align='center'>");
		htmlCoreReport.writeRefreshAndPeriodLinks(graphName, "graph");
		writeln("</div>");

		new HtmlCounterRequestGraphReport(range, getWriter()).writeRequestAndGraphDetail(collector,
				collectorServer, graphName);

		writeHtmlFooter();
	}

	public void writeRequestUsages(String graphName) throws IOException {
		writeHtmlHeader(true, false);

		writeln("<div align='center'>");
		htmlCoreReport.writeRefreshAndPeriodLinks(graphName, "usages");
		writeln("</div>");

		new HtmlCounterRequestGraphReport(range, getWriter()).writeRequestUsages(collector,
				graphName);

		writeHtmlFooter();
	}

	public void writeSessions(List<SessionInformations> sessionsInformations, String message,
			String sessionsPart) throws IOException {
		assert sessionsInformations != null;
		writeHtmlHeader();
		writeMessageIfNotNull(message, sessionsPart);
		new HtmlSessionInformationsReport(sessionsInformations, getWriter()).toHtml();
		writeHtmlFooter();
	}

	public void writeSessionDetail(String sessionId, SessionInformations sessionInformations)
			throws IOException {
		assert sessionId != null;
		writeHtmlHeader();
		new HtmlSessionInformationsReport(null, getWriter()).writeSessionDetails(sessionId,
				sessionInformations);
		writeHtmlFooter();
	}

	public void writeHotspots(List<SampledMethod> hotspots) throws IOException {
		writeHtmlHeader();
		new HtmlHotspotsReport(hotspots, getWriter()).toHtml();
		writeHtmlFooter();
	}

	public void writeHeapHistogram(HeapHistogram heapHistogram, String message,
			String heapHistoPart) throws IOException {
		assert heapHistogram != null;
		writeHtmlHeader();
		writeMessageIfNotNull(message, heapHistoPart);
		new HtmlHeapHistogramReport(heapHistogram, getWriter()).toHtml();
		writeHtmlFooter();
	}

	public void writeProcesses(List<ProcessInformations> processInformationsList)
			throws IOException {
		assert processInformationsList != null;
		writeHtmlHeader();
		new HtmlProcessInformationsReport(processInformationsList, getWriter()).toHtml();
		writeHtmlFooter();
	}

	public void writeProcesses(Map<String, List<ProcessInformations>> processesByTitle)
			throws IOException {
		assert processesByTitle != null;
		writeHtmlHeader();
		new HtmlProcessInformationsReport(new ArrayList<ProcessInformations>(), getWriter())
				.writeLinks();
		for (final Map.Entry<String, List<ProcessInformations>> entry : processesByTitle
				.entrySet()) {
			final String title = entry.getKey();
			final List<ProcessInformations> processes = entry.getValue();
			writeDirectly("<h3 class='chapterTitle'><img src='?resource=processes.png' alt='"
					+ title + "'/>&nbsp;" + title + "</h3>");

			new HtmlProcessInformationsReport(processes, getWriter()).writeTable();
		}
		writeHtmlFooter();
	}

	public void writeDatabase(DatabaseInformations databaseInformations) throws IOException {
		assert databaseInformations != null;
		writeHtmlHeader();
		new HtmlDatabaseInformationsReport(databaseInformations, getWriter()).toHtml();
		writeHtmlFooter();
	}

	public void writeConnections(List<ConnectionInformations> connectionInformationsList,
			boolean withoutHeaders) throws IOException {
		assert connectionInformationsList != null;
		final HtmlConnectionInformationsReport htmlConnectionInformationsReport = new HtmlConnectionInformationsReport(
				connectionInformationsList, getWriter());
		if (withoutHeaders) {
			// pour affichage dans serveur de collecte
			htmlConnectionInformationsReport.writeConnections();
		} else {
			writeHtmlHeader();
			htmlConnectionInformationsReport.toHtml();
			writeHtmlFooter();
		}
	}

	public void writeJndi(List<JndiBinding> jndiBindings, String path) throws IOException {
		assert jndiBindings != null;
		writeHtmlHeader();
		new HtmlJndiTreeReport(jndiBindings, path, getWriter()).toHtml();
		writeHtmlFooter();
	}

	public void writeMBeans(List<MBeanNode> mbeans) throws IOException {
		assert mbeans != null;
		writeHtmlHeader();
		new HtmlMBeansReport(mbeans, getWriter()).toHtml();
		writeHtmlFooter();
	}

	public void writeMBeans(Map<String, List<MBeanNode>> mbeansByTitle) throws IOException {
		assert mbeansByTitle != null;
		writeHtmlHeader();
		new HtmlMBeansReport(new ArrayList<MBeanNode>(), getWriter()).writeLinks();
		for (final Map.Entry<String, List<MBeanNode>> entry : mbeansByTitle.entrySet()) {
			final String title = entry.getKey();
			final List<MBeanNode> nodes = entry.getValue();
			writeDirectly("<h3 class='chapterTitle'><img src='?resource=mbeans.png' alt='" + title
					+ "'/>&nbsp;" + title + "</h3>");

			new HtmlMBeansReport(nodes, getWriter()).writeTree();
		}
		writeHtmlFooter();
	}

	public void writeCacheWithKeys(String cacheId, CacheInformations cacheInformations,
			String message, String cacheKeyPart, boolean withoutHeaders) throws IOException {
		assert cacheId != null;
		assert cacheInformations != null;
		final HtmlCacheInformationsReport htmlCacheInformationsReport = new HtmlCacheInformationsReport(
				Collections.singletonList(cacheInformations), getWriter());
		if (withoutHeaders) {
			htmlCacheInformationsReport.writeCacheWithKeys(cacheId, withoutHeaders);
		} else {
			writeHtmlHeader();
			htmlCacheInformationsReport.writeCacheWithKeys(cacheId, withoutHeaders);
			writeHtmlFooter();
			writeMessageIfNotNull(message, cacheKeyPart);
		}
	}

	public void writeJCacheWithKeys(String cacheId, JCacheInformations jcacheInformations,
			String message, String cacheKeyPart, boolean withoutHeaders) throws IOException {
		assert cacheId != null;
		assert jcacheInformations != null;
		final HtmlJCacheInformationsReport htmlJCacheInformationsReport = new HtmlJCacheInformationsReport(
				Collections.singletonList(jcacheInformations), getWriter());
		if (withoutHeaders) {
			htmlJCacheInformationsReport.writeJCacheWithKeys(cacheId, withoutHeaders);
		} else {
			writeHtmlHeader();
			htmlJCacheInformationsReport.writeJCacheWithKeys(cacheId, withoutHeaders);
			writeHtmlFooter();
			writeMessageIfNotNull(message, cacheKeyPart);
		}
	}

	public void writeCrashes() throws IOException {
		writeHtmlHeader();
		final List<HsErrPid> hsErrPidList = htmlCoreReport.getHsErrPidList();
		final HtmlHsErrPidReport htmlHsErrPidReport = new HtmlHsErrPidReport(hsErrPidList,
				getWriter());
		htmlHsErrPidReport.toHtml();
		writeHtmlFooter();
	}
}
