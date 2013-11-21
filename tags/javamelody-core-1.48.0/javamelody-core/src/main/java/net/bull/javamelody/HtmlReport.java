/*
 * Copyright 2008-2012 by Emeric Vernat
 *
 *     This file is part of Java Melody.
 *
 * Java Melody is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Java Melody is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Java Melody.  If not, see <http://www.gnu.org/licenses/>.
 */
package net.bull.javamelody;

import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import net.bull.javamelody.HtmlCounterReport.HtmlCounterRequestGraphReport;
import net.bull.javamelody.SamplingProfiler.SampledMethod;

/**
 * Rapport html.
 * @author Emeric Vernat
 */
class HtmlReport extends HtmlAbstractReport {
	private static final String SCRIPT_BEGIN = "<script type='text/javascript'>";
	private static final String SCRIPT_END = "</script>";

	private final Collector collector;
	private final CollectorServer collectorServer;
	private final List<JavaInformations> javaInformationsList;
	private final Range range;
	private final HtmlCoreReport htmlCoreReport;

	HtmlReport(Collector collector, CollectorServer collectorServer,
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

	HtmlReport(Collector collector, CollectorServer collectorServer,
			List<JavaInformations> javaInformationsList, Period period, Writer writer) {
		this(collector, collectorServer, javaInformationsList, period.getRange(), writer);
	}

	@Override
	void toHtml() throws IOException {
		writeHtmlHeader();
		htmlCoreReport.toHtml();
		writeHtmlFooter();
	}

	void toHtml(String message, String anchorNameForRedirect) throws IOException {
		writeHtmlHeader();
		htmlCoreReport.toHtml(message, anchorNameForRedirect);
		writeHtmlFooter();
	}

	void writeLastShutdown() throws IOException {
		writeHtmlHeader(false, true);
		htmlCoreReport.toHtml(null, null);
		writeHtmlFooter();
	}

	static void writeAddAndRemoveApplicationLinks(String currentApplication, Writer writer)
			throws IOException {
		HtmlCoreReport.writeAddAndRemoveApplicationLinks(currentApplication, writer);
	}

	void writeAllCurrentRequestsAsPart() throws IOException {
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

	void writeAllThreadsAsPart() throws IOException {
		writeHtmlHeader();
		htmlCoreReport.writeAllThreadsAsPart();
		writeHtmlFooter();
	}

	void writeThreadsDump() throws IOException {
		htmlCoreReport.writeThreadsDump();
	}

	void writeCounterSummaryPerClass(String counterName, String requestId) throws IOException {
		writeHtmlHeader();
		htmlCoreReport.writeCounterSummaryPerClass(counterName, requestId);
		writeHtmlFooter();
	}

	void writeHtmlHeader() throws IOException {
		writeHtmlHeader(false, false);
	}

	private void writeHtmlHeader(boolean includeSlider, boolean includeCssInline)
			throws IOException {
		writeln("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">");
		writeDirectly("<html><head><title>"
				+ getFormattedString("Monitoring_sur", collector.getApplication()) + "</title>");
		writeln("");
		if (includeCssInline) {
			writeln("<style type='text/css'>");
			final InputStream in = new BufferedInputStream(getClass().getResourceAsStream(
					Parameters.getResourcePath("monitoring.css")));
			final ByteArrayOutputStream out = new ByteArrayOutputStream();
			try {
				TransportFormat.pump(in, out);
			} finally {
				in.close();
			}
			writeDirectly(out.toString());
			writeln("</style>");
		} else {
			writeln("<link rel='stylesheet' href='?resource=monitoring.css' type='text/css'/>");
		}
		writeln("<link type='image/png' rel='shortcut icon' href='?resource=systemmonitor.png' />");
		writeln("<script type='text/javascript' src='?resource=resizable_tables.js'></script>");
		writeln("<script type='text/javascript' src='?resource=sorttable.js'></script>");
		// prototype.js nécessaire pour effects.js et slider.js
		writeln("<script type='text/javascript' src='?resource=prototype.js'></script>");
		// Effect slidedown/slideup décrit ici http://madrobby.github.com/scriptaculous/effect-slidedown/
		writeln("<script type='text/javascript' src='?resource=effects.js'></script>");
		if (includeSlider) {
			writeln("<script type='text/javascript' src='?resource=slider.js'></script>");
		}
		writeJavaScript();
		writeln("</head><body>");
	}

	void writeHtmlFooter() throws IOException {
		final String analyticsId = Parameters.getParameter(Parameter.ANALYTICS_ID);
		if (analyticsId != null) {
			writeDirectly(SCRIPT_BEGIN);
			writeDirectly("var gaJsHost = (('https:' == document.location.protocol) ? 'https://ssl.' : 'http://www.');\n");
			writeDirectly("document.write(unescape(\"%3Cscript src='\" + gaJsHost + \"google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E\"));\n");
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

	void writeMessageIfNotNull(String message, String partToRedirectTo) throws IOException {
		htmlCoreReport.writeMessageIfNotNull(message, partToRedirectTo, null);
	}

	void writeRequestAndGraphDetail(String graphName) throws IOException {
		writeHtmlHeader(true, false);

		writeln("<div align='center'>");
		htmlCoreReport.writeRefreshAndPeriodLinks(graphName, "graph");
		writeln("</div>");

		new HtmlCounterRequestGraphReport(range, getWriter()).writeRequestAndGraphDetail(collector,
				collectorServer, graphName);

		writeHtmlFooter();
	}

	void writeRequestUsages(String graphName) throws IOException {
		writeHtmlHeader(true, false);

		writeln("<div align='center'>");
		htmlCoreReport.writeRefreshAndPeriodLinks(graphName, "usages");
		writeln("</div>");

		new HtmlCounterRequestGraphReport(range, getWriter()).writeRequestUsages(collector,
				graphName);

		writeHtmlFooter();
	}

	void writeSessions(List<SessionInformations> sessionsInformations, String message,
			String sessionsPart) throws IOException {
		assert sessionsInformations != null;
		writeHtmlHeader();
		writeMessageIfNotNull(message, sessionsPart);
		new HtmlSessionInformationsReport(sessionsInformations, getWriter()).toHtml();
		writeHtmlFooter();
	}

	void writeSessionDetail(String sessionId, SessionInformations sessionInformations)
			throws IOException {
		assert sessionId != null;
		writeHtmlHeader();
		new HtmlSessionInformationsReport(null, getWriter()).writeSessionDetails(sessionId,
				sessionInformations);
		writeHtmlFooter();
	}

	void writeHotspots(List<SampledMethod> hotspots) throws IOException {
		writeHtmlHeader();
		new HtmlHotspotsReport(hotspots, getWriter()).toHtml();
		writeHtmlFooter();
	}

	void writeHeapHistogram(HeapHistogram heapHistogram, String message, String heapHistoPart)
			throws IOException {
		assert heapHistogram != null;
		writeHtmlHeader();
		writeMessageIfNotNull(message, heapHistoPart);
		new HtmlHeapHistogramReport(heapHistogram, getWriter()).toHtml();
		writeHtmlFooter();
	}

	void writeProcesses(List<ProcessInformations> processInformationsList) throws IOException {
		assert processInformationsList != null;
		writeHtmlHeader();
		new HtmlProcessInformationsReport(processInformationsList, getWriter()).toHtml();
		writeHtmlFooter();
	}

	void writeProcesses(Map<String, List<ProcessInformations>> processesByTitle) throws IOException {
		assert processesByTitle != null;
		writeHtmlHeader();
		new HtmlProcessInformationsReport(new ArrayList<ProcessInformations>(), getWriter())
				.writeLinks();
		for (final Map.Entry<String, List<ProcessInformations>> entry : processesByTitle.entrySet()) {
			final String title = entry.getKey();
			final List<ProcessInformations> processes = entry.getValue();
			writeDirectly("<h3><img width='24' height='24' src='?resource=processes.png' alt='"
					+ title + "'/>&nbsp;" + title + "</h3>");

			new HtmlProcessInformationsReport(processes, getWriter()).writeTable();
		}
		writeHtmlFooter();
	}

	void writeDatabase(DatabaseInformations databaseInformations) throws IOException {
		assert databaseInformations != null;
		writeHtmlHeader();
		new HtmlDatabaseInformationsReport(databaseInformations, getWriter()).toHtml();
		writeHtmlFooter();
	}

	void writeConnections(List<ConnectionInformations> connectionInformationsList,
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

	void writeJndi(List<JndiBinding> jndiBindings, String path) throws IOException {
		assert jndiBindings != null;
		writeHtmlHeader();
		new HtmlJndiTreeReport(jndiBindings, path, getWriter()).toHtml();
		writeHtmlFooter();
	}

	void writeMBeans(List<MBeanNode> mbeans) throws IOException {
		assert mbeans != null;
		writeHtmlHeader();
		new HtmlMBeansReport(mbeans, getWriter()).toHtml();
		writeHtmlFooter();
	}

	void writeMBeans(Map<String, List<MBeanNode>> mbeansByTitle) throws IOException {
		assert mbeansByTitle != null;
		writeHtmlHeader();
		new HtmlMBeansReport(new ArrayList<MBeanNode>(), getWriter()).writeLinks();
		for (final Map.Entry<String, List<MBeanNode>> entry : mbeansByTitle.entrySet()) {
			final String title = entry.getKey();
			final List<MBeanNode> nodes = entry.getValue();
			writeDirectly("<h3><img width='24' height='24' src='?resource=mbeans.png' alt='"
					+ title + "'/>&nbsp;" + title + "</h3>");

			new HtmlMBeansReport(nodes, getWriter()).writeTree();
		}
		writeHtmlFooter();
	}
}
