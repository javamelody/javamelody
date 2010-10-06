/*
 * Copyright 2008-2010 by Emeric Vernat
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
import java.util.List;

import javax.naming.InitialContext;
import javax.naming.NamingException;

import net.bull.javamelody.HtmlCounterReport.HtmlCounterRequestGraphReport;

/**
 * Rapport html.
 * @author Emeric Vernat
 */
class HtmlReport {
	private static final String SCRIPT_BEGIN = "<script type='text/javascript'>";
	private static final String SCRIPT_END = "</script>";

	private final Collector collector;
	private final Range range;
	private final Writer writer;
	private final HtmlCoreReport htmlCoreReport;

	HtmlReport(Collector collector, CollectorServer collectorServer,
			List<JavaInformations> javaInformationsList, Range range, Writer writer) {
		super();
		assert collector != null;
		assert javaInformationsList != null && !javaInformationsList.isEmpty();
		assert range != null;
		assert writer != null;

		this.collector = collector;
		this.range = range;
		this.writer = writer;
		this.htmlCoreReport = new HtmlCoreReport(collector, collectorServer, javaInformationsList,
				range, writer);
	}

	HtmlReport(Collector collector, CollectorServer collectorServer,
			List<JavaInformations> javaInformationsList, Period period, Writer writer) {
		this(collector, collectorServer, javaInformationsList, period.getRange(), writer);
	}

	void toHtml(String message) throws IOException {
		writeHtmlHeader();
		htmlCoreReport.toHtml(message);
		writeHtmlFooter();
	}

	void writeLastShutdown() throws IOException {
		writeHtmlHeader(false, true);
		htmlCoreReport.toHtml(null);
		writeHtmlFooter();
	}

	static void writeAddAndRemoveApplicationLinks(String currentApplication, Writer writer)
			throws IOException {
		HtmlCoreReport.writeAddAndRemoveApplicationLinks(currentApplication, writer);
	}

	void writeAllCurrentRequestsAsPart(boolean withoutHeaders) throws IOException {
		if (withoutHeaders) {
			// affichage pour serveur de collecte
			htmlCoreReport.writeAllCurrentRequestsAsPart(false);
		} else {
			writeHtmlHeader();
			htmlCoreReport.writeAllCurrentRequestsAsPart(true);
			writeHtmlFooter();
		}
	}

	void writeAllThreadsAsPart() throws IOException {
		writeHtmlHeader();
		htmlCoreReport.writeAllThreadsAsPart();
		writeHtmlFooter();
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
		writer.write("<html><head><title>"
				+ I18N.getFormattedString("Monitoring_sur", collector.getApplication())
				+ "</title>");
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
			writer.write(out.toString());
			writeln("</style>");
		} else {
			writeln("<link rel='stylesheet' href='?resource=monitoring.css' type='text/css'/>");
		}
		writeln("<link type='image/png' rel='shortcut icon' href='?resource=systemmonitor.png' />");
		writeln("<script type='text/javascript' src='?resource=resizable_tables.js'></script>");
		writeln("<script type='text/javascript' src='?resource=sorttable.js'></script>");
		if (includeSlider) {
			writeln("<script type='text/javascript' src='?resource=prototype.js'></script>");
			writeln("<script type='text/javascript' src='?resource=slider.js'></script>");
		}
		writeJavaScript();
		writeln("</head><body>");
	}

	void writeHtmlFooter() throws IOException {
		final String analyticsId = Parameters.getParameter(Parameter.ANALYTICS_ID);
		if (analyticsId != null) {
			writer.write(SCRIPT_BEGIN);
			writer.write("var gaJsHost = (('https:' == document.location.protocol) ? 'https://ssl.' : 'http://www.');\n");
			writer.write("document.write(unescape(\"%3Cscript src='\" + gaJsHost + \"google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E\"));\n");
			writer.write(SCRIPT_END);
			writer.write(SCRIPT_BEGIN);
			writer.write(" try{\n");
			writer.write("var pageTracker = _gat._getTracker('" + analyticsId + "');\n");
			writer.write("pageTracker._trackPageview();\n");
			writer.write("} catch(err) {}\n");
			writer.write(SCRIPT_END);
			writer.write('\n');
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
		writeln("    document.getElementById(id).style.display='inline';");
		writeln("  } else {");
		writeln("    if (document.getElementById(id + 'Img') != null) {");
		writeln("      document.getElementById(id + 'Img').src='?resource=bullets/plus.png';");
		writeln("    }");
		writeln("    document.getElementById(id).style.display='none';");
		writeln("  }");
		writeln("}");
		writeln(SCRIPT_END);
	}

	void writeMessageIfNotNull(String message, String partToRedirectTo) throws IOException {
		htmlCoreReport.writeMessageIfNotNull(message, partToRedirectTo);
	}

	void writeRequestAndGraphDetail(String graphName) throws IOException {
		writeHtmlHeader(true, false);

		writeln("<div align='center'>");
		htmlCoreReport.writeRefreshAndPeriodLinks(graphName, "graph");
		writeln("</div>");

		new HtmlCounterRequestGraphReport(range, writer).writeRequestAndGraphDetail(collector,
				graphName);

		writeHtmlFooter();
	}

	void writeRequestUsages(String graphName) throws IOException {
		writeHtmlHeader(true, false);

		writeln("<div align='center'>");
		htmlCoreReport.writeRefreshAndPeriodLinks(graphName, "usages");
		writeln("</div>");

		new HtmlCounterRequestGraphReport(range, writer).writeRequestUsages(collector, graphName);

		writeHtmlFooter();
	}

	void writeSessions(List<SessionInformations> sessionsInformations, String message,
			String sessionsPart) throws IOException {
		assert sessionsInformations != null;
		writeHtmlHeader();
		writeMessageIfNotNull(message, sessionsPart);
		new HtmlSessionInformationsReport(writer).toHtml(sessionsInformations);
		writeHtmlFooter();
	}

	void writeSessionDetail(String sessionId, SessionInformations sessionInformations)
			throws IOException {
		assert sessionId != null;
		writeHtmlHeader();
		new HtmlSessionInformationsReport(writer).writeSessionDetails(sessionId,
				sessionInformations);
		writeHtmlFooter();
	}

	void writeHeapHistogram(HeapHistogram heapHistogram, String message, String heapHistoPart)
			throws IOException {
		assert heapHistogram != null;
		writeHtmlHeader();
		writeMessageIfNotNull(message, heapHistoPart);
		new HtmlHeapHistogramReport(writer).toHtml(heapHistogram);
		writeHtmlFooter();
	}

	void writeProcesses(List<ProcessInformations> processInformationsList) throws IOException {
		assert processInformationsList != null;
		writeHtmlHeader();
		new HtmlProcessInformationsReport(processInformationsList, writer).toHtml();
		writeHtmlFooter();
	}

	void writeDatabase(DatabaseInformations databaseInformations) throws IOException {
		assert databaseInformations != null;
		writeHtmlHeader();
		new HtmlDatabaseInformationsReport(databaseInformations, writer).toHtml();
		writeHtmlFooter();
	}

	void writeConnections(List<ConnectionInformations> connectionInformationsList,
			boolean withoutHeaders) throws IOException {
		assert connectionInformationsList != null;
		final HtmlConnectionInformationsReport htmlConnectionInformationsReport = new HtmlConnectionInformationsReport(
				connectionInformationsList, writer);
		if (withoutHeaders) {
			// pour affichage dans serveur de collecte
			htmlConnectionInformationsReport.writeConnections();
		} else {
			writeHtmlHeader();
			htmlConnectionInformationsReport.toHtml();
			writeHtmlFooter();
		}
	}

	void writeJndi(String path) throws IOException, NamingException {
		writeHtmlHeader();
		new HtmlJndiTreeReport(new InitialContext(), path, writer).toHtml();
		writeHtmlFooter();
	}

	private void writeln(String html) throws IOException {
		I18N.writelnTo(html, writer);
	}
}
