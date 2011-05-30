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
package net.bull.javamelody; // NOPMD

import static net.bull.javamelody.HttpParameters.CONNECTIONS_PART;
import static net.bull.javamelody.HttpParameters.COUNTER_PARAMETER;
import static net.bull.javamelody.HttpParameters.COUNTER_SUMMARY_PER_CLASS_PART;
import static net.bull.javamelody.HttpParameters.CURRENT_REQUESTS_PART;
import static net.bull.javamelody.HttpParameters.DATABASE_PART;
import static net.bull.javamelody.HttpParameters.FORMAT_PARAMETER;
import static net.bull.javamelody.HttpParameters.GRAPH_PARAMETER;
import static net.bull.javamelody.HttpParameters.GRAPH_PART;
import static net.bull.javamelody.HttpParameters.HEAP_HISTO_PART;
import static net.bull.javamelody.HttpParameters.HTML_BODY_FORMAT;
import static net.bull.javamelody.HttpParameters.HTML_CHARSET;
import static net.bull.javamelody.HttpParameters.HTML_CONTENT_TYPE;
import static net.bull.javamelody.HttpParameters.JNDI_PART;
import static net.bull.javamelody.HttpParameters.MBEANS_PART;
import static net.bull.javamelody.HttpParameters.PART_PARAMETER;
import static net.bull.javamelody.HttpParameters.PATH_PARAMETER;
import static net.bull.javamelody.HttpParameters.PROCESSES_PART;
import static net.bull.javamelody.HttpParameters.REQUEST_PARAMETER;
import static net.bull.javamelody.HttpParameters.SESSIONS_PART;
import static net.bull.javamelody.HttpParameters.SESSION_ID_PARAMETER;
import static net.bull.javamelody.HttpParameters.THREADS_PART;
import static net.bull.javamelody.HttpParameters.USAGES_PART;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.util.Collections;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * Contrôleur au sens MVC de l'ihm de monitoring pour la partie html.
 * @author Emeric Vernat
 */
class HtmlController {
	private final HttpCookieManager httpCookieManager = new HttpCookieManager();
	private final Collector collector;
	private final CollectorServer collectorServer;
	private final String messageForReport;
	private final String anchorNameForRedirect;

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
		final String part = httpRequest.getParameter(PART_PARAMETER);
		if (!isFromCollectorServer()
				&& (part == null || CURRENT_REQUESTS_PART.equalsIgnoreCase(part)
						|| GRAPH_PART.equalsIgnoreCase(part) || COUNTER_SUMMARY_PER_CLASS_PART
						.equalsIgnoreCase(part))) {
			// avant de faire l'affichage on fait une collecte, pour que les courbes
			// et les compteurs par jour soit à jour avec les dernières requêtes
			collector.collectLocalContextWithoutErrors();
		}

		// simple appel de monitoring sans format
		httpResponse.setContentType(HTML_CONTENT_TYPE);
		final BufferedWriter writer = getWriter(httpResponse);
		try {
			final Range range = httpCookieManager.getRange(httpRequest, httpResponse);
			final HtmlReport htmlReport = new HtmlReport(collector, collectorServer,
					javaInformationsList, range, writer);
			if (part == null) {
				htmlReport.toHtml(messageForReport, anchorNameForRedirect);
			} else {
				doHtmlPart(httpRequest, part, htmlReport);
			}
		} finally {
			writer.close();
		}
	}

	static BufferedWriter getWriter(HttpServletResponse httpResponse) throws IOException {
		return new BufferedWriter(new OutputStreamWriter(httpResponse.getOutputStream(),
				HTML_CHARSET));
	}

	private void doHtmlPart(HttpServletRequest httpRequest, String part, HtmlReport htmlReport)
			throws IOException {
		if (GRAPH_PART.equalsIgnoreCase(part)) {
			final String graphName = httpRequest.getParameter(GRAPH_PARAMETER);
			htmlReport.writeRequestAndGraphDetail(graphName);
		} else if (USAGES_PART.equalsIgnoreCase(part)) {
			final String graphName = httpRequest.getParameter(GRAPH_PARAMETER);
			htmlReport.writeRequestUsages(graphName);
		} else if (CURRENT_REQUESTS_PART.equalsIgnoreCase(part)) {
			final boolean withoutHeaders = HTML_BODY_FORMAT.equalsIgnoreCase(httpRequest
					.getParameter(FORMAT_PARAMETER));
			doCurrentRequests(htmlReport, withoutHeaders);
		} else if (THREADS_PART.equalsIgnoreCase(part)) {
			htmlReport.writeAllThreadsAsPart();
		} else if (COUNTER_SUMMARY_PER_CLASS_PART.equalsIgnoreCase(part)) {
			final String counterName = httpRequest.getParameter(COUNTER_PARAMETER);
			final String requestId = httpRequest.getParameter(GRAPH_PARAMETER);
			htmlReport.writeCounterSummaryPerClass(counterName, requestId);
		} else {
			doHtmlPartForSystemActions(httpRequest, part, htmlReport);
		}
	}

	private void doHtmlPartForSystemActions(HttpServletRequest httpRequest, String part,
			HtmlReport htmlReport) throws IOException {
		if (SESSIONS_PART.equalsIgnoreCase(part)) {
			doSessions(htmlReport, httpRequest.getParameter(SESSION_ID_PARAMETER));
		} else if (HEAP_HISTO_PART.equalsIgnoreCase(part)) {
			doHeapHisto(htmlReport);
		} else if (PROCESSES_PART.equalsIgnoreCase(part)) {
			doProcesses(htmlReport);
		} else if (DATABASE_PART.equalsIgnoreCase(part)) {
			final int requestIndex = DatabaseInformations.parseRequestIndex(httpRequest
					.getParameter(REQUEST_PARAMETER));
			doDatabase(htmlReport, requestIndex);
		} else if (CONNECTIONS_PART.equalsIgnoreCase(part)) {
			final boolean withoutHeaders = HTML_BODY_FORMAT.equalsIgnoreCase(httpRequest
					.getParameter(FORMAT_PARAMETER));
			doConnections(htmlReport, withoutHeaders);
		} else if (JNDI_PART.equalsIgnoreCase(part)) {
			doJndi(htmlReport, httpRequest.getParameter(PATH_PARAMETER));
		} else if (MBEANS_PART.equalsIgnoreCase(part)) {
			final boolean withoutHeaders = HTML_BODY_FORMAT.equalsIgnoreCase(httpRequest
					.getParameter(FORMAT_PARAMETER));
			doMBeans(htmlReport, withoutHeaders);
		} else {
			throw new IllegalArgumentException(part);
		}
	}

	private void doSessions(HtmlReport htmlReport, String sessionId) throws IOException {
		// par sécurité
		Action.checkSystemActionsEnabled();
		final List<SessionInformations> sessionsInformations;
		if (!isFromCollectorServer()) {
			if (sessionId == null) {
				sessionsInformations = SessionListener.getAllSessionsInformations();
			} else {
				sessionsInformations = Collections.singletonList(SessionListener
						.getSessionInformationsBySessionId(sessionId));
			}
		} else {
			sessionsInformations = collectorServer.collectSessionInformations(
					collector.getApplication(), sessionId);
		}
		if (sessionId == null || sessionsInformations.isEmpty()) {
			htmlReport.writeSessions(sessionsInformations, messageForReport, SESSIONS_PART);
		} else {
			final SessionInformations sessionInformation = sessionsInformations.get(0);
			htmlReport.writeSessionDetail(sessionId, sessionInformation);
		}
	}

	private void doCurrentRequests(HtmlReport htmlReport, boolean withoutHeaders)
			throws IOException {
		if (isFromCollectorServer()) {
			// le html des requêtes en cours dans une page à part n'est utile que depuis une
			// application monitorée (le serveur de collecte n'a pas les données requises)
			throw new IllegalStateException();
		}
		htmlReport.writeAllCurrentRequestsAsPart(withoutHeaders);
	}

	private void doHeapHisto(HtmlReport htmlReport) throws IOException {
		// par sécurité
		Action.checkSystemActionsEnabled();
		final HeapHistogram heapHistogram;
		try {
			if (!isFromCollectorServer()) {
				heapHistogram = VirtualMachine.createHeapHistogram();
			} else {
				heapHistogram = collectorServer.collectHeapHistogram(collector.getApplication());
			}
		} catch (final Exception e) {
			LOG.warn("heaphisto report failed", e);
			htmlReport.writeMessageIfNotNull(String.valueOf(e.getMessage()), null);
			return;
		}
		htmlReport.writeHeapHistogram(heapHistogram, messageForReport, HEAP_HISTO_PART);
	}

	private void doProcesses(HtmlReport htmlReport) throws IOException {
		// par sécurité
		Action.checkSystemActionsEnabled();
		try {
			htmlReport.writeProcesses(ProcessInformations.buildProcessInformations());
		} catch (final Exception e) {
			LOG.warn("processes report failed", e);
			htmlReport.writeMessageIfNotNull(String.valueOf(e.getMessage()), null);
		}
	}

	private void doDatabase(HtmlReport htmlReport, int index) throws IOException {
		// par sécurité
		Action.checkSystemActionsEnabled();
		try {
			final DatabaseInformations databaseInformations;
			if (!isFromCollectorServer()) {
				databaseInformations = new DatabaseInformations(index);
			} else {
				databaseInformations = collectorServer.collectDatabaseInformations(
						collector.getApplication(), index);
			}
			htmlReport.writeDatabase(databaseInformations);
		} catch (final Exception e) {
			LOG.warn("database report failed", e);
			htmlReport.writeMessageIfNotNull(String.valueOf(e.getMessage()), null);
		}
	}

	private void doConnections(HtmlReport htmlReport, boolean withoutHeaders) throws IOException {
		// par sécurité
		Action.checkSystemActionsEnabled();
		htmlReport.writeConnections(JdbcWrapper.getConnectionInformationsList(), withoutHeaders);
	}

	private void doJndi(HtmlReport htmlReport, String path) throws IOException {
		// par sécurité
		Action.checkSystemActionsEnabled();
		try {
			htmlReport.writeJndi(path);
		} catch (final Exception e) {
			LOG.warn("jndi report failed", e);
			htmlReport.writeMessageIfNotNull(String.valueOf(e.getMessage()), null);
		}
	}

	private void doMBeans(HtmlReport htmlReport, boolean withoutHeaders) throws IOException {
		// par sécurité
		Action.checkSystemActionsEnabled();
		try {
			htmlReport.writeMBeans(withoutHeaders);
		} catch (final Exception e) {
			LOG.warn("mbeans report failed", e);
			htmlReport.writeMessageIfNotNull(String.valueOf(e.getMessage()), null);
		}
	}

	void writeHtmlToLastShutdownFile() {
		try {
			final File dir = Parameters.getStorageDirectory(collector.getApplication());
			if (!dir.mkdirs() && !dir.exists()) {
				throw new IOException("JavaMelody directory can't be created: " + dir.getPath());
			}
			final File lastShutdownFile = new File(dir, "last_shutdown.html");
			final BufferedWriter writer = new BufferedWriter(new FileWriter(lastShutdownFile));
			try {
				final JavaInformations javaInformations = new JavaInformations(
						Parameters.getServletContext(), true);
				// on pourrait faire I18N.bindLocale(Locale.getDefault()), mais cela se fera tout seul
				final HtmlReport htmlReport = new HtmlReport(collector, collectorServer,
						Collections.singletonList(javaInformations), Period.JOUR, writer);
				htmlReport.writeLastShutdown();
			} finally {
				writer.close();
			}
		} catch (final IOException e) {
			LOG.warn("exception while writing the last shutdown report", e);
		}
	}

	private boolean isFromCollectorServer() {
		return collectorServer != null;
	}
}
