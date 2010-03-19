/*
 * Copyright 2008-2009 by Emeric Vernat, Bull
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

import static net.bull.javamelody.HttpParameters.ACTION_PARAMETER;
import static net.bull.javamelody.HttpParameters.COLLECTOR_PARAMETER;
import static net.bull.javamelody.HttpParameters.CONTENT_DISPOSITION;
import static net.bull.javamelody.HttpParameters.COUNTER_PARAMETER;
import static net.bull.javamelody.HttpParameters.CURRENT_REQUESTS_PART;
import static net.bull.javamelody.HttpParameters.DATABASE_PART;
import static net.bull.javamelody.HttpParameters.FORMAT_PARAMETER;
import static net.bull.javamelody.HttpParameters.GRAPH_PARAMETER;
import static net.bull.javamelody.HttpParameters.GRAPH_PART;
import static net.bull.javamelody.HttpParameters.HEAP_HISTO_PART;
import static net.bull.javamelody.HttpParameters.HEIGHT_PARAMETER;
import static net.bull.javamelody.HttpParameters.HTML_CHARSET;
import static net.bull.javamelody.HttpParameters.HTML_CONTENT_TYPE;
import static net.bull.javamelody.HttpParameters.JOB_ID_PARAMETER;
import static net.bull.javamelody.HttpParameters.LAST_VALUE_PART;
import static net.bull.javamelody.HttpParameters.PART_PARAMETER;
import static net.bull.javamelody.HttpParameters.POM_XML_PART;
import static net.bull.javamelody.HttpParameters.PROCESSES_PART;
import static net.bull.javamelody.HttpParameters.REQUEST_PARAMETER;
import static net.bull.javamelody.HttpParameters.RESOURCE_PARAMETER;
import static net.bull.javamelody.HttpParameters.SESSIONS_PART;
import static net.bull.javamelody.HttpParameters.SESSION_ID_PARAMETER;
import static net.bull.javamelody.HttpParameters.THREAD_ID_PARAMETER;
import static net.bull.javamelody.HttpParameters.USAGES_PART;
import static net.bull.javamelody.HttpParameters.WEB_XML_PART;
import static net.bull.javamelody.HttpParameters.WIDTH_PARAMETER;

import java.io.BufferedInputStream;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * Contrôleur au sens MVC de l'ihm de monitoring.
 * @author Emeric Vernat
 */
class MonitoringController {
	static {
		boolean webXmlExists = false;
		boolean pomXmlExists = false;
		try {
			final InputStream webXmlAsStream = getWebXmlAsStream();
			if (webXmlAsStream != null) {
				webXmlAsStream.close();
				webXmlExists = true;
			}
			final InputStream pomXmlAsStream = getPomXmlAsStream();
			if (pomXmlAsStream != null) {
				pomXmlAsStream.close();
				pomXmlExists = true;
			}
		} catch (final IOException e) {
			Collector.printStackTrace(e);
		}
		JavaInformations.setWebXmlExistsAndPomXmlExists(webXmlExists, pomXmlExists);
	}
	private final HttpCookieManager httpCookieManager = new HttpCookieManager();
	private final Collector collector;
	private final CollectorServer collectorServer;
	private String messageForReport;

	MonitoringController(Collector collector, CollectorServer collectorServer) {
		super();
		assert collector != null;
		this.collector = collector;
		this.collectorServer = collectorServer;
	}

	boolean isJavaInformationsNeeded(HttpServletRequest httpRequest) {
		return httpRequest.getParameter(RESOURCE_PARAMETER) == null
				&& httpRequest.getParameter(GRAPH_PARAMETER) == null
				&& httpRequest.getParameter(PART_PARAMETER) == null;
	}

	String executeActionIfNeeded(HttpServletRequest httpRequest) throws IOException {
		assert httpRequest != null;
		final String actionParameter = httpRequest.getParameter(ACTION_PARAMETER);
		if (actionParameter != null) {
			try {
				// langue préférée du navigateur, getLocale ne peut être null
				I18N.bindLocale(httpRequest.getLocale());
				// par sécurité
				final Action action = Action.valueOfIgnoreCase(actionParameter);
				if (action != Action.CLEAR_COUNTER) {
					Action.checkSystemActionsEnabled();
				}
				final String counterName = httpRequest.getParameter(COUNTER_PARAMETER);
				final String sessionId = httpRequest.getParameter(SESSION_ID_PARAMETER);
				final String threadId = httpRequest.getParameter(THREAD_ID_PARAMETER);
				final String jobId = httpRequest.getParameter(JOB_ID_PARAMETER);
				messageForReport = action.execute(collector, counterName, sessionId, threadId,
						jobId);
				return messageForReport;
			} finally {
				I18N.unbindLocale();
			}
		}
		return null;
	}

	void doReport(HttpServletRequest httpRequest, HttpServletResponse httpResponse,
			List<JavaInformations> javaInformationsList) throws IOException {
		assert httpRequest != null;
		assert httpResponse != null;
		assert javaInformationsList != null;

		final String resource = httpRequest.getParameter(RESOURCE_PARAMETER);
		if (resource != null) {
			doResource(httpResponse, resource);
			return;
		}

		// dans tous les cas sauf resource,
		// il n'y a pas de cache navigateur (sur la page html, les courbes ou le flux sérialisé)
		noCache(httpResponse);

		try {
			// langue préférée du navigateur, getLocale ne peut être null
			I18N.bindLocale(httpRequest.getLocale());

			doReportCore(httpRequest, httpResponse, javaInformationsList);
		} finally {
			I18N.unbindLocale();
		}
	}

	private void doReportCore(HttpServletRequest httpRequest, HttpServletResponse httpResponse,
			List<JavaInformations> javaInformationsList) throws IOException {
		final Range range = httpCookieManager.getRange(httpRequest, httpResponse);
		final String part = httpRequest.getParameter(PART_PARAMETER);
		final String graph = httpRequest.getParameter(GRAPH_PARAMETER);
		if (part == null && graph != null) {
			doGraph(httpRequest, httpResponse, range, graph);
		} else if (LAST_VALUE_PART.equalsIgnoreCase(part)) {
			doLastValue(httpResponse, graph);
		} else if (WEB_XML_PART.equalsIgnoreCase(part)) {
			doWebXml(httpResponse);
		} else if (POM_XML_PART.equalsIgnoreCase(part)) {
			doPomXml(httpResponse);
		} else {
			final String format = httpRequest.getParameter(FORMAT_PARAMETER);
			if (format == null || "html".equalsIgnoreCase(format)) {
				doCompressedHtml(httpRequest, httpResponse, javaInformationsList, range, part);
			} else if ("pdf".equalsIgnoreCase(format)) {
				doPdf(httpRequest, httpResponse, range, javaInformationsList);
			} else {
				doSerializable(httpRequest, httpResponse, javaInformationsList, format);
			}
		}
	}

	static void noCache(HttpServletResponse httpResponse) {
		httpResponse.addHeader("Cache-Control", "no-cache");
		httpResponse.addHeader("Pragma", "no-cache");
		httpResponse.addHeader("Expires", "-1");
	}

	private void doCompressedHtml(HttpServletRequest httpRequest, HttpServletResponse httpResponse,
			List<JavaInformations> javaInformationsList, Range range, String part)
			throws IOException {
		if (isCompressionSupported(httpRequest)) {
			// comme la page html peut être volumineuse avec toutes les requêtes sql et http
			// on compresse le flux de réponse en gzip (à moins que la compression http
			// ne soit pas supportée comme par ex s'il y a un proxy squid qui ne supporte que http 1.0)
			final CompressionServletResponseWrapper wrappedResponse = new CompressionServletResponseWrapper(
					httpResponse, 4096);
			try {
				doHtml(httpRequest, wrappedResponse, javaInformationsList, range, part);
			} finally {
				wrappedResponse.finishResponse();
			}
		} else {
			doHtml(httpRequest, httpResponse, javaInformationsList, range, part);
		}
	}

	private void doSerializable(HttpServletRequest httpRequest, HttpServletResponse httpResponse,
			List<JavaInformations> javaInformationsList, String format) throws IOException {
		// l'appelant (un serveur d'agrégation par exemple) peut appeler
		// la page monitoring avec un format "serialized" ou "xml" en paramètre
		// pour avoir les données au format sérialisé java ou xml
		final TransportFormat transportFormat = TransportFormat.valueOfIgnoreCase(format);
		final Serializable serializable = createSerializable(httpRequest, javaInformationsList);
		httpResponse.setContentType(transportFormat.getMimeType());
		final String fileName = "JavaMelody_"
				+ collector.getApplication().replace(' ', '_').replace("/", "") + '_'
				+ I18N.getCurrentDate().replace('/', '_') + '.' + transportFormat.getCode();
		httpResponse.addHeader(CONTENT_DISPOSITION, "inline;filename=" + fileName);

		transportFormat.writeSerializableTo(serializable, httpResponse.getOutputStream());

		if ("stop".equalsIgnoreCase(httpRequest.getParameter(COLLECTOR_PARAMETER))) {
			// on a été appelé par un serveur de collecte qui fera l'aggrégation dans le temps,
			// le stockage et les courbes, donc on arrête le timer s'il est démarré
			// et on vide les stats pour que le serveur de collecte ne récupère que les deltas
			collector.stop();
		}
	}

	private Serializable createSerializable(HttpServletRequest httpRequest,
			List<JavaInformations> javaInformationsList) {
		final String part = httpRequest.getParameter(PART_PARAMETER);
		try {
			if (HEAP_HISTO_PART.equalsIgnoreCase(part)) {
				// par sécurité
				Action.checkSystemActionsEnabled();
				return VirtualMachine.createHeapHistogram();
			} else if (SESSIONS_PART.equalsIgnoreCase(part)) {
				// par sécurité
				Action.checkSystemActionsEnabled();
				final String sessionId = httpRequest.getParameter(SESSION_ID_PARAMETER);
				if (sessionId == null) {
					final List<SessionInformations> sessionsInformations = SessionListener
							.getAllSessionsInformations();
					return new ArrayList<SessionInformations>(sessionsInformations);
				}
				return SessionListener.getSessionInformationsBySessionId(sessionId);
			} else if (PROCESSES_PART.equalsIgnoreCase(part)) {
				// par sécurité
				Action.checkSystemActionsEnabled();
				return new ArrayList<ProcessInformations>(ProcessInformations
						.buildProcessInformations());
			} else if (DATABASE_PART.equalsIgnoreCase(part)) {
				// par sécurité
				Action.checkSystemActionsEnabled();
				final int requestIndex;
				if (httpRequest.getParameter(REQUEST_PARAMETER) != null) {
					requestIndex = Integer.parseInt(httpRequest.getParameter(REQUEST_PARAMETER));
				} else {
					requestIndex = 0;
				}
				return new DatabaseInformations(requestIndex);
			}
		} catch (final Exception e) {
			return e;
		}

		return createDefaultSerializable(javaInformationsList);
	}

	private Serializable createDefaultSerializable(List<JavaInformations> javaInformationsList) {
		final List<Counter> counters = collector.getCounters();
		final List<Serializable> serialized = new ArrayList<Serializable>(counters.size()
				+ javaInformationsList.size());
		// on clone les counters avant de les sérialiser pour ne pas avoir de problèmes de concurrences d'accès
		for (final Counter counter : counters) {
			serialized.add(counter.clone());
		}
		serialized.addAll(javaInformationsList);
		if (messageForReport != null) {
			serialized.add(messageForReport);
		}
		return (Serializable) serialized;
	}

	private void doHtml(HttpServletRequest httpRequest, HttpServletResponse httpResponse,
			List<JavaInformations> javaInformationsList, Range range, String part)
			throws IOException {
		if (collectorServer == null
				&& (part == null || CURRENT_REQUESTS_PART.equalsIgnoreCase(part) || GRAPH_PART
						.equalsIgnoreCase(part))) {
			// avant de faire l'affichage on fait une collecte, pour que les courbes
			// et les compteurs par jour soit à jour avec les dernières requêtes
			collector.collectLocalContextWithoutErrors();
		}

		// simple appel de monitoring sans format
		httpResponse.setContentType(HTML_CONTENT_TYPE);
		final BufferedWriter writer = getWriter(httpResponse);
		try {
			final HtmlReport htmlReport = new HtmlReport(collector, collectorServer,
					javaInformationsList, range, writer);
			if (part == null) {
				htmlReport.toHtml(messageForReport);
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
		} else if (SESSIONS_PART.equalsIgnoreCase(part)) {
			final String sessionId = httpRequest.getParameter(SESSION_ID_PARAMETER);
			doSessions(sessionId, htmlReport);
		} else if (CURRENT_REQUESTS_PART.equalsIgnoreCase(part)) {
			doCurrentRequests(htmlReport);
		} else if (HEAP_HISTO_PART.equalsIgnoreCase(part)) {
			doHeapHisto(htmlReport);
		} else if (PROCESSES_PART.equalsIgnoreCase(part)) {
			doProcesses(htmlReport);
		} else if (DATABASE_PART.equalsIgnoreCase(part)) {
			final int requestIndex;
			if (httpRequest.getParameter(REQUEST_PARAMETER) != null) {
				requestIndex = Integer.parseInt(httpRequest.getParameter(REQUEST_PARAMETER));
			} else {
				requestIndex = 0;
			}
			doDatabase(htmlReport, requestIndex);
		} else {
			throw new IllegalArgumentException(part);
		}
	}

	private void doSessions(String sessionId, HtmlReport htmlReport) throws IOException {
		// par sécurité
		Action.checkSystemActionsEnabled();
		final List<SessionInformations> sessionsInformations;
		if (collectorServer == null) {
			if (sessionId == null) {
				sessionsInformations = SessionListener.getAllSessionsInformations();
			} else {
				sessionsInformations = Collections.singletonList(SessionListener
						.getSessionInformationsBySessionId(sessionId));
			}
		} else {
			sessionsInformations = collectorServer.collectSessionInformations(collector
					.getApplication(), sessionId);
		}
		if (sessionId == null || sessionsInformations.isEmpty()) {
			htmlReport.writeSessions(sessionsInformations, messageForReport, SESSIONS_PART);
		} else {
			final SessionInformations sessionInformation = sessionsInformations.get(0);
			htmlReport.writeSessionDetail(sessionId, sessionInformation);
		}
	}

	private void doCurrentRequests(HtmlReport htmlReport) throws IOException {
		if (collectorServer != null) {
			// la partie html des requêtes html n'est utile que depuis une application monitorée
			// (suite à l'appel depuis le serveur de collecte qui lui n'a pas les données requises)
			throw new IllegalStateException();
		}
		htmlReport.writeCurrentRequests(JavaInformations.buildThreadInformationsList(), true, null);
	}

	private void doHeapHisto(HtmlReport htmlReport) throws IOException {
		// par sécurité
		Action.checkSystemActionsEnabled();
		final HeapHistogram heapHistogram;
		try {
			if (collectorServer == null) {
				heapHistogram = VirtualMachine.createHeapHistogram();
			} else {
				heapHistogram = collectorServer.collectHeapHistogram(collector.getApplication());
			}
		} catch (final Exception e) {
			Collector.printStackTrace(e);
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
			Collector.printStackTrace(e);
			htmlReport.writeMessageIfNotNull(String.valueOf(e.getMessage()), null);
		}
	}

	private void doDatabase(HtmlReport htmlReport, int requestIndex) throws IOException {
		// par sécurité
		Action.checkSystemActionsEnabled();
		try {
			htmlReport.writeDatabase(new DatabaseInformations(requestIndex));
		} catch (final Exception e) {
			Collector.printStackTrace(e);
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
				final JavaInformations javaInformations = new JavaInformations(Parameters
						.getServletContext(), true);
				// on pourrait faire I18N.bindLocale(Locale.getDefault()), mais cela se fera tout seul
				final HtmlReport htmlReport = new HtmlReport(collector, collectorServer,
						Collections.singletonList(javaInformations), Period.JOUR, writer);
				htmlReport.toHtml(null);
			} finally {
				writer.close();
			}
		} catch (final IOException e) {
			Collector.printStackTrace(e);
		}
	}

	private void doPdf(HttpServletRequest httpRequest, HttpServletResponse httpResponse,
			Range range, List<JavaInformations> javaInformationsList) throws IOException {
		if (collectorServer == null) {
			// avant de faire l'affichage on fait une collecte,  pour que les courbes
			// et les compteurs par jour soit à jour avec les dernières requêtes
			collector.collectLocalContextWithoutErrors();
		}

		// simple appel de monitoring sans format
		httpResponse.setContentType("application/pdf");
		httpResponse.addHeader(CONTENT_DISPOSITION, encodeFileNameToContentDisposition(httpRequest,
				PdfReport.getFileName(collector.getApplication())));
		try {
			final PdfReport pdfReport = new PdfReport(collector, collectorServer != null,
					javaInformationsList, range, httpResponse.getOutputStream());
			pdfReport.toPdf();
		} finally {
			httpResponse.getOutputStream().flush();
		}
	}

	private void doResource(HttpServletResponse httpResponse, String resource) throws IOException {
		httpResponse.addHeader("Cache-Control", "max-age=3600"); // cache navigateur 1h
		final OutputStream out = httpResponse.getOutputStream();
		// on enlève tout ".." dans le paramètre par sécurité
		final String localResource = Parameters.getResourcePath(resource.replace("..", ""));
		// ce contentType est nécessaire sinon la css n'est pas prise en compte
		// sous firefox sur un serveur distant
		httpResponse.setContentType(Parameters.getServletContext().getMimeType(localResource));
		final InputStream in = new BufferedInputStream(getClass()
				.getResourceAsStream(localResource));
		try {
			TransportFormat.pump(in, out);
		} finally {
			in.close();
		}
	}

	private void doGraph(HttpServletRequest httpRequest, HttpServletResponse httpResponse,
			Range range, String graphName) throws IOException {
		final int width = Math.min(Integer.parseInt(httpRequest.getParameter(WIDTH_PARAMETER)),
				1600);
		final int height = Math.min(Integer.parseInt(httpRequest.getParameter(HEIGHT_PARAMETER)),
				1600);
		final JRobin jrobin = collector.getJRobin(graphName);
		if (jrobin != null) {
			final byte[] img = jrobin.graph(range, width, height);
			// png comme indiqué dans la classe jrobin
			httpResponse.setContentType("image/png");
			httpResponse.setContentLength(img.length);
			final String fileName = graphName + ".png";
			httpResponse.addHeader(CONTENT_DISPOSITION, "inline;filename=" + fileName);
			httpResponse.getOutputStream().write(img);
			httpResponse.flushBuffer();
		}
	}

	// part=lastValue&graph=x,y,z sera utilisé par munin notamment
	private void doLastValue(HttpServletResponse httpResponse, String graphName) throws IOException {
		httpResponse.setContentType("text/plain");
		boolean first = true;
		for (final String graph : graphName.split(",")) {
			final JRobin jrobin = collector.getJRobin(graph);
			final double lastValue;
			if (jrobin == null) {
				lastValue = -1;
			} else {
				lastValue = jrobin.getLastValue();
			}
			if (first) {
				first = false;
			} else {
				httpResponse.getWriter().write(",");
			}
			httpResponse.getWriter().write(String.valueOf(lastValue));
		}
		httpResponse.flushBuffer();
	}

	private void doWebXml(HttpServletResponse httpResponse) throws IOException {
		// par sécurité
		Action.checkSystemActionsEnabled();
		final OutputStream out = httpResponse.getOutputStream();
		httpResponse.setContentType(Parameters.getServletContext().getMimeType("/WEB-INF/web.xml"));
		httpResponse.addHeader(CONTENT_DISPOSITION, "inline;filename=web.xml");
		final InputStream in = getWebXmlAsStream();
		if (in != null) {
			try {
				TransportFormat.pump(in, out);
			} finally {
				in.close();
			}
		}
	}

	private void doPomXml(HttpServletResponse httpResponse) throws IOException {
		// par sécurité
		Action.checkSystemActionsEnabled();
		final OutputStream out = httpResponse.getOutputStream();
		httpResponse.setContentType(Parameters.getServletContext().getMimeType("/WEB-INF/web.xml"));
		httpResponse.addHeader(CONTENT_DISPOSITION, "inline;filename=pom.xml");
		final InputStream in = getPomXmlAsStream();
		if (in != null) {
			try {
				TransportFormat.pump(in, out);
			} finally {
				in.close();
			}
		}
	}

	private static InputStream getWebXmlAsStream() {
		final InputStream webXml = Parameters.getServletContext().getResourceAsStream(
				"/WEB-INF/web.xml");
		if (webXml == null) {
			return null;
		}
		return new BufferedInputStream(webXml);
	}

	@SuppressWarnings("unchecked")
	private static InputStream getPomXmlAsStream() {
		final Set mavenDir = Parameters.getServletContext().getResourcePaths("/META-INF/maven/");
		if (mavenDir == null || mavenDir.isEmpty()) {
			return null;
		}
		final Set groupDir = Parameters.getServletContext().getResourcePaths(
				(String) mavenDir.iterator().next());
		if (groupDir == null || groupDir.isEmpty()) {
			return null;
		}
		final InputStream pomXml = Parameters.getServletContext().getResourceAsStream(
				groupDir.iterator().next() + "pom.xml");
		if (pomXml == null) {
			return null;
		}
		return new BufferedInputStream(pomXml);
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
		sb.append("attachment;filename*=\"");
		char c;
		for (int i = 0; i < length; i++) {
			c = fileName.charAt(i);
			if (c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c >= '0' && c <= '9') {
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

	private static boolean isCompressionSupported(HttpServletRequest httpRequest) {
		// est-ce que le navigateur déclare accepter la compression gzip ?
		boolean supportCompression = false;
		@SuppressWarnings("unchecked")
		final List<String> acceptEncodings = Collections.list(httpRequest
				.getHeaders("Accept-Encoding"));
		for (final String name : acceptEncodings) {
			if (name.contains("gzip")) {
				supportCompression = true;
				break;
			}
		}
		return supportCompression;
	}
}
