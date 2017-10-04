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

import java.io.BufferedInputStream;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Serializable;
import java.net.URLDecoder;
import java.util.Collections;
import java.util.List;

import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import net.bull.javamelody.Parameter;
import net.bull.javamelody.SessionListener;
import net.bull.javamelody.internal.common.HttpParameter;
import net.bull.javamelody.internal.common.HttpPart;
import net.bull.javamelody.internal.common.I18N;
import net.bull.javamelody.internal.common.LOG;
import net.bull.javamelody.internal.common.Parameters;
import net.bull.javamelody.internal.model.Action;
import net.bull.javamelody.internal.model.Collector;
import net.bull.javamelody.internal.model.CollectorServer;
import net.bull.javamelody.internal.model.JRobin;
import net.bull.javamelody.internal.model.JavaInformations;
import net.bull.javamelody.internal.model.MBeans;
import net.bull.javamelody.internal.model.MavenArtifact;
import net.bull.javamelody.internal.model.Range;
import net.bull.javamelody.internal.model.TransportFormat;

/**
 * Contrôleur au sens MVC de l'ihm de monitoring.
 * @author Emeric Vernat
 */
public class MonitoringController {
	static {
		boolean webXmlExists = false;
		boolean pomXmlExists = false;
		try {
			final InputStream webXmlAsStream = getWebXmlAsStream();
			if (webXmlAsStream != null) {
				webXmlAsStream.close();
				webXmlExists = true;
			}
			final InputStream pomXmlAsStream = MavenArtifact.getWebappPomXmlAsStream();
			if (pomXmlAsStream != null) {
				pomXmlAsStream.close();
				pomXmlExists = true;
			}
		} catch (final IOException e) {
			LOG.warn(e.toString(), e);
		}
		JavaInformations.setWebXmlExistsAndPomXmlExists(webXmlExists, pomXmlExists);
	}

	private static final boolean GZIP_COMPRESSION_DISABLED = Parameter.GZIP_COMPRESSION_DISABLED
			.getValueAsBoolean();
	private static final boolean CSRF_PROTECTION_ENABLED = Parameter.CSRF_PROTECTION_ENABLED
			.getValueAsBoolean();

	private final HttpCookieManager httpCookieManager = new HttpCookieManager();
	private final Collector collector;
	private final CollectorServer collectorServer;
	private String messageForReport;
	private String anchorNameForRedirect;

	public MonitoringController(Collector collector, CollectorServer collectorServer) {
		super();
		assert collector != null;
		this.collector = collector;
		this.collectorServer = collectorServer;
	}

	public String executeActionIfNeeded(HttpServletRequest httpRequest) throws IOException {
		assert httpRequest != null;
		final String actionParameter = HttpParameter.ACTION.getParameterFrom(httpRequest);
		if (actionParameter != null) {
			if (CSRF_PROTECTION_ENABLED) {
				checkCsrfToken(httpRequest);
			}
			try {
				// langue préférée du navigateur, getLocale ne peut être null
				I18N.bindLocale(httpRequest.getLocale());
				// par sécurité
				final Action action = Action.valueOfIgnoreCase(actionParameter);
				if (action != Action.CLEAR_COUNTER && action != Action.MAIL_TEST) {
					Action.checkSystemActionsEnabled();
				}
				final HttpSession currentSession = httpRequest.getSession(false);
				final String counterName = HttpParameter.COUNTER.getParameterFrom(httpRequest);
				final String sessionId = HttpParameter.SESSION_ID.getParameterFrom(httpRequest);
				final String threadId = HttpParameter.THREAD_ID.getParameterFrom(httpRequest);
				final String jobId = HttpParameter.JOB_ID.getParameterFrom(httpRequest);
				final String cacheId = HttpParameter.CACHE_ID.getParameterFrom(httpRequest);
				final String cacheKey = HttpParameter.CACHE_KEY.getParameterFrom(httpRequest);
				messageForReport = action.execute(collector, collectorServer, currentSession,
						counterName, sessionId, threadId, jobId, cacheId, cacheKey);
				if (collector.getCounterByName(counterName) != null) {
					// on ne veut pas d'injection de faux counterName dans l'ancre
					anchorNameForRedirect = action.getContextName(counterName);
				} else {
					anchorNameForRedirect = action.getContextName(null);
				}
				return messageForReport;
			} finally {
				I18N.unbindLocale();
			}
		}
		return null;
	}

	public static void checkCsrfToken(HttpServletRequest httpRequest) {
		final String token = HttpParameter.TOKEN.getParameterFrom(httpRequest);
		if (token == null) {
			throw new IllegalArgumentException("csrf token missing");
		}
		final HttpSession session = httpRequest.getSession(false);
		if (session == null
				|| !token.equals(session.getAttribute(SessionListener.CSRF_TOKEN_SESSION_NAME))) {
			throw new IllegalArgumentException("invalid token parameter");
		}
	}

	public void doActionIfNeededAndReport(HttpServletRequest httpRequest,
			HttpServletResponse httpResponse, ServletContext servletContext)
			throws IOException, ServletException {
		executeActionIfNeeded(httpRequest);

		// javaInformations doit être réinstanciée et doit être après executeActionIfNeeded
		// pour avoir des informations à jour
		final JavaInformations javaInformations;
		if (MonitoringController.isJavaInformationsNeeded(httpRequest)) {
			javaInformations = new JavaInformations(servletContext, true);
		} else {
			javaInformations = null;
		}

		doReport(httpRequest, httpResponse, Collections.singletonList(javaInformations));
	}

	public void doReport(HttpServletRequest httpRequest, HttpServletResponse httpResponse,
			List<JavaInformations> javaInformationsList) throws IOException, ServletException {
		assert httpRequest != null;
		assert httpResponse != null;
		assert javaInformationsList != null;

		final String resource = HttpParameter.RESOURCE.getParameterFrom(httpRequest);
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
			// session http s'il y en a une
			SessionListener.bindSession(httpRequest.getSession(false));

			final String part = HttpParameter.PART.getParameterFrom(httpRequest);
			final String graph = HttpParameter.GRAPH.getParameterFrom(httpRequest);
			if (part == null && graph != null) {
				final Range range = httpCookieManager.getRange(httpRequest, httpResponse);
				doGraph(httpRequest, httpResponse, range, graph);
			} else if (HttpPart.WEB_XML.isPart(httpRequest)) {
				doWebXml(httpResponse);
			} else if (HttpPart.POM_XML.isPart(httpRequest)) {
				doPomXml(httpResponse);
			} else if (HttpPart.JNLP.isPart(httpRequest)) {
				final Range range = httpCookieManager.getRange(httpRequest, httpResponse);
				doJnlp(httpRequest, httpResponse, range);
			} else if (HttpParameter.REPORT.getParameterFrom(httpRequest) != null) {
				final String reportName = URLDecoder
						.decode(HttpParameter.REPORT.getParameterFrom(httpRequest), "UTF-8");
				doCustomReport(httpRequest, httpResponse, reportName);
			} else {
				doReportCore(httpRequest, httpResponse, javaInformationsList);
			}
		} finally {
			I18N.unbindLocale();
			SessionListener.unbindSession();
		}
	}

	private void doReportCore(HttpServletRequest httpRequest, HttpServletResponse httpResponse,
			List<JavaInformations> javaInformationsList) throws IOException {
		final String format = HttpParameter.FORMAT.getParameterFrom(httpRequest);
		if (HttpPart.LAST_VALUE.isPart(httpRequest)
				&& !TransportFormat.isATransportFormat(format)) {
			doLastValue(httpResponse, HttpParameter.GRAPH.getParameterFrom(httpRequest));
		} else if (HttpParameter.JMX_VALUE.getParameterFrom(httpRequest) != null
				&& !TransportFormat.isATransportFormat(format)) {
			// par sécurité
			Action.checkSystemActionsEnabled();
			doJmxValue(httpResponse, HttpParameter.JMX_VALUE.getParameterFrom(httpRequest));
		} else if (format == null || "html".equalsIgnoreCase(format)
				|| HtmlController.HTML_BODY_FORMAT.equalsIgnoreCase(format)) {
			doCompressedHtml(httpRequest, httpResponse, javaInformationsList);
		} else if ("pdf".equalsIgnoreCase(format)) {
			final PdfController pdfController = new PdfController(collector, collectorServer);
			pdfController.doPdf(httpRequest, httpResponse, javaInformationsList);
		} else if ("prometheus".equalsIgnoreCase(format)) {
			httpResponse.setContentType("text/plain; version=0.0.4;charset=UTF-8");
			final PrometheusController prometheusController = new PrometheusController(
					javaInformationsList, collector, httpResponse.getWriter());
			prometheusController.report();
		} else {
			doCompressedSerializable(httpRequest, httpResponse, javaInformationsList);
		}
	}

	public static void noCache(HttpServletResponse httpResponse) {
		httpResponse.addHeader("Cache-Control", "no-cache");
		httpResponse.addHeader("Pragma", "no-cache");
		httpResponse.addHeader("Expires", "-1");
	}

	public void addPdfContentTypeAndDisposition(HttpServletRequest httpRequest,
			HttpServletResponse httpResponse) {
		// méthode utilisée dans le monitoring Jenkins
		new PdfController(collector, collectorServer).addPdfContentTypeAndDisposition(httpRequest,
				httpResponse);
	}

	private void doCompressedHtml(HttpServletRequest httpRequest, HttpServletResponse httpResponse,
			List<JavaInformations> javaInformationsList) throws IOException {
		if (CSRF_PROTECTION_ENABLED && SessionListener.getCurrentSession() == null) {
			SessionListener.bindSession(httpRequest.getSession());
		}
		final HtmlController htmlController = new HtmlController(collector, collectorServer,
				messageForReport, anchorNameForRedirect);
		// on teste CompressionServletResponseWrapper car il peut déjà être mis dans le serveur de collecte
		// par CollectorServlet.doCompressedPart
		if (isCompressionSupported(httpRequest)
				&& !(httpResponse instanceof CompressionServletResponseWrapper)
				// this checks if it is the Jenkins plugin
				// (another filter may already compress the stream, in which case we must not compress a second time,
				// in particular for org.kohsuke.stapler.compression.CompressionFilter
				// from https://github.com/stapler/stapler in Jenkins v1.470+ (issue JENKINS-14050)
				&& !GZIP_COMPRESSION_DISABLED) {
			// comme la page html peut être volumineuse avec toutes les requêtes sql et http
			// on compresse le flux de réponse en gzip à partir de 4 Ko
			// (à moins que la compression http ne soit pas supportée
			// comme par ex s'il y a un proxy squid qui ne supporte que http 1.0)

			final CompressionServletResponseWrapper wrappedResponse = new CompressionServletResponseWrapper(
					httpResponse, 4096);
			try {
				htmlController.doHtml(httpRequest, wrappedResponse, javaInformationsList);
			} finally {
				wrappedResponse.finishResponse();
			}
		} else {
			htmlController.doHtml(httpRequest, httpResponse, javaInformationsList);
		}
	}

	public void writeHtmlToLastShutdownFile() {
		new HtmlController(collector, collectorServer, messageForReport, anchorNameForRedirect)
				.writeHtmlToLastShutdownFile();
	}

	static BufferedWriter getWriter(HttpServletResponse httpResponse) throws IOException {
		return HtmlController.getWriter(httpResponse);
	}

	private void doCompressedSerializable(HttpServletRequest httpRequest,
			HttpServletResponse httpResponse, List<JavaInformations> javaInformationsList)
			throws IOException {
		final String part = HttpParameter.PART.getParameterFrom(httpRequest);
		if (HtmlController.isLocalCollectNeeded(part)
				&& HttpParameter.PERIOD.getParameterFrom(httpRequest) != null) {
			// pour l'ihm swing, on fait une collecte, pour que les courbes
			// et les compteurs par jour soit à jour avec les dernières requêtes
			collector.collectLocalContextWithoutErrors();
		}
		Serializable serializable;
		try {
			final SerializableController serializableController = new SerializableController(
					collector);
			serializable = serializableController.createSerializable(httpRequest,
					javaInformationsList, messageForReport);
		} catch (final Throwable t) { // NOPMD
			serializable = t;
		}
		doCompressedSerializable(httpRequest, httpResponse, serializable);
	}

	public void doCompressedSerializable(HttpServletRequest httpRequest,
			HttpServletResponse httpResponse, Serializable serializable) throws IOException {
		// note: normalement la compression est supportée ici car s'il s'agit du serveur de collecte,
		// LabradorRetriever appelle connection.setRequestProperty("Accept-Encoding", "gzip");
		// et on teste CompressionServletResponseWrapper car il peut déjà être mis dans le serveur de collecte
		// par CollectorServlet.doCompressedPart
		final SerializableController serializableController = new SerializableController(collector);
		if (isCompressionSupported(httpRequest)
				&& !(httpResponse instanceof CompressionServletResponseWrapper)
				&& !GZIP_COMPRESSION_DISABLED) {
			// comme les données peuvent être volumineuses avec toutes les requêtes sql et http
			// et les threads on compresse le flux de réponse en gzip à partir de 50 Ko
			// (à moins que la compression http ne soit pas supportée
			// comme par ex s'il y a un proxy squid qui ne supporte que http 1.0)
			final CompressionServletResponseWrapper wrappedResponse = new CompressionServletResponseWrapper(
					httpResponse, 50 * 1024);
			try {
				serializableController.doSerializable(httpRequest, wrappedResponse, serializable);
			} finally {
				wrappedResponse.finishResponse();
			}
		} else {
			serializableController.doSerializable(httpRequest, httpResponse, serializable);
		}
	}

	public static void doResource(HttpServletResponse httpResponse, String resource)
			throws IOException {
		// on enlève tout ".." dans le paramètre par sécurité
		final String localResource = Parameters.getResourcePath(resource.replace("..", ""));
		final InputStream resourceAsStream = MonitoringController.class
				.getResourceAsStream(localResource);
		if (resourceAsStream == null) {
			httpResponse.sendError(HttpServletResponse.SC_NOT_FOUND, "Resource not found");
			return;
		}
		try {
			addHeadersForResource(httpResponse, localResource);

			final OutputStream out = httpResponse.getOutputStream();
			final InputStream in = new BufferedInputStream(resourceAsStream);
			try {
				TransportFormat.pump(in, out);
			} finally {
				in.close();
			}
		} finally {
			resourceAsStream.close();
		}
	}

	public static void addHeadersForResource(HttpServletResponse httpResponse, String resource) {
		httpResponse.addHeader("Cache-Control", "max-age=3600"); // cache navigateur 1h
		// un contentType est nécessaire sinon la css n'est pas prise en compte
		// sous firefox sur un serveur distant
		if (resource.endsWith(".css")) {
			httpResponse.setContentType("text/css");
		} else {
			final String mimeType = Parameters.getServletContext().getMimeType(resource);
			// mimeType peut être null, cf issue 69
			if (mimeType != null) {
				httpResponse.setContentType(mimeType);
			}
		}
	}

	private void doGraph(HttpServletRequest httpRequest, HttpServletResponse httpResponse,
			Range range, String graphName) throws IOException {
		final int width = Math
				.min(Integer.parseInt(HttpParameter.WIDTH.getParameterFrom(httpRequest)), 1600);
		final int height = Math
				.min(Integer.parseInt(HttpParameter.HEIGHT.getParameterFrom(httpRequest)), 1600);
		final String max = HttpParameter.MAX.getParameterFrom(httpRequest);
		final boolean maxHidden = max != null && !Boolean.parseBoolean(max);
		final JRobin jrobin = collector.getJRobin(graphName);
		if (jrobin != null) {
			final byte[] img = jrobin.graph(range, width, height, maxHidden);
			// png comme indiqué dans la classe jrobin
			httpResponse.setContentType("image/png");
			httpResponse.setContentLength(img.length);
			final String fileName = graphName + ".png";
			// encoding des CRLF pour http://en.wikipedia.org/wiki/HTTP_response_splitting
			httpResponse.addHeader("Content-Disposition",
					"inline;filename=" + fileName.replace('\n', '_').replace('\r', '_'));
			httpResponse.getOutputStream().write(img);
			httpResponse.flushBuffer();
		}
	}

	// part=lastValue&graph=x,y,z sera utilisé par munin notamment
	private void doLastValue(HttpServletResponse httpResponse, String graphName)
			throws IOException {
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

	// jmxValue=x|y|z pourra aussi être utilisé par munin notamment
	private void doJmxValue(HttpServletResponse httpResponse, String jmxValueParameter)
			throws IOException {
		httpResponse.setContentType("text/plain");
		httpResponse.getWriter().write(MBeans.getConvertedAttributes(jmxValueParameter));
		httpResponse.flushBuffer();
	}

	private void doWebXml(HttpServletResponse httpResponse) throws IOException {
		// par sécurité
		Action.checkSystemActionsEnabled();
		final OutputStream out = httpResponse.getOutputStream();
		httpResponse.setContentType("application/xml");
		httpResponse.addHeader("Content-Disposition", "inline;filename=web.xml");
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
		httpResponse.setContentType("application/xml");
		httpResponse.addHeader("Content-Disposition", "inline;filename=pom.xml");
		final InputStream in = MavenArtifact.getWebappPomXmlAsStream();
		if (in != null) {
			try {
				TransportFormat.pump(in, out);
			} finally {
				in.close();
			}
		}
	}

	private static InputStream getWebXmlAsStream() {
		final InputStream webXml = Parameters.getServletContext()
				.getResourceAsStream("/WEB-INF/web.xml");
		if (webXml == null) {
			return null;
		}
		return new BufferedInputStream(webXml);
	}

	private void doJnlp(HttpServletRequest httpRequest, HttpServletResponse httpResponse,
			Range range) throws IOException {
		httpResponse.setContentType("application/x-java-jnlp-file");
		final String codebase = httpRequest.getRequestURL().toString();
		final String cookies = httpCookieManager.getCookiesAsString(httpRequest);

		new JnlpPage(collector, collectorServer, codebase, cookies, range, httpResponse.getWriter())
				.toJnlp();
	}

	private static void doCustomReport(HttpServletRequest httpRequest,
			HttpServletResponse httpResponse, String reportName)
			throws ServletException, IOException {
		final String customReportPath = Parameters.getParameterValueByName(reportName);
		if (!customReportPath.isEmpty() && customReportPath.charAt(0) == '/'
				&& Parameters.getServletContext().getRequestDispatcher(customReportPath) != null) {
			Parameters.getServletContext().getRequestDispatcher(customReportPath)
					.forward(httpRequest, httpResponse);
		} else {
			httpResponse.sendRedirect(customReportPath);
		}
	}

	static boolean isCompressionSupported(HttpServletRequest httpRequest) {
		// est-ce que le navigateur déclare accepter la compression gzip ?
		boolean supportCompression = false;
		final List<String> acceptEncodings = Collections
				.list(httpRequest.getHeaders("Accept-Encoding"));
		for (final String name : acceptEncodings) {
			if (name.contains("gzip")) {
				supportCompression = true;
				break;
			}
		}
		return supportCompression;
	}

	public static boolean isJavaInformationsNeeded(HttpServletRequest httpRequest) {
		if (HttpParameter.RESOURCE.getParameterFrom(httpRequest) == null
				&& HttpParameter.GRAPH.getParameterFrom(httpRequest) == null) {
			final String part = HttpParameter.PART.getParameterFrom(httpRequest);
			return part == null || HttpPart.CURRENT_REQUESTS.getName().equals(part)
					|| HttpPart.DEFAULT_WITH_CURRENT_REQUESTS.getName().equals(part)
					|| HttpPart.JVM.getName().equals(part)
					|| HttpPart.THREADS.getName().equals(part)
					|| HttpPart.THREADS_DUMP.getName().equals(part);
		}
		return false;
	}
}
