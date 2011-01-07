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

import static net.bull.javamelody.HttpParameters.ACTION_PARAMETER;
import static net.bull.javamelody.HttpParameters.CONNECTIONS_PART;
import static net.bull.javamelody.HttpParameters.CURRENT_REQUESTS_PART;
import static net.bull.javamelody.HttpParameters.DATABASE_PART;
import static net.bull.javamelody.HttpParameters.FORMAT_PARAMETER;
import static net.bull.javamelody.HttpParameters.HEAP_HISTO_PART;
import static net.bull.javamelody.HttpParameters.HTML_BODY_FORMAT;
import static net.bull.javamelody.HttpParameters.HTML_CONTENT_TYPE;
import static net.bull.javamelody.HttpParameters.JMX_VALUE;
import static net.bull.javamelody.HttpParameters.JNDI_PART;
import static net.bull.javamelody.HttpParameters.JOB_ID_PARAMETER;
import static net.bull.javamelody.HttpParameters.MBEANS_PART;
import static net.bull.javamelody.HttpParameters.PART_PARAMETER;
import static net.bull.javamelody.HttpParameters.PATH_PARAMETER;
import static net.bull.javamelody.HttpParameters.POM_XML_PART;
import static net.bull.javamelody.HttpParameters.PROCESSES_PART;
import static net.bull.javamelody.HttpParameters.REQUEST_PARAMETER;
import static net.bull.javamelody.HttpParameters.SESSIONS_PART;
import static net.bull.javamelody.HttpParameters.SESSION_ID_PARAMETER;
import static net.bull.javamelody.HttpParameters.THREADS_PART;
import static net.bull.javamelody.HttpParameters.THREAD_ID_PARAMETER;
import static net.bull.javamelody.HttpParameters.WEB_XML_PART;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.Serializable;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * Contrôleur au sens MVC de l'ihm de monitoring dans le serveur collecte.
 * @author Emeric Vernat
 */
public class CollectorController {
	private static final String COOKIE_NAME = "javamelody.application";

	private final HttpCookieManager httpCookieManager = new HttpCookieManager();

	private final CollectorServer collectorServer;

	CollectorController(CollectorServer collectorServer) {
		super();
		assert collectorServer != null;
		this.collectorServer = collectorServer;
	}

	void addCollectorApplication(String appName, String appUrls) throws IOException {
		if (!Parameters.getCollectorApplicationsFile().canWrite()) {
			throw new IllegalStateException(
					"applications should be added or removed in the applications.properties file, because the user is not allowed to write: "
							+ Parameters.getCollectorApplicationsFile());
		}
		final List<URL> urls = Parameters.parseUrl(appUrls);
		collectorServer.addCollectorApplication(appName, urls);
	}

	void doMonitoring(HttpServletRequest req, HttpServletResponse resp, String application)
			throws IOException {
		try {
			final String actionParameter = req.getParameter(ACTION_PARAMETER);
			if (actionParameter != null) {
				final String messageForReport;
				if ("remove_application".equalsIgnoreCase(actionParameter)) {
					collectorServer.removeCollectorApplication(application);
					messageForReport = I18N.getFormattedString("application_enlevee", application);
					showAlertAndRedirectTo(resp, messageForReport, "?");
				} else if (Action.valueOfIgnoreCase(actionParameter) != Action.CLEAR_COUNTER) {
					// on forwarde l'action (gc, invalidate session(s) ou heap dump) sur l'application monitorée
					// et on récupère les informations à jour (notamment mémoire et nb de sessions)
					messageForReport = forwardActionAndUpdateData(req, application);
					writeMessage(req, resp, application, messageForReport);
				} else {
					// nécessaire si action clear_counter
					final Collector collector = getCollectorByApplication(application);
					final MonitoringController monitoringController = new MonitoringController(
							collector, collectorServer);
					messageForReport = monitoringController.executeActionIfNeeded(req);
					writeMessage(req, resp, application, messageForReport);
				}
				return;
			}

			doReport(req, resp, application);
		} catch (final RuntimeException e) {
			// catch RuntimeException pour éviter warning exception
			writeMessage(req, resp, application, e.getMessage());
		} catch (final Exception e) {
			writeMessage(req, resp, application, e.getMessage());
		}
	}

	private void doReport(HttpServletRequest req, HttpServletResponse resp, String application)
			throws IOException {
		final Collector collector = getCollectorByApplication(application);
		final MonitoringController monitoringController = new MonitoringController(collector,
				collectorServer);
		final String partParameter = req.getParameter(PART_PARAMETER);
		if (req.getParameter(JMX_VALUE) != null) {
			doJmxValue(req, resp, application, req.getParameter(JMX_VALUE));
		} else if (TransportFormat.isATransportFormat(req.getParameter(FORMAT_PARAMETER))) {
			doCompressedSerializable(req, resp, application, monitoringController);
		} else if (partParameter == null) {
			// la récupération de javaInformationsList doit être après forwardActionAndUpdateData
			// pour être à jour
			final List<JavaInformations> javaInformationsList = getJavaInformationsByApplication(application);
			monitoringController.doReport(req, resp, javaInformationsList);
		} else {
			doCompressedPart(req, resp, application, monitoringController, partParameter);
		}
	}

	private void doCompressedPart(HttpServletRequest httpRequest, HttpServletResponse httpResponse,
			String application, MonitoringController monitoringController, String partParameter)
			throws IOException {
		if (MonitoringController.isCompressionSupported(httpRequest)) {
			// comme la page html peut être volumineuse
			// on compresse le flux de réponse en gzip à partir de 4 Ko
			// (à moins que la compression http ne soit pas supportée
			// comme par ex s'il y a un proxy squid qui ne supporte que http 1.0)
			final CompressionServletResponseWrapper wrappedResponse = new CompressionServletResponseWrapper(
					httpResponse, 4096);
			try {
				doPart(httpRequest, wrappedResponse, application, monitoringController,
						partParameter);
			} finally {
				wrappedResponse.finishResponse();
			}
		} else {
			doPart(httpRequest, httpResponse, application, monitoringController, partParameter);
		}
	}

	private void doPart(HttpServletRequest req, HttpServletResponse resp, String application,
			MonitoringController monitoringController, String partParameter) throws IOException {
		if (WEB_XML_PART.equalsIgnoreCase(partParameter)) {
			noCache(resp);
			doProxy(req, resp, application, WEB_XML_PART);
		} else if (POM_XML_PART.equalsIgnoreCase(partParameter)) {
			noCache(resp);
			doProxy(req, resp, application, POM_XML_PART);
		} else if (CURRENT_REQUESTS_PART.equalsIgnoreCase(partParameter)) {
			doMultiHtmlProxy(req, resp, application, CURRENT_REQUESTS_PART, "Requetes_en_cours",
					null, "hourglass.png");
		} else if (MBEANS_PART.equalsIgnoreCase(partParameter)) {
			doMultiHtmlProxy(req, resp, application, MBEANS_PART, "MBeans", null, "mbeans.png");
		} else if (CONNECTIONS_PART.equalsIgnoreCase(partParameter)) {
			doMultiHtmlProxy(req, resp, application, CONNECTIONS_PART, "Connexions_jdbc_ouvertes",
					"connexions_intro", "db.png");
		} else if (PROCESSES_PART.equalsIgnoreCase(partParameter)) {
			doProcesses(req, resp, application);
		} else if (JNDI_PART.equalsIgnoreCase(partParameter)) {
			doJndi(req, resp, application);
		} else {
			final List<JavaInformations> javaInformationsList = getJavaInformationsByApplication(application);
			monitoringController.doReport(req, resp, javaInformationsList);
		}
	}

	private void doProcesses(HttpServletRequest req, HttpServletResponse resp, String application)
			throws IOException {
		final PrintWriter writer = createWriterFromOutputStream(resp);
		final HtmlReport htmlReport = createHtmlReport(req, resp, writer, application);
		htmlReport.writeHtmlHeader();
		writer.write("<div class='noPrint'>");
		I18N.writelnTo(
				"<a href='javascript:history.back()'><img src='?resource=action_back.png' alt='#Retour#'/> #Retour#</a>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",
				writer);
		writer.write("<a href='?part=");
		writer.write(PROCESSES_PART);
		writer.write("'>");
		I18N.writelnTo("<img src='?resource=action_refresh.png' alt='#Actualiser#'/> #Actualiser#",
				writer);
		writer.write("</a></div>");
		final String title = I18N.getString("Processus");
		for (final URL url : getUrlsByApplication(application)) {
			final String htmlTitle = "<h3><img width='24' height='24' src='?resource=processes.png' alt='"
					+ title + "'/>&nbsp;" + title + " (" + getHostAndPort(url) + ")</h3>";
			writer.write(htmlTitle);
			writer.flush();
			final URL proxyUrl = new URL(url.toString() + '&' + PART_PARAMETER + '='
					+ PROCESSES_PART);
			final List<ProcessInformations> processes = new LabradorRetriever(proxyUrl).call();
			new HtmlProcessInformationsReport(processes, writer).writeTable();
		}
		htmlReport.writeHtmlFooter();
		writer.close();
	}

	private void doJndi(HttpServletRequest req, HttpServletResponse resp, String application)
			throws IOException {
		// contrairement aux requêtes en cours ou aux processus, un serveur de l'application suffira
		// car l'arbre JNDI est en général identique dans tout l'éventuel cluster
		final URL url = getUrlsByApplication(application).get(0);
		final String pathParameter = req.getParameter(PATH_PARAMETER) != null ? req
				.getParameter(PATH_PARAMETER) : "";
		final String jndiParameters = '&' + PART_PARAMETER + '=' + JNDI_PART + '&' + PATH_PARAMETER
				+ '=' + pathParameter;
		final String htmlFormat = "html";
		final URL currentRequestsUrl = new URL(url.toString()
				.replace(TransportFormat.SERIALIZED.getCode(), htmlFormat)
				.replace(TransportFormat.XML.getCode(), htmlFormat)
				+ jndiParameters);
		new LabradorRetriever(currentRequestsUrl).copyTo(req, resp);
	}

	private void doJmxValue(HttpServletRequest req, HttpServletResponse resp, String application,
			String jmxValueParameter) throws IOException {
		noCache(resp);
		resp.setContentType("text/plain");
		boolean first = true;
		for (final URL url : getUrlsByApplication(application)) {
			if (first) {
				first = false;
			} else {
				resp.getOutputStream().write('|');
				resp.getOutputStream().write('|');
			}
			final URL proxyUrl = new URL(url.toString() + '&' + JMX_VALUE + '=' + jmxValueParameter);
			new LabradorRetriever(proxyUrl).copyTo(req, resp);
		}
		resp.getOutputStream().close();
	}

	private void doProxy(HttpServletRequest req, HttpServletResponse resp, String application,
			String partParameter) throws IOException {
		// récupération à la demande du contenu du web.xml de la webapp monitorée
		// (et non celui du serveur de collecte),
		// on prend la 1ère url puisque le contenu de web.xml est censé être le même
		// dans tout l'éventuel cluster
		final URL url = getUrlsByApplication(application).get(0);
		// on récupère le contenu du web.xml sur la webapp et on transfert ce contenu
		final URL proxyUrl = new URL(url.toString() + '&' + PART_PARAMETER + '=' + partParameter);
		new LabradorRetriever(proxyUrl).copyTo(req, resp);
	}

	private void doMultiHtmlProxy(HttpServletRequest req, HttpServletResponse resp,
			String application, String partParameter, String titleKey, String introductionKey,
			String iconName) throws IOException {
		final PrintWriter writer = createWriterFromOutputStream(resp);
		final HtmlReport htmlReport = createHtmlReport(req, resp, writer, application);
		htmlReport.writeHtmlHeader();
		writer.write("<div class='noPrint'>");
		I18N.writelnTo(
				"<a href='javascript:history.back()'><img src='?resource=action_back.png' alt='#Retour#'/> #Retour#</a>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",
				writer);
		writer.write("<a href='?part=");
		writer.write(partParameter);
		writer.write("'>");
		I18N.writelnTo("<img src='?resource=action_refresh.png' alt='#Actualiser#'/> #Actualiser#",
				writer);
		writer.write("</a></div>");
		if (introductionKey != null) {
			writer.write("<br/>");
			writer.write(I18N.getString(introductionKey));
		}
		final String title = I18N.getString(titleKey);
		for (final URL url : getUrlsByApplication(application)) {
			final String htmlTitle = "<h3><img width='24' height='24' src='?resource=" + iconName
					+ "' alt='" + title + "'/>&nbsp;" + title + " (" + getHostAndPort(url)
					+ ")</h3>";
			writer.write(htmlTitle);
			writer.flush(); // flush du buffer de writer, sinon le copyTo passera avant dans l'outputStream
			final URL proxyUrl = new URL(url.toString()
					.replace(TransportFormat.SERIALIZED.getCode(), HTML_BODY_FORMAT)
					.replace(TransportFormat.XML.getCode(), HTML_BODY_FORMAT)
					+ '&' + PART_PARAMETER + '=' + partParameter);
			new LabradorRetriever(proxyUrl).copyTo(req, resp);
		}
		htmlReport.writeHtmlFooter();
		writer.close();
	}

	private void doCompressedSerializable(HttpServletRequest httpRequest,
			HttpServletResponse httpResponse, String application,
			MonitoringController monitoringController) throws IOException {
		Serializable serializable;
		try {
			serializable = createSerializable(httpRequest, application, monitoringController);
		} catch (final Exception e) {
			serializable = e;
		}
		monitoringController.doCompressedSerializable(httpRequest, httpResponse, serializable);
	}

	private Serializable createSerializable(HttpServletRequest httpRequest, String application,
			MonitoringController monitoringController) throws Exception { // NOPMD
		final String part = httpRequest.getParameter(PART_PARAMETER);
		if (HEAP_HISTO_PART.equalsIgnoreCase(part)) {
			// par sécurité
			Action.checkSystemActionsEnabled();
			return collectorServer.collectHeapHistogram(application);
		} else if (SESSIONS_PART.equalsIgnoreCase(part)) {
			// par sécurité
			Action.checkSystemActionsEnabled();
			final String sessionId = httpRequest.getParameter(SESSION_ID_PARAMETER);
			return new ArrayList<SessionInformations>(collectorServer.collectSessionInformations(
					application, sessionId));
		} else if (PROCESSES_PART.equalsIgnoreCase(part)) {
			// par sécurité
			Action.checkSystemActionsEnabled();
			return new ArrayList<List<ProcessInformations>>(
					collectorServer.collectProcessInformations(application));
		} else if (DATABASE_PART.equalsIgnoreCase(part)) {
			// par sécurité
			Action.checkSystemActionsEnabled();
			final int requestIndex = DatabaseInformations.parseRequestIndex(httpRequest
					.getParameter(REQUEST_PARAMETER));
			return collectorServer.collectDatabaseInformations(application, requestIndex);
		} else if (CONNECTIONS_PART.equalsIgnoreCase(part)) {
			// par sécurité
			Action.checkSystemActionsEnabled();
			return new ArrayList<List<ConnectionInformations>>(
					collectorServer.collectConnectionInformations(application));
		} else if (THREADS_PART.equalsIgnoreCase(part)) {
			return new ArrayList<List<ThreadInformations>>(
					collectorServer.getThreadInformationsLists(application));
		}

		final List<JavaInformations> javaInformationsList = getJavaInformationsByApplication(application);
		return monitoringController.createDefaultSerializable(javaInformationsList);
	}

	private HtmlReport createHtmlReport(HttpServletRequest req, HttpServletResponse resp,
			PrintWriter writer, String application) {
		final Range range = httpCookieManager.getRange(req, resp);
		final Collector collector = getCollectorByApplication(application);
		final List<JavaInformations> javaInformationsList = getJavaInformationsByApplication(application);
		return new HtmlReport(collector, collectorServer, javaInformationsList, range, writer);
	}

	private static String getHostAndPort(URL url) {
		if (url.getPort() != -1) {
			return url.getHost() + ':' + url.getPort();
		}
		// port est -1 si c'est le port par défaut (80)
		return url.getHost();
	}

	void writeMessage(HttpServletRequest req, HttpServletResponse resp, String application,
			String message) throws IOException {
		noCache(resp);
		final Collector collector = getCollectorByApplication(application);
		final List<JavaInformations> javaInformationsList = getJavaInformationsByApplication(application);
		if (application == null || collector == null || javaInformationsList == null) {
			showAlertAndRedirectTo(resp, message, "?");
		} else {
			final PrintWriter writer = createWriterFromOutputStream(resp);
			final String partParameter = req.getParameter(PART_PARAMETER);
			// la période n'a pas d'importance pour writeMessageIfNotNull
			new HtmlReport(collector, collectorServer, javaInformationsList, Period.TOUT, writer)
					.writeMessageIfNotNull(message, partParameter);
			writer.close();
		}
	}

	private static PrintWriter createWriterFromOutputStream(HttpServletResponse httpResponse)
			throws IOException {
		noCache(httpResponse);
		httpResponse.setContentType(HTML_CONTENT_TYPE);
		return new PrintWriter(MonitoringController.getWriter(httpResponse));
	}

	static void writeOnlyAddApplication(HttpServletResponse resp) throws IOException {
		noCache(resp);
		resp.setContentType(HTML_CONTENT_TYPE);
		final PrintWriter writer = createWriterFromOutputStream(resp);
		writer.write("<html><head><title>Monitoring</title></head><body>");
		HtmlReport.writeAddAndRemoveApplicationLinks(null, writer);
		writer.write("</body></html>");
		writer.close();
	}

	static void showAlertAndRedirectTo(HttpServletResponse resp, String message, String redirectTo)
			throws IOException {
		resp.setContentType(HTML_CONTENT_TYPE);
		final PrintWriter writer = createWriterFromOutputStream(resp);
		writer.write("<script type='text/javascript'>alert('");
		writer.write(I18N.javascriptEncode(message));
		writer.write("');location.href='");
		writer.write(redirectTo);
		writer.write("';</script>");
		writer.close();
	}

	private static void noCache(HttpServletResponse httpResponse) {
		MonitoringController.noCache(httpResponse);
	}

	private String forwardActionAndUpdateData(HttpServletRequest req, String application)
			throws IOException {
		final String actionParameter = req.getParameter(ACTION_PARAMETER);
		final String sessionIdParameter = req.getParameter(SESSION_ID_PARAMETER);
		final String threadIdParameter = req.getParameter(THREAD_ID_PARAMETER);
		final String jobIdParameter = req.getParameter(JOB_ID_PARAMETER);
		final List<URL> urls = getUrlsByApplication(application);
		final List<URL> actionUrls = new ArrayList<URL>(urls.size());
		for (final URL url : urls) {
			final StringBuilder actionUrl = new StringBuilder(url.toString());
			actionUrl.append("&action=").append(actionParameter);
			if (sessionIdParameter != null) {
				actionUrl.append("&sessionId=").append(sessionIdParameter);
			}
			if (threadIdParameter != null) {
				actionUrl.append("&threadId=").append(threadIdParameter);
			}
			if (jobIdParameter != null) {
				actionUrl.append("&jobId=").append(jobIdParameter);
			}
			actionUrls.add(new URL(actionUrl.toString()));
		}
		return collectorServer.collectForApplication(application, actionUrls);
	}

	String getApplication(HttpServletRequest req, HttpServletResponse resp) {
		// on utilise un cookie client pour stocker l'application
		// car la page html est faite pour une seule application sans passer son nom en paramètre des requêtes
		// et pour ne pas perdre l'application choisie entre les reconnexions
		String application = req.getParameter("application");
		if (application == null) {
			// pas de paramètre application dans la requête, on cherche le cookie
			final Cookie cookie = httpCookieManager.getCookieByName(req, COOKIE_NAME);
			if (cookie != null) {
				application = cookie.getValue();
				if (!collectorServer.isApplicationDataAvailable(application)) {
					cookie.setMaxAge(-1);
					resp.addCookie(cookie);
					application = null;
				}
			}
			if (application == null) {
				// pas de cookie, on prend la première application si elle existe
				application = collectorServer.getFirstApplication();
			}
		} else if (collectorServer.isApplicationDataAvailable(application)) {
			// un paramètre application est présent dans la requête: l'utilisateur a choisi une application,
			// donc on fixe le cookie
			httpCookieManager.addCookie(req, resp, COOKIE_NAME, String.valueOf(application));
		}
		return application;
	}

	private Collector getCollectorByApplication(String application) {
		return collectorServer.getCollectorByApplication(application);
	}

	private List<JavaInformations> getJavaInformationsByApplication(String application) {
		return collectorServer.getJavaInformationsByApplication(application);
	}

	private static List<URL> getUrlsByApplication(String application) throws IOException {
		return CollectorServer.getUrlsByApplication(application);
	}
}
