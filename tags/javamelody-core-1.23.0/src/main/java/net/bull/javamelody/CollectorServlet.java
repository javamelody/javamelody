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
import static net.bull.javamelody.HttpParameters.HTML_BODY_FORMAT;
import static net.bull.javamelody.HttpParameters.HTML_CONTENT_TYPE;
import static net.bull.javamelody.HttpParameters.JNDI_PART;
import static net.bull.javamelody.HttpParameters.JOB_ID_PARAMETER;
import static net.bull.javamelody.HttpParameters.PART_PARAMETER;
import static net.bull.javamelody.HttpParameters.PATH_PARAMETER;
import static net.bull.javamelody.HttpParameters.POM_XML_PART;
import static net.bull.javamelody.HttpParameters.PROCESSES_PART;
import static net.bull.javamelody.HttpParameters.REQUEST_PARAMETER;
import static net.bull.javamelody.HttpParameters.SESSION_ID_PARAMETER;
import static net.bull.javamelody.HttpParameters.THREAD_ID_PARAMETER;
import static net.bull.javamelody.HttpParameters.WEB_XML_PART;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StreamCorruptedException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * Servlet de collecte utilisée uniquement pour serveur de collecte séparé de l'application monitorée.
 * @author Emeric Vernat
 */
public class CollectorServlet extends HttpServlet {
	private static final String BACK_LINK = "<a href='javascript:history.back()'><img src='?resource=action_back.png' alt='#Retour#'/> #Retour#</a>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;";

	private static final String COOKIE_NAME = "javamelody.application";

	private static final long serialVersionUID = -2070469677921953224L;

	@SuppressWarnings("all")
	private static final Logger LOGGER = Logger.getLogger("javamelody");

	private Pattern allowedAddrPattern;

	@SuppressWarnings("all")
	private final transient HttpCookieManager httpCookieManager = new HttpCookieManager();

	@SuppressWarnings("all")
	private transient CollectorServer collectorServer;

	/** {@inheritDoc} */
	@Override
	public void init(ServletConfig config) throws ServletException {
		super.init(config);
		Parameters.initialize(config.getServletContext());
		if (!Boolean.parseBoolean(Parameters.getParameter(Parameter.LOG))) {
			// si log désactivé dans serveur de collecte,
			// alors pas de log, comme dans webapp
			LOGGER.setLevel(Level.WARN);
		}
		// dans le serveur de collecte, on est sûr que log4j est disponible
		LOGGER.info("initialization of the collector servlet of the monitoring");
		if (Parameters.getParameter(Parameter.ALLOWED_ADDR_PATTERN) != null) {
			allowedAddrPattern = Pattern.compile(Parameters
					.getParameter(Parameter.ALLOWED_ADDR_PATTERN));
		}

		try {
			collectorServer = new CollectorServer();
		} catch (final IOException e) {
			throw new ServletException(e.getMessage(), e);
		}
	}

	/** {@inheritDoc} */
	@Override
	protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws ServletException,
			IOException {
		final long start = System.currentTimeMillis();
		if (isAddressAllowed(req)) {
			resp.sendError(HttpServletResponse.SC_FORBIDDEN, "Forbidden access");
			return;
		}
		final String application = getApplication(req, resp);
		I18N.bindLocale(req.getLocale());
		try {
			if (application == null) {
				writeOnlyAddApplication(resp);
				return;
			}
			if (!collectorServer.isApplicationDataAvailable(application)) {
				resp.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR,
						"Data unavailable for the application " + application);
				return;
			}
			doMonitoring(req, resp, application);
		} finally {
			I18N.unbindLocale();
			if (LOGGER.isDebugEnabled()) {
				LOGGER.debug("monitoring from " + req.getRemoteAddr() + ", request="
						+ req.getRequestURI()
						+ (req.getQueryString() != null ? '?' + req.getQueryString() : "")
						+ ", application=" + application + " in "
						+ (System.currentTimeMillis() - start) + "ms");
			}
		}
	}

	/** {@inheritDoc} */
	@Override
	protected void doPost(HttpServletRequest req, HttpServletResponse resp)
			throws ServletException, IOException {
		if (isAddressAllowed(req)) {
			resp.sendError(HttpServletResponse.SC_FORBIDDEN, "Forbidden access");
			return;
		}
		// post du formulaire d'ajout d'application à monitorer
		final String appName = req.getParameter("appName");
		final String appUrls = req.getParameter("appUrls");
		I18N.bindLocale(req.getLocale());
		try {
			if (appName == null || appUrls == null) {
				writeMessage(req, resp, getApplication(req, resp),
						I18N.getString("donnees_manquantes"));
				return;
			}
			if (!appUrls.startsWith("http://") && !appUrls.startsWith("https://")) {
				writeMessage(req, resp, getApplication(req, resp), I18N.getString("urls_format"));
				return;
			}
			final List<URL> urls = Parameters.parseUrl(appUrls);
			collectorServer.addCollectorApplication(appName, urls);
			LOGGER.info("monitored application added: " + appName);
			LOGGER.info("urls of the monitored application: " + urls);
			showAlertAndRedirectTo(resp, I18N.getFormattedString("application_ajoutee", appName),
					"?application=" + appName);
		} catch (final FileNotFoundException e) {
			final String message = I18N.getString("monitoring_configure");
			LOGGER.warn(message, e);
			writeMessage(req, resp, getApplication(req, resp), message + '\n' + e.toString());
		} catch (final StreamCorruptedException e) {
			final String message = I18N.getFormattedString("reponse_non_comprise", appUrls);
			LOGGER.warn(message, e);
			writeMessage(req, resp, getApplication(req, resp), message + '\n' + e.toString());
		} catch (final Exception e) {
			LOGGER.warn(e.toString(), e);
			writeMessage(req, resp, getApplication(req, resp), e.toString());
		} finally {
			I18N.unbindLocale();
		}
	}

	private void doMonitoring(HttpServletRequest req, HttpServletResponse resp, String application)
			throws IOException {
		try {
			final Collector collector = getCollectorByApplication(application);
			final MonitoringController monitoringController = new MonitoringController(collector,
					collectorServer);
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
					messageForReport = monitoringController.executeActionIfNeeded(req);
					writeMessage(req, resp, application, messageForReport);
				}
				return;
			}

			final String partParameter = req.getParameter(PART_PARAMETER);
			if (partParameter == null) {
				// la récupération de javaInformationsList doit être après forwardActionAndUpdateData
				// pour être à jour
				final List<JavaInformations> javaInformationsList = getJavaInformationsByApplication(application);
				monitoringController.doReport(req, resp, javaInformationsList);
			} else {
				doPart(req, resp, application, monitoringController, partParameter);
			}
		} catch (final RuntimeException e) {
			// catch RuntimeException pour éviter warning exception
			writeMessage(req, resp, application, e.getMessage());
		} catch (final Exception e) {
			writeMessage(req, resp, application, e.getMessage());
		}
	}

	private void doPart(HttpServletRequest req, HttpServletResponse resp, String application,
			MonitoringController monitoringController, String partParameter) throws IOException {
		if (WEB_XML_PART.equalsIgnoreCase(partParameter)) {
			MonitoringController.noCache(resp);
			doProxy(req, resp, application, PART_PARAMETER + '=' + WEB_XML_PART);
		} else if (POM_XML_PART.equalsIgnoreCase(partParameter)) {
			MonitoringController.noCache(resp);
			doProxy(req, resp, application, PART_PARAMETER + '=' + POM_XML_PART);
		} else if (CURRENT_REQUESTS_PART.equalsIgnoreCase(partParameter)) {
			doCurrentRequests(req, resp, application);
		} else if (PROCESSES_PART.equalsIgnoreCase(partParameter)) {
			doProcesses(req, resp, application);
		} else if (JNDI_PART.equalsIgnoreCase(partParameter)) {
			doJndi(req, resp, application);
		} else if (DATABASE_PART.equalsIgnoreCase(partParameter)) {
			doDatabase(req, resp, application);
		} else if (CONNECTIONS_PART.equalsIgnoreCase(partParameter)) {
			doConnections(req, resp, application);
		} else {
			final List<JavaInformations> javaInformationsList = getJavaInformationsByApplication(application);
			monitoringController.doReport(req, resp, javaInformationsList);
		}
	}

	private void doProxy(HttpServletRequest req, HttpServletResponse resp, String application,
			String urlParameter) throws IOException {
		// récupération à la demande du contenu du web.xml de la webapp monitorée
		// (et non celui du serveur de collecte),
		// on prend la 1ère url puisque le contenu de web.xml est censé être le même
		// dans tout l'éventuel cluster
		final URL url = getUrlsByApplication(application).get(0);
		// on récupère le contenu du web.xml sur la webapp et on transfert ce contenu
		final URL webXmlUrl = new URL(url.toString() + '&' + urlParameter);
		new LabradorRetriever(webXmlUrl).copyTo(req, resp);
	}

	private void doCurrentRequests(HttpServletRequest req, HttpServletResponse resp,
			String application) throws IOException {
		final PrintWriter writer = createWriterFromOutputStream(resp);
		final HtmlReport htmlReport = createHtmlReport(req, resp, writer, application);
		htmlReport.writeHtmlHeader();
		writer.write("<div class='noPrint'>");
		I18N.writelnTo(BACK_LINK, writer);
		writer.write("<a href='?part=");
		writer.write(CURRENT_REQUESTS_PART);
		writer.write("'>");
		I18N.writelnTo("<img src='?resource=action_refresh.png' alt='#Actualiser#'/> #Actualiser#",
				writer);
		writer.write("</a></div>");
		final String title = I18N.getString("Requetes_en_cours");
		for (final URL url : getUrlsByApplication(application)) {
			final String htmlTitle = "<h3><img width='24' height='24' src='?resource=hourglass.png' alt='"
					+ title + "'/>" + title + " (" + getHostAndPort(url) + ")</h3>";
			writer.write(htmlTitle);
			writer.flush(); // flush du buffer de writer, sinon le copyTo passera avant dans l'outputStream
			final URL currentRequestsUrl = new URL(url.toString()
					.replace(TransportFormat.SERIALIZED.getCode(), HTML_BODY_FORMAT)
					.replace(TransportFormat.XML.getCode(), HTML_BODY_FORMAT)
					+ '&' + PART_PARAMETER + '=' + CURRENT_REQUESTS_PART);
			new LabradorRetriever(currentRequestsUrl).copyTo(req, resp);
		}
		htmlReport.writeHtmlFooter();
		writer.close();
	}

	private void doProcesses(HttpServletRequest req, HttpServletResponse resp, String application)
			throws IOException {
		final PrintWriter writer = createWriterFromOutputStream(resp);
		final HtmlReport htmlReport = createHtmlReport(req, resp, writer, application);
		htmlReport.writeHtmlHeader();
		writer.write("<div class='noPrint'>");
		I18N.writelnTo(BACK_LINK, writer);
		writer.write("<a href='?part=");
		writer.write(PROCESSES_PART);
		writer.write("'>");
		I18N.writelnTo("<img src='?resource=action_refresh.png' alt='#Actualiser#'/> #Actualiser#",
				writer);
		writer.write("</a></div>");
		final String title = I18N.getString("Processus");
		for (final URL url : getUrlsByApplication(application)) {
			final String htmlTitle = "<h3><img width='24' height='24' src='?resource=threads.png' alt='"
					+ title + "'/>&nbsp;" + title + " (" + getHostAndPort(url) + ")</h3>";
			writer.write(htmlTitle);
			writer.flush();
			final URL processesUrl = new URL(url.toString() + '&' + PART_PARAMETER + '='
					+ PROCESSES_PART);
			final List<ProcessInformations> processes = new LabradorRetriever(processesUrl).call();
			new HtmlProcessInformationsReport(processes, writer).writeTable();
		}
		htmlReport.writeHtmlFooter();
		writer.close();
	}

	private void doJndi(HttpServletRequest req, HttpServletResponse resp, String application)
			throws IOException {
		final URL url = getUrlsByApplication(application).get(0);
		final String pathParameter = req.getParameter(PATH_PARAMETER) != null ? req
				.getParameter(PATH_PARAMETER) : "";
		final String jndiParameters = '&' + PART_PARAMETER + '=' + JNDI_PART + '&' + PATH_PARAMETER
				+ '=' + pathParameter;
		final String htmlBodyFormat = "html";
		final URL currentRequestsUrl = new URL(url.toString()
				.replace(TransportFormat.SERIALIZED.getCode(), htmlBodyFormat)
				.replace(TransportFormat.XML.getCode(), htmlBodyFormat)
				+ jndiParameters);
		new LabradorRetriever(currentRequestsUrl).copyTo(req, resp);
	}

	private void doDatabase(HttpServletRequest req, HttpServletResponse resp, String application)
			throws IOException {
		final int requestIndex = DatabaseInformations.parseRequestIndex(req
				.getParameter(REQUEST_PARAMETER));
		final PrintWriter writer = createWriterFromOutputStream(resp);
		final HtmlReport htmlReport = createHtmlReport(req, resp, writer, application);
		final URL url = getUrlsByApplication(application).get(0);
		final URL processesUrl = new URL(url.toString() + '&' + PART_PARAMETER + '='
				+ DATABASE_PART + '&' + REQUEST_PARAMETER + '=' + requestIndex);
		final DatabaseInformations databaseInformations = new LabradorRetriever(processesUrl)
				.call();
		htmlReport.writeDatabase(databaseInformations);
		writer.close();
	}

	private void doConnections(HttpServletRequest req, HttpServletResponse resp, String application)
			throws IOException {
		final PrintWriter writer = createWriterFromOutputStream(resp);
		final HtmlReport htmlReport = createHtmlReport(req, resp, writer, application);
		htmlReport.writeHtmlHeader();
		writer.write("<div class='noPrint'>");
		I18N.writelnTo(BACK_LINK, writer);
		writer.write("<a href='?part=");
		writer.write(CONNECTIONS_PART);
		writer.write("'>");
		I18N.writelnTo("<img src='?resource=action_refresh.png' alt='#Actualiser#'/> #Actualiser#",
				writer);
		writer.write("</a></div><br/>");
		writer.write(I18N.getString("connexions_intro"));
		final String title = I18N.getString("Connexions_jdbc_ouvertes");
		for (final URL url : getUrlsByApplication(application)) {
			final String htmlTitle = "<h3><img width='24' height='24' src='?resource=db.png' alt='"
					+ title + "'/>&nbsp;" + title + " (" + getHostAndPort(url) + ")</h3>";
			writer.write(htmlTitle);
			writer.flush(); // flush du buffer de writer, sinon le copyTo passera avant dans l'outputStream
			final URL connectionsUrl = new URL(url.toString()
					.replace(TransportFormat.SERIALIZED.getCode(), HTML_BODY_FORMAT)
					.replace(TransportFormat.XML.getCode(), HTML_BODY_FORMAT)
					+ '&' + PART_PARAMETER + '=' + CONNECTIONS_PART);
			new LabradorRetriever(connectionsUrl).copyTo(req, resp);
		}
		htmlReport.writeHtmlFooter();
		writer.close();
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

	private void writeMessage(HttpServletRequest req, HttpServletResponse resp, String application,
			String message) throws IOException {
		MonitoringController.noCache(resp);
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

	private Collector getCollectorByApplication(String application) {
		return collectorServer.getCollectorByApplication(application);
	}

	private List<JavaInformations> getJavaInformationsByApplication(String application) {
		return collectorServer.getJavaInformationsByApplication(application);
	}

	private static PrintWriter createWriterFromOutputStream(HttpServletResponse httpResponse)
			throws IOException {
		MonitoringController.noCache(httpResponse);
		httpResponse.setContentType(HTML_CONTENT_TYPE);
		return new PrintWriter(MonitoringController.getWriter(httpResponse));
	}

	private static void writeOnlyAddApplication(HttpServletResponse resp) throws IOException {
		MonitoringController.noCache(resp);
		resp.setContentType(HTML_CONTENT_TYPE);
		final PrintWriter writer = createWriterFromOutputStream(resp);
		writer.write("<html><head><title>Monitoring</title></head><body>");
		HtmlReport.writeAddAndRemoveApplicationLinks(null, writer);
		writer.write("</body></html>");
		writer.close();
	}

	private static void showAlertAndRedirectTo(HttpServletResponse resp, String message,
			String redirectTo) throws IOException {
		resp.setContentType(HTML_CONTENT_TYPE);
		final PrintWriter writer = createWriterFromOutputStream(resp);
		writer.write("<script type='text/javascript'>alert('");
		writer.write(I18N.javascriptEncode(message));
		writer.write("');location.href='");
		writer.write(redirectTo);
		writer.write("';</script>");
		writer.close();
	}

	private boolean isAddressAllowed(HttpServletRequest req) {
		return allowedAddrPattern != null
				&& !allowedAddrPattern.matcher(req.getRemoteAddr()).matches();
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

	private String getApplication(HttpServletRequest req, HttpServletResponse resp) {
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

	private static List<URL> getUrlsByApplication(String application) throws IOException {
		return CollectorServer.getUrlsByApplication(application);
	}

	/** {@inheritDoc} */
	@Override
	public void destroy() {
		LOGGER.info("collector servlet stopping");
		if (collectorServer != null) {
			collectorServer.stop();
		}
		Collector.stopJRobin();
		LOGGER.info("collector servlet stopped");
		super.destroy();
	}
}
