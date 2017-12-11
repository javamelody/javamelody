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

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.Serializable;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.ServletException;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.log4j.Logger;

import net.bull.javamelody.Parameter;
import net.bull.javamelody.internal.common.HttpParameter;
import net.bull.javamelody.internal.common.HttpPart;
import net.bull.javamelody.internal.common.I18N;
import net.bull.javamelody.internal.common.Parameters;
import net.bull.javamelody.internal.model.Action;
import net.bull.javamelody.internal.model.Collector;
import net.bull.javamelody.internal.model.CollectorServer;
import net.bull.javamelody.internal.model.ConnectionInformations;
import net.bull.javamelody.internal.model.Counter;
import net.bull.javamelody.internal.model.CounterRequest;
import net.bull.javamelody.internal.model.CounterRequestAggregation;
import net.bull.javamelody.internal.model.CounterRequestContext;
import net.bull.javamelody.internal.model.DatabaseInformations;
import net.bull.javamelody.internal.model.JavaInformations;
import net.bull.javamelody.internal.model.JndiBinding;
import net.bull.javamelody.internal.model.LabradorRetriever;
import net.bull.javamelody.internal.model.MBeanNode;
import net.bull.javamelody.internal.model.Period;
import net.bull.javamelody.internal.model.ProcessInformations;
import net.bull.javamelody.internal.model.Range;
import net.bull.javamelody.internal.model.RemoteCollector;
import net.bull.javamelody.internal.model.SamplingProfiler.SampledMethod;
import net.bull.javamelody.internal.model.SessionInformations;
import net.bull.javamelody.internal.model.ThreadInformations;
import net.bull.javamelody.internal.model.TransportFormat;
import net.bull.javamelody.internal.web.html.HtmlAbstractReport;
import net.bull.javamelody.internal.web.html.HtmlReport;

/**
 * Contrôleur au sens MVC de l'ihm de monitoring dans le serveur de collecte ({@link CollectorServer}).
 * @author Emeric Vernat
 */
public class CollectorController { // NOPMD
	private static final Logger LOGGER = Logger.getLogger("javamelody");

	private static final String COOKIE_NAME = "javamelody.application";

	private static final boolean CSRF_PROTECTION_ENABLED = Parameter.CSRF_PROTECTION_ENABLED
			.getValueAsBoolean();

	private final HttpCookieManager httpCookieManager = new HttpCookieManager();

	private final CollectorServer collectorServer;

	public CollectorController(CollectorServer collectorServer) {
		super();
		assert collectorServer != null;
		this.collectorServer = collectorServer;
	}

	public void addCollectorApplication(String appName, String appUrls) throws IOException {
		final File file = Parameters.getCollectorApplicationsFile();
		if (file.exists() && !file.canWrite()) {
			throw new IllegalStateException(
					"applications should be added or removed in the applications.properties file, because the user is not allowed to write: "
							+ file);
		}
		final List<URL> urls = Parameters.parseUrl(appUrls);
		collectorServer.addCollectorApplication(appName, urls);
	}

	public void removeCollectorApplicationNodes(String appName, String nodeUrls)
			throws IOException {
		final List<URL> urls = Parameters.parseUrl(nodeUrls);
		collectorServer.removeCollectorApplicationNodes(appName, urls);
	}

	public void doMonitoring(HttpServletRequest req, HttpServletResponse resp, String application)
			throws IOException {
		try {
			final String actionParameter = HttpParameter.ACTION.getParameterFrom(req);
			if (actionParameter != null) {
				if (CSRF_PROTECTION_ENABLED) {
					MonitoringController.checkCsrfToken(req);
				}
				final String messageForReport;
				if ("remove_application".equalsIgnoreCase(actionParameter)) {
					collectorServer.removeCollectorApplication(application);
					LOGGER.info("monitored application removed: " + application);
					messageForReport = I18N.getFormattedString("application_enlevee", application);
					showAlertAndRedirectTo(resp, messageForReport, "?");
					return;
				}

				final Collector collector = getCollectorByApplication(application);
				final MonitoringController monitoringController = new MonitoringController(
						collector, collectorServer);
				final Action action = Action.valueOfIgnoreCase(actionParameter);
				if (action != Action.CLEAR_COUNTER && action != Action.MAIL_TEST
						&& action != Action.PURGE_OBSOLETE_FILES) {
					// on forwarde l'action (gc, invalidate session(s) ou heap dump) sur l'application monitorée
					// et on récupère les informations à jour (notamment mémoire et nb de sessions)
					messageForReport = forwardActionAndUpdateData(req, application);
				} else {
					// nécessaire si action clear_counter
					messageForReport = monitoringController.executeActionIfNeeded(req);
				}

				if (TransportFormat
						.isATransportFormat(HttpParameter.FORMAT.getParameterFrom(req))) {
					final SerializableController serializableController = new SerializableController(
							collector);
					final Range range = serializableController.getRangeForSerializable(req);
					final List<Object> serializable = new ArrayList<Object>();
					final List<JavaInformations> javaInformationsList = getJavaInformationsByApplication(
							application);
					serializable.addAll((List<?>) serializableController.createDefaultSerializable(
							javaInformationsList, range, messageForReport));
					monitoringController.doCompressedSerializable(req, resp,
							(Serializable) serializable);
				} else {
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
			throws IOException, ServletException {
		final Collector collector = getCollectorByApplication(application);
		final MonitoringController monitoringController = new MonitoringController(collector,
				collectorServer);
		final String partParameter = HttpParameter.PART.getParameterFrom(req);
		final String formatParameter = HttpParameter.FORMAT.getParameterFrom(req);
		if (HttpParameter.JMX_VALUE.getParameterFrom(req) != null) {
			doJmxValue(req, resp, application, HttpParameter.JMX_VALUE.getParameterFrom(req));
		} else if (TransportFormat.isATransportFormat(formatParameter)) {
			doCompressedSerializable(req, resp, application, monitoringController);
		} else if (partParameter == null || "pdf".equalsIgnoreCase(formatParameter)) {
			// la récupération de javaInformationsList doit être après forwardActionAndUpdateData
			// pour être à jour
			final List<JavaInformations> javaInformationsList = getJavaInformationsByApplication(
					application);
			monitoringController.doReport(req, resp, javaInformationsList);
		} else {
			doCompressedPart(req, resp, application, monitoringController);
		}
	}

	private void doCompressedPart(HttpServletRequest httpRequest, HttpServletResponse httpResponse,
			String application, MonitoringController monitoringController)
			throws IOException, ServletException {
		if (MonitoringController.isCompressionSupported(httpRequest)) {
			// comme la page html peut être volumineuse
			// on compresse le flux de réponse en gzip à partir de 4 Ko
			// (à moins que la compression http ne soit pas supportée
			// comme par ex s'il y a un proxy squid qui ne supporte que http 1.0)
			final CompressionServletResponseWrapper wrappedResponse = new CompressionServletResponseWrapper(
					httpResponse, 4096);
			try {
				doPart(httpRequest, wrappedResponse, application, monitoringController);
			} finally {
				wrappedResponse.finishResponse();
			}
		} else {
			doPart(httpRequest, httpResponse, application, monitoringController);
		}
	}

	private void doPart(HttpServletRequest req, HttpServletResponse resp, String application,
			MonitoringController monitoringController) throws IOException, ServletException {
		if (HttpPart.WEB_XML.isPart(req) || HttpPart.POM_XML.isPart(req)
				|| HttpPart.DEPENDENCIES.isPart(req) || HttpPart.SPRING_BEANS.isPart(req)) {
			noCache(resp);
			// récupération à la demande du contenu du web.xml de la webapp monitorée
			// (et non celui du serveur de collecte),
			// on prend la 1ère url puisque le contenu de web.xml est censé être le même
			// dans tout l'éventuel cluster
			final URL url = getUrlsByApplication(application).get(0);
			// on récupère le contenu du web.xml sur la webapp et on transfert ce contenu
			doProxy(req, resp, url, HttpParameter.PART.getParameterFrom(req));
		} else if (HttpPart.SOURCE.isPart(req)) {
			noCache(resp);
			final URL url = getUrlsByApplication(application).get(0);
			doProxy(req, resp, url, HttpParameter.PART.toString() + '&' + HttpParameter.CLASS + '='
					+ HttpParameter.CLASS.getParameterFrom(req));
		} else if (HttpPart.CONNECTIONS.isPart(req)) {
			doMultiHtmlProxy(req, resp, application, HttpPart.CONNECTIONS.getName(),
					I18N.getString("Connexions_jdbc_ouvertes"), I18N.getString("connexions_intro"),
					"db.png");
		} else if (HttpPart.CACHE_KEYS.isPart(req)) {
			// note: cache keys may not be serializable, so we do not try to serialize them
			final String cacheId = HttpParameter.CACHE_ID.getParameterFrom(req);
			doMultiHtmlProxy(req, resp, application,
					HttpPart.CACHE_KEYS.toString() + '&' + HttpParameter.CACHE_ID + '=' + cacheId,
					I18N.getFormattedString("Keys_cache", cacheId), null, "caches.png");
		} else {
			final List<JavaInformations> javaInformationsList = getJavaInformationsByApplication(
					application);
			monitoringController.doReport(req, resp, javaInformationsList);
		}
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
			final URL proxyUrl = new URL(
					url.toString().replace(TransportFormat.SERIALIZED.getCode(), "")
							.replace(TransportFormat.XML.getCode(), "") + '&'
							+ HttpParameter.JMX_VALUE + '=' + jmxValueParameter);
			new LabradorRetriever(proxyUrl).copyTo(req, resp);
		}
		resp.getOutputStream().close();
	}

	private void doProxy(HttpServletRequest req, HttpServletResponse resp, URL url,
			String partParameter) throws IOException {
		final URL proxyUrl = new URL(url.toString()
				.replace(TransportFormat.SERIALIZED.getCode(), HtmlController.HTML_BODY_FORMAT)
				.replace(TransportFormat.XML.getCode(), HtmlController.HTML_BODY_FORMAT) + '&'
				+ HttpParameter.PART + '=' + partParameter);
		new LabradorRetriever(proxyUrl).copyTo(req, resp);
	}

	private void doMultiHtmlProxy(HttpServletRequest req, HttpServletResponse resp,
			String application, String partParameter, String title, String introduction,
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
		if (introduction != null) {
			writer.write("<br/>");
			writer.write(I18N.htmlEncode(introduction, false));
		}
		for (final URL url : getUrlsByApplication(application)) {
			final String htmlTitle = "<h3 class='chapterTitle'><img src='?resource=" + iconName
					+ "' alt='" + I18N.urlEncode(title) + "'/>&nbsp;"
					+ I18N.htmlEncode(title, false) + " (" + getHostAndPort(url) + ")</h3>";
			writer.write(htmlTitle);
			writer.flush(); // flush du buffer de writer, sinon le copyTo passera avant dans l'outputStream
			doProxy(req, resp, url, partParameter);
		}
		htmlReport.writeHtmlFooter();
		writer.close();
	}

	private void doCompressedSerializable(HttpServletRequest httpRequest,
			HttpServletResponse httpResponse, String application,
			MonitoringController monitoringController) throws IOException {
		Serializable serializable;
		try {
			serializable = createSerializable(httpRequest, application);
		} catch (final Exception e) {
			serializable = e;
		}
		monitoringController.doCompressedSerializable(httpRequest, httpResponse, serializable);
	}

	private Serializable createSerializable(HttpServletRequest httpRequest, String application)
			throws Exception { // NOPMD
		final Serializable resultForSystemActions = createSerializableForSystemActions(httpRequest,
				application);
		if (resultForSystemActions != null) {
			return resultForSystemActions;
		}

		final Collector collector = getCollectorByApplication(application);
		final SerializableController serializableController = new SerializableController(collector);
		final Range range = serializableController.getRangeForSerializable(httpRequest);
		if (HttpPart.THREADS.isPart(httpRequest)) {
			return new ArrayList<List<ThreadInformations>>(
					collectorServer.getThreadInformationsLists(application));
		} else if (HttpPart.CURRENT_REQUESTS.isPart(httpRequest)) {
			return new LinkedHashMap<JavaInformations, List<CounterRequestContext>>(
					collectorServer.collectCurrentRequests(application));
		} else if (HttpPart.EXPLAIN_PLAN.isPart(httpRequest)) {
			final String sqlRequest = httpRequest.getHeader(HttpParameter.REQUEST.getName());
			return collectorServer.collectSqlRequestExplainPlan(application, sqlRequest);
		} else if (HttpPart.COUNTER_SUMMARY_PER_CLASS.isPart(httpRequest)) {
			final String counterName = HttpParameter.COUNTER.getParameterFrom(httpRequest);
			final String requestId = HttpParameter.GRAPH.getParameterFrom(httpRequest);
			final Counter counter = collector.getRangeCounter(range, counterName);
			final List<CounterRequest> requestList = new CounterRequestAggregation(counter)
					.getRequestsAggregatedOrFilteredByClassName(requestId);
			return new ArrayList<CounterRequest>(requestList);
		} else if (HttpPart.APPLICATIONS.isPart(httpRequest)) {
			// list all applications, with last exceptions if not available,
			// use ?part=applications&format=json for example
			final Map<String, Throwable> applications = new HashMap<String, Throwable>();
			for (final String app : Parameters.getCollectorUrlsByApplications().keySet()) {
				applications.put(app, null);
			}
			applications.putAll(collectorServer.getLastCollectExceptionsByApplication());
			return new HashMap<String, Throwable>(applications);
		} else if (HttpPart.JROBINS.isPart(httpRequest)
				|| HttpPart.OTHER_JROBINS.isPart(httpRequest)) {
			// pour UI Swing
			return serializableController.createSerializable(httpRequest, null, null);
		}

		final List<JavaInformations> javaInformationsList = getJavaInformationsByApplication(
				application);
		return serializableController.createDefaultSerializable(javaInformationsList, range, null);
	}

	// CHECKSTYLE:OFF
	private Serializable createSerializableForSystemActions(HttpServletRequest httpRequest,
			String application) throws IOException {
		// CHECKSTYLE:ON
		if (HttpPart.JVM.isPart(httpRequest)) {
			final List<JavaInformations> javaInformationsList = getJavaInformationsByApplication(
					application);
			return new ArrayList<JavaInformations>(javaInformationsList);
		} else if (HttpPart.HEAP_HISTO.isPart(httpRequest)) {
			// par sécurité
			Action.checkSystemActionsEnabled();
			return collectorServer.collectHeapHistogram(application);
		} else if (HttpPart.SESSIONS.isPart(httpRequest)) {
			// par sécurité
			Action.checkSystemActionsEnabled();
			final String sessionId = HttpParameter.SESSION_ID.getParameterFrom(httpRequest);
			final List<SessionInformations> sessionInformations = collectorServer
					.collectSessionInformations(application, sessionId);
			if (sessionId != null && !sessionInformations.isEmpty()) {
				return sessionInformations.get(0);
			}
			return new ArrayList<SessionInformations>(sessionInformations);
		} else if (HttpPart.HOTSPOTS.isPart(httpRequest)) {
			// par sécurité
			Action.checkSystemActionsEnabled();
			return new ArrayList<SampledMethod>(collectorServer.collectHotspots(application));
		} else if (HttpPart.PROCESSES.isPart(httpRequest)) {
			// par sécurité
			Action.checkSystemActionsEnabled();
			return new LinkedHashMap<String, List<ProcessInformations>>(
					collectorServer.collectProcessInformations(application));
		} else if (HttpPart.JNDI.isPart(httpRequest)) {
			// par sécurité
			Action.checkSystemActionsEnabled();
			final String path = HttpParameter.PATH.getParameterFrom(httpRequest);
			return new ArrayList<JndiBinding>(
					collectorServer.collectJndiBindings(application, path));
		} else if (HttpPart.MBEANS.isPart(httpRequest)) {
			// par sécurité
			Action.checkSystemActionsEnabled();
			return new LinkedHashMap<String, List<MBeanNode>>(
					collectorServer.collectMBeans(application));
		} else if (HttpPart.DATABASE.isPart(httpRequest)) {
			// par sécurité
			Action.checkSystemActionsEnabled();
			final int requestIndex = DatabaseInformations
					.parseRequestIndex(HttpParameter.REQUEST.getParameterFrom(httpRequest));
			return collectorServer.collectDatabaseInformations(application, requestIndex);
		} else if (HttpPart.CONNECTIONS.isPart(httpRequest)) {
			// par sécurité
			Action.checkSystemActionsEnabled();
			return new ArrayList<List<ConnectionInformations>>(
					collectorServer.collectConnectionInformations(application));
		}
		return null;
	}

	private HtmlReport createHtmlReport(HttpServletRequest req, HttpServletResponse resp,
			PrintWriter writer, String application) {
		final Range range = httpCookieManager.getRange(req, resp);
		final Collector collector = getCollectorByApplication(application);
		final List<JavaInformations> javaInformationsList = getJavaInformationsByApplication(
				application);
		return new HtmlReport(collector, collectorServer, javaInformationsList, range, writer);
	}

	private static String getHostAndPort(URL url) {
		return RemoteCollector.getHostAndPort(url);
	}

	public void writeMessage(HttpServletRequest req, HttpServletResponse resp, String application,
			String message) throws IOException {
		final Collector collector = getCollectorByApplication(application);
		final List<JavaInformations> javaInformationsList = getJavaInformationsByApplication(
				application);
		if (application == null || collector == null || javaInformationsList == null) {
			showAlertAndRedirectTo(resp, message, "?");
		} else {
			final PrintWriter writer = createWriterFromOutputStream(resp);
			final String partToRedirectTo;
			if (HttpParameter.CACHE_ID.getParameterFrom(req) == null
					|| HttpParameter.PART.getParameterFrom(req) == null) {
				partToRedirectTo = HttpParameter.PART.getParameterFrom(req);
			} else {
				partToRedirectTo = HttpParameter.PART.getParameterFrom(req) + '&'
						+ HttpParameter.CACHE_ID + '='
						+ HttpParameter.CACHE_ID.getParameterFrom(req);
			}
			// la période n'a pas d'importance pour writeMessageIfNotNull
			new HtmlReport(collector, collectorServer, javaInformationsList, Period.TOUT, writer)
					.writeMessageIfNotNull(message, partToRedirectTo);
			writer.close();
		}
	}

	private static PrintWriter createWriterFromOutputStream(HttpServletResponse httpResponse)
			throws IOException {
		noCache(httpResponse);
		return new PrintWriter(MonitoringController.getWriter(httpResponse));
	}

	public static void writeOnlyAddApplication(HttpServletResponse resp) throws IOException {
		final PrintWriter writer = createWriterFromOutputStream(resp);
		writer.write("<html lang='" + I18N.getCurrentLocale().getLanguage()
				+ "'><head><title>Monitoring</title></head><body>");
		HtmlReport.writeAddAndRemoveApplicationLinks(null, writer);
		writer.write("</body></html>");
		writer.close();
	}

	public static void writeDataUnavailableForApplication(String application,
			HttpServletResponse resp) throws IOException {
		final PrintWriter writer = createWriterFromOutputStream(resp);
		writer.write("<html lang='" + I18N.getCurrentLocale().getLanguage()
				+ "'><head><title>Monitoring</title></head><body>");
		writer.write(
				I18N.htmlEncode(I18N.getFormattedString("data_unavailable", application), false));
		writer.write("<br/><br/>");
		writer.write("<a href='?'><img src='?resource=action_back.png' alt=\""
				+ I18N.getString("Retour") + "\"/> " + I18N.getString("Retour") + "</a>");
		if (Parameters.getCollectorApplicationsFile().canWrite()) {
			writer.write("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
			writer.write("<a href='?action=remove_application&amp;application="
					+ I18N.urlEncode(application) + HtmlAbstractReport.getCsrfTokenUrlPart()
					+ "' ");
			final String messageConfirmation = I18N.getFormattedString("confirm_remove_application",
					application);
			writer.write("onclick=\"javascript:return confirm('"
					+ I18N.javascriptEncode(messageConfirmation) + "');\">");
			final String removeApplicationLabel = I18N.getFormattedString("remove_application",
					application);
			writer.write("<img src='?resource=action_delete.png' alt=\"" + removeApplicationLabel
					+ "\"/> " + removeApplicationLabel + "</a>");
		}
		writer.write("</body></html>");
		writer.close();
	}

	public static void showAlertAndRedirectTo(HttpServletResponse resp, String message,
			String redirectTo) throws IOException {
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
		final String actionParameter = HttpParameter.ACTION.getParameterFrom(req);
		final String sessionIdParameter = HttpParameter.SESSION_ID.getParameterFrom(req);
		final String threadIdParameter = HttpParameter.THREAD_ID.getParameterFrom(req);
		final String jobIdParameter = HttpParameter.JOB_ID.getParameterFrom(req);
		final String cacheIdParameter = HttpParameter.CACHE_ID.getParameterFrom(req);
		final String cacheKeyParameter = HttpParameter.CACHE_KEY.getParameterFrom(req);
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
			if (cacheIdParameter != null) {
				actionUrl.append("&cacheId=").append(cacheIdParameter);
			}
			if (cacheKeyParameter != null) {
				actionUrl.append("&cacheKey=").append(cacheKeyParameter);
			}
			actionUrls.add(new URL(actionUrl.toString()));
		}
		return collectorServer.collectForApplicationForAction(application, actionUrls);
	}

	public String getApplication(HttpServletRequest req, HttpServletResponse resp) {
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
