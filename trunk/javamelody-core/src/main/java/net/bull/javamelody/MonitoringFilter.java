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

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Timer;
import java.util.TimerTask;
import java.util.regex.Pattern;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

/**
 * Filtre de servlet pour le monitoring.
 * C'est la classe de ce filtre qui doit être déclarée dans le fichier web.xml de la webapp.
 * @author Emeric Vernat
 */
public class MonitoringFilter implements Filter {
	private Collector collector;

	// Ces variables httpCounter et errorCounter conservent un état qui est global au filtre
	// et à l'application (donc thread-safe).
	private Counter httpCounter;
	private Counter errorCounter;

	private boolean monitoringDisabled;
	private boolean logEnabled;
	private boolean contextFactoryEnabled;
	private Pattern urlExcludePattern;
	private Pattern allowedAddrPattern;
	private FilterConfig filterConfig;
	private String monitoringUrl;
	private Timer timer;

	private static final class CollectTimerTask extends TimerTask {
		private final Collector collector;

		CollectTimerTask(Collector collector) {
			super();
			this.collector = collector;
		}

		/** {@inheritDoc} */
		@Override
		public void run() {
			// il ne doit pas y avoir d'erreur dans cette task
			collector.collectLocalContextWithoutErrors();
		}
	}

	/** {@inheritDoc} */
	public void init(FilterConfig config) throws ServletException {
		this.filterConfig = config;
		Parameters.initialize(config);
		monitoringDisabled = Boolean.parseBoolean(Parameters.getParameter(Parameter.DISABLED));
		if (monitoringDisabled) {
			return;
		}
		LOG.debug("JavaMelody filter init started");
		boolean initOk = false;
		this.timer = new Timer("javamelody"
				+ Parameters.getContextPath(config.getServletContext()).replace('/', ' '), true);
		try {
			logSystemInformationsAndParameters();

			initLogs();

			this.contextFactoryEnabled = !monitoringDisabled
					&& Boolean.parseBoolean(Parameters
							.getParameter(Parameter.CONTEXT_FACTORY_ENABLED));
			if (contextFactoryEnabled) {
				MonitoringInitialContextFactory.init();
			}

			// si l'application a utilisé JdbcDriver avant d'initialiser ce filtre
			// (par exemple dans un listener de contexte), on doit récupérer son sqlCounter
			// car il est lié à une connexion jdbc qui est certainement conservée dans un pool
			// (sinon les requêtes sql sur cette connexion ne seront pas monitorées)
			// sqlCounter dans JdbcWrapper peut être alimenté soit par une datasource soit par un driver
			JdbcWrapper.SINGLETON.initServletContext(config.getServletContext());
			if (!Parameters.isNoDatabase()) {
				JdbcWrapper.SINGLETON.rebindDataSources();
			}

			// initialisation du listener de jobs quartz
			if (JobInformations.QUARTZ_AVAILABLE) {
				JobGlobalListener.initJobGlobalListener();
			}

			final List<Counter> counters = initCounters();
			final String application = Parameters.getCurrentApplication();
			this.collector = new Collector(application, counters, timer);

			if (Parameters.getParameter(Parameter.URL_EXCLUDE_PATTERN) != null) {
				// lance une PatternSyntaxException si la syntaxe du pattern est invalide
				urlExcludePattern = Pattern.compile(Parameters
						.getParameter(Parameter.URL_EXCLUDE_PATTERN));
			}
			if (Parameters.getParameter(Parameter.ALLOWED_ADDR_PATTERN) != null) {
				allowedAddrPattern = Pattern.compile(Parameters
						.getParameter(Parameter.ALLOWED_ADDR_PATTERN));
			}

			initCollect();
			LOG.debug("JavaMelody filter init done");
			initOk = true;
		} finally {
			if (!initOk) {
				// si exception dans initialisation, on annule la création du timer
				// (sinon tomcat ne serait pas content)
				timer.cancel();
				LOG.debug("JavaMelody filter init canceled");
			}
		}
	}

	private List<Counter> initCounters() {
		// liaison des compteurs : les contextes par thread du sqlCounter ont pour parent le httpCounter
		final Counter sqlCounter = JdbcWrapper.SINGLETON.getSqlCounter();
		this.httpCounter = new Counter(Counter.HTTP_COUNTER_NAME, "dbweb.png", sqlCounter);
		this.errorCounter = new Counter(Counter.ERROR_COUNTER_NAME, "error.png");
		this.errorCounter.setMaxRequestsCount(250);

		final Counter ejbCounter = MonitoringProxy.getEjbCounter();
		final Counter springCounter = MonitoringProxy.getSpringCounter();
		final Counter guiceCounter = MonitoringProxy.getGuiceCounter();
		final Counter servicesCounter = MonitoringProxy.getServicesCounter();
		final Counter strutsCounter = MonitoringProxy.getStrutsCounter();
		final Counter logCounter = LoggingHandler.getLogCounter();
		final Counter jspCounter = JspWrapper.getJspCounter();
		final List<Counter> counters;
		if (JobInformations.QUARTZ_AVAILABLE) {
			final Counter jobCounter = JobGlobalListener.getJobCounter();
			counters = Arrays.asList(httpCounter, sqlCounter, ejbCounter, springCounter,
					guiceCounter, servicesCounter, strutsCounter, jspCounter, errorCounter,
					logCounter, jobCounter);
		} else {
			counters = Arrays.asList(httpCounter, sqlCounter, ejbCounter, springCounter,
					guiceCounter, servicesCounter, strutsCounter, jspCounter, errorCounter,
					logCounter);
		}

		setRequestTransformPatterns(counters);
		final String displayedCounters = Parameters.getParameter(Parameter.DISPLAYED_COUNTERS);
		// displayedCounters doit être traité avant l'initialisation du collector
		// sinon les dayCounters ne seront pas bons
		if (displayedCounters == null) {
			// par défaut, les compteurs http, sql, error et log sont affichés
			httpCounter.setDisplayed(true);
			sqlCounter.setDisplayed(!Parameters.isNoDatabase());
			errorCounter.setDisplayed(true);
			logCounter.setDisplayed(true);
			ejbCounter.setDisplayed(false);
			springCounter.setDisplayed(false);
			guiceCounter.setDisplayed(false);
			servicesCounter.setDisplayed(false);
			strutsCounter.setDisplayed(false);
			jspCounter.setDisplayed(false);
		} else {
			setDisplayedCounters(counters, displayedCounters);
		}
		LOG.debug("counters initialized");
		return counters;
	}

	private static void setRequestTransformPatterns(List<Counter> counters) {
		for (final Counter counter : counters) {
			// le paramètre pour ce nom de compteur doit exister
			final Parameter parameter = Parameter.valueOfIgnoreCase(counter.getName()
					+ "_TRANSFORM_PATTERN");
			if (Parameters.getParameter(parameter) != null) {
				final Pattern pattern = Pattern.compile(Parameters.getParameter(parameter));
				counter.setRequestTransformPattern(pattern);
			}
		}
	}

	private static void setDisplayedCounters(List<Counter> counters, String displayedCounters) {
		for (final Counter counter : counters) {
			if (counter.isJobCounter()) {
				// le compteur "job" a toujours displayed=true s'il est présent,
				// même s'il n'est pas dans la liste des "displayedCounters"
				counter.setDisplayed(true);
			} else {
				counter.setDisplayed(false);
			}
		}
		for (final String displayedCounter : displayedCounters.split(",")) {
			final String displayedCounterName = displayedCounter.trim();
			boolean found = false;
			for (final Counter counter : counters) {
				if (displayedCounterName.equalsIgnoreCase(counter.getName())) {
					counter.setDisplayed(true);
					found = true;
					break;
				}
			}
			if (!found) {
				throw new IllegalArgumentException("Unknown counter: " + displayedCounterName);
			}
		}
	}

	private void initCollect() {
		try {
			Class.forName("org.jrobin.core.RrdDb");
		} catch (final ClassNotFoundException e) {
			LOG.debug("jrobin classes unavailable: collect of data is disabled");
			// si pas de jar jrobin, alors pas de collecte
			return;
		}

		final int resolutionSeconds = Parameters.getResolutionSeconds();
		final int periodMillis = resolutionSeconds * 1000;
		// on schedule la tâche de fond
		final TimerTask task = new CollectTimerTask(collector);
		timer.schedule(task, periodMillis, periodMillis);
		LOG.debug("collect task scheduled every " + resolutionSeconds + 's');

		// on appelle la collecte pour que les instances jrobin soient définies
		// au cas où un graph de la page de monitoring soit demandé de suite
		collector.collectLocalContextWithoutErrors();
		LOG.debug("first collect of data done");

		if (Parameters.getParameter(Parameter.MAIL_SESSION) != null
				&& Parameters.getParameter(Parameter.ADMIN_EMAILS) != null) {
			MailReport.scheduleReportMailForLocalServer(collector, timer);
			LOG.debug("mail reports scheduled for "
					+ Parameters.getParameter(Parameter.ADMIN_EMAILS));
		}
	}

	private void initLogs() {
		logEnabled = Boolean.parseBoolean(Parameters.getParameter(Parameter.LOG));
		// on branche le handler java.util.logging pour le counter de logs
		LoggingHandler.getSingleton().register();

		if (LOG.LOG4J_ENABLED) {
			// si log4j est disponible on branche aussi l'appender pour le counter de logs
			Log4JAppender.getSingleton().register();
		}

		if (LOG.LOGBACK_ENABLED) {
			// si logback est disponible on branche aussi l'appender pour le counter de logs
			LogbackAppender.getSingleton().register();
		}
		LOG.debug("log listeners initialized");
	}

	private void logSystemInformationsAndParameters() {
		// log les principales informations sur le système et sur les paramètres définis spécifiquement
		LOG.debug("OS: " + System.getProperty("os.name") + ' '
				+ System.getProperty("sun.os.patch.level") + ", " + System.getProperty("os.arch")
				+ '/' + System.getProperty("sun.arch.data.model"));
		LOG.debug("Java: " + System.getProperty("java.runtime.name") + ", "
				+ System.getProperty("java.runtime.version"));
		LOG.debug("Server: " + filterConfig.getServletContext().getServerInfo());
		LOG.debug("Webapp context: " + Parameters.getContextPath(filterConfig.getServletContext()));
		LOG.debug("JavaMelody version: " + Parameters.JAVAMELODY_VERSION);
		LOG.debug("Host: " + Parameters.getHostName() + '@' + Parameters.getHostAddress());
		for (final Parameter parameter : Parameter.values()) {
			final String value = Parameters.getParameter(parameter);
			if (value != null) {
				LOG.debug("parameter defined: " + parameter.getCode() + '=' + value);
			}
		}
	}

	/** {@inheritDoc} */
	public void destroy() {
		if (monitoringDisabled) {
			return;
		}
		try {
			try {
				if (collector != null) {
					new MonitoringController(collector, null).writeHtmlToLastShutdownFile();
				}
			} finally {
				//on rebind les dataSources initiales à la place des proxy
				JdbcWrapper.SINGLETON.stop();
				JdbcDriver.SINGLETON.deregister();

				// on enlève l'appender de logback, log4j et le handler de java.util.logging
				deregisterLogs();

				// on enlève le listener de jobs quartz
				if (JobInformations.QUARTZ_AVAILABLE) {
					JobGlobalListener.destroyJobGlobalListener();
				}
			}
		} finally {
			if (contextFactoryEnabled) {
				MonitoringInitialContextFactory.stop();
			}

			// on arrête le thread du collector,
			// on persiste les compteurs pour les relire à l'initialisation et ne pas perdre les stats
			// et on vide les compteurs
			if (timer != null) {
				timer.cancel();
			}
			if (collector != null) {
				collector.stop();
			}
			Collector.stopJRobin();
			Collector.detachVirtualMachine();

			// nettoyage avant le retrait de la webapp au cas où celui-ci ne suffise pas
			collector = null;
			httpCounter = null;
			errorCounter = null;
			urlExcludePattern = null;
			allowedAddrPattern = null;
			filterConfig = null;
			timer = null;
		}
	}

	private void deregisterLogs() {
		if (LOG.LOGBACK_ENABLED) {
			LogbackAppender.getSingleton().deregister();
		}
		if (LOG.LOG4J_ENABLED) {
			Log4JAppender.getSingleton().deregister();
		}
		LoggingHandler.getSingleton().deregister();
	}

	/** {@inheritDoc} */
	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
			throws IOException, ServletException {
		if (!(request instanceof HttpServletRequest) || !(response instanceof HttpServletResponse)
				|| isHttpMonitoringDisabled() || isRequestExcluded((HttpServletRequest) request)) {
			// si ce n'est pas une requête http ou si le monitoring http est désactivé
			// ou si cette url est exclue, on ne monitore pas cette requête http
			chain.doFilter(request, response);
			return;
		}
		final HttpServletRequest httpRequest = (HttpServletRequest) request;
		final HttpServletResponse httpResponse = (HttpServletResponse) response;

		if (httpRequest.getRequestURI().equals(getMonitoringUrl(httpRequest))) {
			doMonitoring(httpRequest, httpResponse);
			return;
		}

		final HttpServletRequest wrappedRequest = JspWrapper.createHttpRequestWrapper(httpRequest);
		final CounterServletResponseWrapper wrappedResponse = new CounterServletResponseWrapper(
				httpResponse);
		final long start = System.currentTimeMillis();
		final long startCpuTime = ThreadInformations.getCurrentThreadCpuTime();
		boolean systemError = false;
		Throwable systemException = null;
		try {
			JdbcWrapper.ACTIVE_THREAD_COUNT.incrementAndGet();
			// on binde le contexte de la requête http pour les requêtes sql
			final String requestName = getCompleteRequestName(httpRequest, false);
			final String completeRequestName = getCompleteRequestName(httpRequest, true);
			httpCounter.bindContext(requestName, completeRequestName, httpRequest.getRemoteUser(),
					startCpuTime);
			// on binde la requête http (utilisateur courant et requête complète) pour les derniers logs d'erreurs
			httpRequest.setAttribute(CounterError.REQUEST_KEY, completeRequestName);
			CounterError.bindRequest(httpRequest);
			chain.doFilter(wrappedRequest, wrappedResponse);
			wrappedResponse.flushBuffer();
		} catch (final Throwable t) { // NOPMD
			// on catche Throwable pour avoir tous les cas d'erreur système
			systemException = t;
			throwException(t);
		} finally {
			try {
				// Si la durée est négative (arrive bien que rarement en cas de synchronisation d'horloge système),
				// alors on considère que la durée est 0.

				// Rq : sous Windows XP, currentTimeMillis a une résolution de 16ms environ
				// (discrètisation de la durée en 0, 16 ou 32 ms, etc ...)
				// et sous linux ou Windows Vista la résolution est bien meilleure.
				// On n'utilise pas nanoTime car il peut être un peu plus lent (mesuré à 2 microsecondes,
				// voir aussi http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=6440250)
				// et car des millisecondes suffisent pour une requête http
				final long duration = Math.max(System.currentTimeMillis() - start, 0);
				final long cpuUsedMillis = (ThreadInformations.getCurrentThreadCpuTime() - startCpuTime) / 1000000;

				JdbcWrapper.ACTIVE_THREAD_COUNT.decrementAndGet();

				putUserInfoInSession(httpRequest);

				if (systemException != null) {
					systemError = true;
					final StringWriter stackTrace = new StringWriter(200);
					systemException.printStackTrace(new PrintWriter(stackTrace));
					errorCounter.addRequestForSystemError(systemException.toString(), duration,
							cpuUsedMillis, stackTrace.toString());
				} else if (wrappedResponse.getStatus() >= 400) {
					systemError = true;
					errorCounter.addRequestForSystemError("Error" + wrappedResponse.getStatus(),
							duration, cpuUsedMillis, null);
				}

				// taille du flux sortant
				final int responseSize = wrappedResponse.getDataLength();
				// nom identifiant la requête
				final String requestName = getRequestName(httpRequest, wrappedResponse);

				// on enregistre la requête dans les statistiques
				httpCounter.addRequest(requestName, duration, cpuUsedMillis, systemError,
						responseSize);
				// on log sur Log4J ou java.util.logging dans la catégorie correspond au nom du filtre dans web.xml
				log(httpRequest, requestName, duration, systemError, responseSize);
			} finally {
				// normalement le unbind du contexte a été fait dans httpCounter.addRequest
				// mais pour être sûr au cas où il y ait une exception comme OutOfMemoryError
				// on le refait ici pour éviter des erreurs par la suite,
				// car il ne doit pas y avoir de contexte restant au delà de la requête http
				httpCounter.unbindContext();
				// et unbind de la requête http
				CounterError.unbindRequest();
			}
		}
	}

	protected final String getMonitoringUrl(HttpServletRequest httpRequest) {
		if (monitoringUrl == null) {
			final String parameterValue = Parameters.getParameter(Parameter.MONITORING_PATH);
			if (parameterValue == null) {
				monitoringUrl = httpRequest.getContextPath() + "/monitoring";
			} else {
				monitoringUrl = httpRequest.getContextPath() + parameterValue;
			}
		}
		return monitoringUrl;
	}

	private void putUserInfoInSession(HttpServletRequest httpRequest) {
		final HttpSession session = httpRequest.getSession(false);
		if (session == null) {
			// la session n'est pas encore créée (et ne le sera peut-être jamais)
			return;
		}
		// on ne met dans la session ces attributs que si ils n'y sont pas déjà
		// (pour que la session ne soit pas resynchronisée si serveur en cluster par exemple),
		// donc l'adresse ip est celle de la première requête créant une session,
		// et si l'adresse ip change ensuite c'est très étrange
		// mais elle n'est pas mise à jour dans la session
		if (session.getAttribute(SessionInformations.SESSION_COUNTRY_KEY) == null) {
			// langue préférée du navigateur, getLocale ne peut être null
			final Locale locale = httpRequest.getLocale();
			if (locale.getCountry().length() > 0) {
				session.setAttribute(SessionInformations.SESSION_COUNTRY_KEY, locale.getCountry());
			} else {
				session.setAttribute(SessionInformations.SESSION_COUNTRY_KEY, locale.getLanguage());
			}
		}
		if (session.getAttribute(SessionInformations.SESSION_REMOTE_ADDR) == null) {
			// adresse ip
			final String forwardedFor = httpRequest.getHeader("X-Forwarded-For");
			final String remoteAddr;
			if (forwardedFor == null) {
				remoteAddr = httpRequest.getRemoteAddr();
			} else {
				remoteAddr = httpRequest.getRemoteAddr() + " forwarded for " + forwardedFor;
			}
			session.setAttribute(SessionInformations.SESSION_REMOTE_ADDR, remoteAddr);
		}
		if (session.getAttribute(SessionInformations.SESSION_REMOTE_USER) == null) {
			// login utilisateur, peut être null
			final String remoteUser = httpRequest.getRemoteUser();
			if (remoteUser != null) {
				session.setAttribute(SessionInformations.SESSION_REMOTE_USER, remoteUser);
			}
		}
	}

	private void doMonitoring(HttpServletRequest httpRequest, HttpServletResponse httpResponse)
			throws IOException {
		if (isRequestNotAllowed(httpRequest)) {
			httpResponse.sendError(HttpServletResponse.SC_FORBIDDEN, "Forbidden access");
			return;
		}
		final MonitoringController monitoringController = new MonitoringController(collector, null);
		monitoringController.executeActionIfNeeded(httpRequest);
		// javaInformations doit être réinstanciée et doit être après executeActionIfNeeded
		// pour avoir des informations à jour
		final JavaInformations javaInformations;
		if (monitoringController.isJavaInformationsNeeded(httpRequest)) {
			javaInformations = new JavaInformations(filterConfig.getServletContext(), true);
		} else {
			javaInformations = null;
		}
		monitoringController.doReport(httpRequest, httpResponse,
				Collections.singletonList(javaInformations));
	}

	private static String getCompleteRequestName(HttpServletRequest httpRequest,
			boolean includeQueryString) {
		// on ne prend pas httpRequest.getPathInfo()
		// car requestURI == <context>/<servlet>/<pathInfo>,
		// et dans le cas où il y a plusieurs servlets (par domaine fonctionnel ou technique)
		// pathInfo ne contient pas l'indication utile de la servlet
		final String tmp = httpRequest.getRequestURI().substring(
				httpRequest.getContextPath().length());
		final String method;
		if ("XMLHttpRequest".equals(httpRequest.getHeader("X-Requested-With"))) {
			method = "ajax " + httpRequest.getMethod();
		} else {
			method = httpRequest.getMethod();
		}
		if (!includeQueryString) {
			return tmp + ' ' + method;
		}
		final String queryString = httpRequest.getQueryString();
		if (queryString == null) {
			return tmp + ' ' + method;
		}
		return tmp + '?' + queryString + ' ' + method;
	}

	private static String getRequestName(HttpServletRequest httpRequest,
			CounterServletResponseWrapper wrappedResponse) {
		if (wrappedResponse.getStatus() == HttpServletResponse.SC_NOT_FOUND) {
			// Sécurité : si status http est 404, alors requestName est Error404
			// pour éviter de saturer la mémoire avec potentiellement beaucoup d'url différentes
			return "Error404";
		}
		return getCompleteRequestName(httpRequest, false);
	}

	private boolean isHttpMonitoringDisabled() {
		return monitoringDisabled || !httpCounter.isDisplayed();
	}

	private boolean isRequestExcluded(HttpServletRequest httpRequest) {
		return urlExcludePattern != null
				&& urlExcludePattern.matcher(
						httpRequest.getRequestURI()
								.substring(httpRequest.getContextPath().length())).matches();
	}

	private boolean isRequestNotAllowed(HttpServletRequest httpRequest) {
		return allowedAddrPattern != null
				&& !allowedAddrPattern.matcher(httpRequest.getRemoteAddr()).matches();
	}

	// cette méthode est protected pour pouvoir être surchargée dans une classe définie par l'application
	protected void log(HttpServletRequest httpRequest, String requestName, long duration,
			boolean systemError, int responseSize) {
		if (!logEnabled) {
			return;
		}
		final String filterName = filterConfig.getFilterName();
		LOG.log(httpRequest, requestName, duration, systemError, responseSize, filterName);
	}

	private static void throwException(Throwable t) throws IOException, ServletException {
		if (t instanceof Error) {
			throw (Error) t;
		} else if (t instanceof RuntimeException) {
			throw (RuntimeException) t;
		} else if (t instanceof IOException) {
			throw (IOException) t;
		} else if (t instanceof ServletException) {
			throw (ServletException) t;
		} else {
			// n'arrive à priori pas car chain.doFilter ne déclare que IOException et ServletException
			// mais au cas où
			throw new ServletException(t.getMessage(), t);
		}
	}
}
