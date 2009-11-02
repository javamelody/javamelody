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
import java.util.logging.Level;
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
	private boolean log4jEnabled;
	private boolean contextFactoryEnabled;
	private Pattern urlExcludePattern;
	private Pattern allowedAddrPattern;
	private FilterConfig filterConfig;
	private String monitoringUrl;
	private Timer timer;

	private static final class CollectTimerTask extends TimerTask {
		private final Collector collector;

		CollectTimerTask(Collector collector) {
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
		this.timer = new Timer("javamelody"
				+ Parameters.getContextPath(config.getServletContext()).replace('/', ' '), true);

		logEnabled = Boolean.parseBoolean(Parameters.getParameter(Parameter.LOG));
		// on branche le handler java.util.logging pour le counter de logs
		LoggingHandler.getSingleton().register();
		try {
			Class.forName("org.apache.log4j.Logger");
			log4jEnabled = true;
			// si log4j est disponible on branche aussi l'appender pour le counter de logs
			Log4JAppender.getSingleton().register();
		} catch (final ClassNotFoundException e) {
			log4jEnabled = false;
		}

		this.contextFactoryEnabled = !monitoringDisabled
				&& Boolean.parseBoolean(Parameters.getParameter(Parameter.CONTEXT_FACTORY_ENABLED));
		if (contextFactoryEnabled) {
			MonitoringInitialContextFactory.init();
		}

		final boolean noDatabase = Parameters.isNoDatabase();
		final JdbcWrapper jdbcWrapper = JdbcDriver.SINGLETON.getJdbcWrapper();
		// si l'application a utilisé JdbcDriver avant d'initialiser ce filtre
		// (par exemple dans un listener de contexte), on doit récupérer son sqlCounter
		// car il est lié à une connexion jdbc qui est certainement conservée dans un pool
		// (sinon les requêtes sql sur cette connexion ne seront pas monitorées)
		final Counter sqlCounter = jdbcWrapper.getSqlCounter();
		// sqlCounter dans JdbcWrapper peut être alimenté soit par une datasource soit par un driver
		jdbcWrapper.initServletContext(config.getServletContext());
		if (!noDatabase) {
			jdbcWrapper.rebindDataSources();
		}

		// liaison des compteurs : les contextes par thread du sqlCounter ont pour parent le httpCounter
		this.httpCounter = new Counter("http", "dbweb.png", sqlCounter);
		this.errorCounter = new Counter(Counter.ERROR_COUNTER_NAME, "error.png");
		this.errorCounter.setMaxRequestsCount(250);

		final String application = Parameters.getCurrentApplication();
		final Counter ejbCounter = MonitoringInterceptor.getEjbCounter();
		final Counter springCounter = MonitoringInterceptor.getSpringCounter();
		final Counter servicesCounter = MonitoringProxy.getServicesCounter();
		final Counter logCounter = LoggingHandler.getLogCounter();

		final List<Counter> counters = Arrays.asList(new Counter[] { httpCounter, sqlCounter,
				ejbCounter, springCounter, servicesCounter, errorCounter, logCounter, });
		setRequestTransformPatterns(counters);
		final String displayedCounters = Parameters.getParameter(Parameter.DISPLAYED_COUNTERS);
		// displayedCounters doit être traité avant l'initialisation du collector
		// sinon les dayCounters ne seront pas bons
		if (displayedCounters == null) {
			// par défaut, tous les compteurs sont affichés sauf ejb
			httpCounter.setDisplayed(true);
			sqlCounter.setDisplayed(!noDatabase);
			errorCounter.setDisplayed(true);
			logCounter.setDisplayed(true);
			ejbCounter.setDisplayed(false);
			springCounter.setDisplayed(false);
			servicesCounter.setDisplayed(false);
		} else {
			setDisplayedCounters(counters, displayedCounters);
		}
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
			counter.setDisplayed(false);
		}
		for (final String displayedCounter : displayedCounters.split(",")) {
			final String displayedCounterName = displayedCounter.trim();
			for (final Counter counter : counters) {
				if (displayedCounterName.equalsIgnoreCase(counter.getName())) {
					counter.setDisplayed(true);
					break;
				}
			}
		}
	}

	private void initCollect() {
		try {
			Class.forName("org.jrobin.core.RrdDb");
		} catch (final ClassNotFoundException e) {
			// si pas de jar jrobin, alors pas de collecte
			return;
		}

		final int periodMillis = Parameters.getResolutionSeconds() * 1000;
		// on schedule la tâche de fond
		final TimerTask task = new CollectTimerTask(collector);
		timer.schedule(task, periodMillis, periodMillis);

		// on appelle la collecte pour que les instances jrobin soient définies
		// au cas où un graph de la page de monitoring soit demandé de suite
		collector.collectLocalContextWithoutErrors();

		if (Parameters.getParameter(Parameter.MAIL_SESSION) != null
				&& Parameters.getParameter(Parameter.ADMIN_EMAILS) != null) {
			MailReport.scheduleReportMailForLocalServer(collector, timer);
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
					new MonitoringController(collector, false).writeHtmlToLastShutdownFile();
				}
			} finally {
				//on rebind les dataSources initiales à la place des proxy
				JdbcDriver.SINGLETON.getJdbcWrapper().stop();
				JdbcDriver.SINGLETON.deregister();

				// on enlève l'appender de log4j et le handler de java.util.logging
				if (log4jEnabled) {
					Log4JAppender.getSingleton().deregister();
				}
				LoggingHandler.getSingleton().deregister();
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

	/** {@inheritDoc} */
	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
			throws IOException, ServletException {
		if (!(request instanceof HttpServletRequest) || !(response instanceof HttpServletResponse)
				|| isHttpMonitoringDisabled()) {
			// si ce n'est pas une requête http, on ne la monitore pas
			chain.doFilter(request, response);
			return;
		}
		final HttpServletRequest httpRequest = (HttpServletRequest) request;
		final HttpServletResponse httpResponse = (HttpServletResponse) response;

		if (isRequestExcluded(httpRequest)) {
			// si cette url est exclue, on ne monitore pas cette requête http
			chain.doFilter(request, response);
			return;
		}
		if (httpRequest.getRequestURI().equals(getMonitoringUrl(httpRequest))) {
			doMonitoring(httpRequest, httpResponse);
			return;
		}

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
			chain.doFilter(request, wrappedResponse);
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
			monitoringUrl = httpRequest.getContextPath() + "/monitoring";
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
			session.setAttribute(SessionInformations.SESSION_REMOTE_ADDR, httpRequest
					.getRemoteAddr());
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
			httpResponse.sendError(HttpServletResponse.SC_FORBIDDEN, "Accès interdit");
			return;
		}
		final MonitoringController monitoringController = new MonitoringController(collector, false);
		monitoringController.executeActionIfNeeded(httpRequest);
		// javaInformations doit être réinstanciée et doit être après executeActionIfNeeded
		// pour avoir des informations à jour
		final JavaInformations javaInformations;
		if (monitoringController.isJavaInformationsNeeded(httpRequest)) {
			javaInformations = new JavaInformations(filterConfig.getServletContext(), true);
		} else {
			javaInformations = null;
		}
		monitoringController.doReport(httpRequest, httpResponse, Collections
				.singletonList(javaInformations));
	}

	private static String getCompleteRequestName(HttpServletRequest httpRequest,
			boolean includeQueryString) {
		// on ne prend pas httpRequest.getPathInfo()
		// car requestURI == <context>/<servlet>/<pathInfo>,
		// et dans le cas où il y a plusieurs servlets (par domaine fonctionnel ou technique)
		// pathInfo ne contient pas l'indication utile de la servlet
		final String tmp = httpRequest.getRequestURI().substring(
				httpRequest.getContextPath().length());
		if (!includeQueryString) {
			return tmp + ' ' + httpRequest.getMethod();
		}
		final String queryString = httpRequest.getQueryString();
		if (queryString == null) {
			return tmp + ' ' + httpRequest.getMethod();
		}
		return tmp + '?' + queryString + ' ' + httpRequest.getMethod();
	}

	private static String getRequestName(HttpServletRequest httpRequest,
			CounterServletResponseWrapper wrappedResponse) {
		final String requestName;
		if (wrappedResponse.getStatus() == HttpServletResponse.SC_NOT_FOUND) {
			// Sécurité : si status http est 404, alors requestName est Error404
			// pour éviter de saturer la mémoire avec potentiellement beaucoup d'url différentes
			requestName = "Error404";
		} else {
			return getCompleteRequestName(httpRequest, false);
		}
		return requestName;
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

	private boolean isRequestNotAllowed(final HttpServletRequest httpRequest) {
		return allowedAddrPattern != null
				&& !allowedAddrPattern.matcher(httpRequest.getRemoteAddr()).matches();
	}

	// cette méthode est protected pour pouvoir être surchargée dans une classe définie par l'application
	@SuppressWarnings("unused")
	protected void log(HttpServletRequest httpRequest, String requestName, long duration,
			boolean systemError, int responseSize) {
		if (!logEnabled) {
			return;
		}
		// dans les 2 cas, on ne construit le message de log
		// que si le logger est configuré pour écrire le niveau INFO
		final String filterName = filterConfig.getFilterName();
		if (log4jEnabled) {
			log4j(httpRequest, duration, systemError, responseSize, filterName);
		} else {
			final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(filterName);
			if (logger.isLoggable(Level.INFO)) {
				logger.info(buildLogMessage(httpRequest, duration, systemError, responseSize));
			}
		}
	}

	private static void log4j(HttpServletRequest httpRequest, long duration, boolean systemError,
			int responseSize, String filterName) {
		// la variable logger doit être dans une méthode à part pour ne pas faire ClassNotFoundException
		// si log4j non présent (mais variable préférable pour performance)
		final org.apache.log4j.Logger logger = org.apache.log4j.Logger.getLogger(filterName);
		if (logger.isInfoEnabled()) {
			logger.info(buildLogMessage(httpRequest, duration, systemError, responseSize));
		}
	}

	private static String buildLogMessage(HttpServletRequest httpRequest, long duration,
			boolean systemError, int responseSize) {
		final StringBuilder msg = new StringBuilder();
		msg.append("remoteAddr = ").append(httpRequest.getRemoteAddr());
		msg.append(", request = ").append(
				httpRequest.getRequestURI().substring(httpRequest.getContextPath().length()));
		if (httpRequest.getQueryString() != null) {
			msg.append('?').append(httpRequest.getQueryString());
		}
		msg.append(' ').append(httpRequest.getMethod());
		msg.append(": ").append(duration).append(" ms");
		if (systemError) {
			msg.append(", erreur");
		}
		msg.append(", ").append(responseSize / 1024).append(" Ko");
		return msg.toString();
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
