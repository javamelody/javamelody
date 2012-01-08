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

import static net.bull.javamelody.HttpParameters.COLLECTOR_PARAMETER;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Collections;
import java.util.Locale;
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

	private static boolean instanceCreated;

	private final boolean instanceEnabled;

	// Ces variables httpCounter et errorCounter conservent un état qui est global au filtre
	// et à l'application (donc thread-safe).
	private Counter httpCounter;
	private Counter errorCounter;

	private boolean monitoringDisabled;
	private boolean logEnabled;
	private Pattern urlExcludePattern;
	private Pattern allowedAddrPattern;
	private FilterContext filterContext;
	private FilterConfig filterConfig;
	private String monitoringUrl;

	/**
	 * Constructeur.
	 */
	public MonitoringFilter() {
		super();
		if (instanceCreated) {
			// ce filter a déjà été chargé précédemment et est chargé une 2ème fois donc on désactive cette 2ème instance
			// (cela peut arriver par exemple dans glassfish v3 lorsque le filter est déclaré dans le fichier web.xml
			// et déclaré par ailleurs dans le fichier web-fragment.xml à l'intérieur du jar)
			instanceEnabled = false;
		} else {
			instanceEnabled = true;
			setInstanceCreated(true);
		}
	}

	private static void setInstanceCreated(boolean newInstanceCreated) {
		instanceCreated = newInstanceCreated;
	}

	/** {@inheritDoc} */
	public void init(FilterConfig config) throws ServletException {
		if (!instanceEnabled) {
			return;
		}
		this.filterConfig = config;
		Parameters.initialize(config);
		monitoringDisabled = Boolean.parseBoolean(Parameters.getParameter(Parameter.DISABLED));
		if (monitoringDisabled) {
			return;
		}

		LOG.debug("JavaMelody filter init started");

		this.filterContext = new FilterContext();
		final Collector collector = filterContext.getCollector();
		this.httpCounter = collector.getCounterByName(Counter.HTTP_COUNTER_NAME);
		this.errorCounter = collector.getCounterByName(Counter.ERROR_COUNTER_NAME);

		logEnabled = Boolean.parseBoolean(Parameters.getParameter(Parameter.LOG));
		if (Parameters.getParameter(Parameter.URL_EXCLUDE_PATTERN) != null) {
			// lance une PatternSyntaxException si la syntaxe du pattern est invalide
			urlExcludePattern = Pattern.compile(Parameters
					.getParameter(Parameter.URL_EXCLUDE_PATTERN));
		}
		if (Parameters.getParameter(Parameter.ALLOWED_ADDR_PATTERN) != null) {
			allowedAddrPattern = Pattern.compile(Parameters
					.getParameter(Parameter.ALLOWED_ADDR_PATTERN));
		}

		LOG.debug("JavaMelody filter init done");
	}

	/** {@inheritDoc} */
	public void destroy() {
		if (monitoringDisabled || !instanceEnabled) {
			return;
		}
		try {
			if (filterContext != null) {
				filterContext.destroy();
			}
		} finally {
			// nettoyage avant le retrait de la webapp au cas où celui-ci ne suffise pas
			httpCounter = null;
			errorCounter = null;
			urlExcludePattern = null;
			allowedAddrPattern = null;
			filterConfig = null;
			filterContext = null;
		}
	}

	/** {@inheritDoc} */
	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
			throws IOException, ServletException {
		if (!(request instanceof HttpServletRequest) || !(response instanceof HttpServletResponse)
				|| monitoringDisabled || !instanceEnabled) {
			// si ce n'est pas une requête http ou si le monitoring est désactivé, on fait suivre
			chain.doFilter(request, response);
			return;
		}
		final HttpServletRequest httpRequest = (HttpServletRequest) request;
		final HttpServletResponse httpResponse = (HttpServletResponse) response;

		if (httpRequest.getRequestURI().equals(getMonitoringUrl(httpRequest))) {
			doMonitoring(httpRequest, httpResponse);
			return;
		}
		if (!httpCounter.isDisplayed() || isRequestExcluded((HttpServletRequest) request)) {
			// si cette url est exclue ou si le counter http est désactivé, on ne monitore pas cette requête http
			chain.doFilter(request, response);
			return;
		}

		doFilter(chain, httpRequest, httpResponse);
	}

	private void doFilter(FilterChain chain, HttpServletRequest httpRequest,
			HttpServletResponse httpResponse) throws IOException, ServletException {
		HttpServletRequest wrappedRequest = JspWrapper.createHttpRequestWrapper(httpRequest);
		if (httpRequest.getContentType() != null
				&& httpRequest.getContentType().startsWith("text/x-gwt-rpc")) {
			wrappedRequest = new GWTRequestWrapper(wrappedRequest);
		}
		final CounterServletResponseWrapper wrappedResponse = new CounterServletResponseWrapper(
				httpResponse);
		final long start = System.currentTimeMillis();
		final long startCpuTime = ThreadInformations.getCurrentThreadCpuTime();
		boolean systemError = false;
		Throwable systemException = null;
		String requestName = getCompleteRequestName(wrappedRequest, false);
		final String completeRequestName = getCompleteRequestName(wrappedRequest, true);
		try {
			JdbcWrapper.ACTIVE_THREAD_COUNT.incrementAndGet();
			// on binde le contexte de la requête http pour les requêtes sql
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
				} else if (wrappedResponse.getCurrentStatus() >= 400) {
					systemError = true;
					errorCounter.addRequestForSystemError(
							"Error" + wrappedResponse.getCurrentStatus(), duration, cpuUsedMillis,
							null);
				}

				// taille du flux sortant
				final int responseSize = wrappedResponse.getDataLength();
				// nom identifiant la requête
				if (wrappedResponse.getCurrentStatus() == HttpServletResponse.SC_NOT_FOUND) {
					// Sécurité : si status http est 404, alors requestName est Error404
					// pour éviter de saturer la mémoire avec potentiellement beaucoup d'url différentes
					requestName = "Error404";
				}

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
		final Collector collector = filterContext.getCollector();
		final MonitoringController monitoringController = new MonitoringController(collector, null);
		monitoringController.executeActionIfNeeded(httpRequest);
		// javaInformations doit être réinstanciée et doit être après executeActionIfNeeded
		// pour avoir des informations à jour
		final JavaInformations javaInformations;
		if (MonitoringController.isJavaInformationsNeeded(httpRequest)) {
			javaInformations = new JavaInformations(filterConfig.getServletContext(), true);
		} else {
			javaInformations = null;
		}
		monitoringController.doReport(httpRequest, httpResponse,
				Collections.singletonList(javaInformations));

		if ("stop".equalsIgnoreCase(httpRequest.getParameter(COLLECTOR_PARAMETER))) {
			// on a été appelé par un serveur de collecte qui fera l'aggrégation dans le temps,
			// le stockage et les courbes, donc on arrête le timer s'il est démarré
			// et on vide les stats pour que le serveur de collecte ne récupère que les deltas
			if (filterContext.getTimer() != null) {
				filterContext.getTimer().cancel();
			}
			collector.stop();
		}
	}

	private static String getCompleteRequestName(HttpServletRequest httpRequest,
			boolean includeQueryString) {
		// on ne prend pas httpRequest.getPathInfo()
		// car requestURI == <context>/<servlet>/<pathInfo>,
		// et dans le cas où il y a plusieurs servlets (par domaine fonctionnel ou technique)
		// pathInfo ne contient pas l'indication utile de la servlet
		String tmp = httpRequest.getRequestURI().substring(httpRequest.getContextPath().length());
		// si la requête http contient un ";", par exemple ";jsessionid=12345567890ABCDEF"
		// quand le navigateur web n'accepte pas les cookies, alors on ignore ce qu'il y a à partir de ";"
		// et on ne garde que la requête http elle-même
		final int lastIndexOfSemiColon = tmp.lastIndexOf(';');
		if (lastIndexOfSemiColon != -1) {
			tmp = tmp.substring(0, lastIndexOfSemiColon);
		}
		final String method;
		if ("XMLHttpRequest".equals(httpRequest.getHeader("X-Requested-With"))) {
			method = "ajax " + httpRequest.getMethod();
		} else {
			method = httpRequest.getMethod();
		}
		if (!includeQueryString) {
			//20091201 dhartford:Check for GWT for modified http request statistic gathering.
			if (httpRequest instanceof GWTRequestWrapper) {
				//Cast the GWT HttpServletRequestWrapper object, get the actual payload for
				//type x-gwt-rpc, and obtain methodname (manually, the 7th pipe-delimited item).
				final GWTRequestWrapper wrapper = (GWTRequestWrapper) httpRequest;
				return tmp + '.' + wrapper.getGwtRpcMethodName() + " GWT-RPC";
			}

			return tmp + ' ' + method;
		}
		final String queryString = httpRequest.getQueryString();
		if (queryString == null) {
			return tmp + ' ' + method;
		}
		return tmp + '?' + queryString + ' ' + method;
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
		LOG.logHttpRequest(httpRequest, requestName, duration, systemError, responseSize,
				filterName);
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

	FilterContext getFilterContext() {
		return filterContext;
	}
}
