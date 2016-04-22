/*
 * Copyright 2008-2016 by Emeric Vernat
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
package net.bull.javamelody;

import static net.bull.javamelody.HttpParameters.COLLECTOR_PARAMETER;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;
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

	private static final List<String> CONTEXT_PATHS = new ArrayList<String>();

	private boolean instanceEnabled;

	// Ces variables httpCounter et errorCounter conservent un état qui est global au filtre
	// et à l'application (donc thread-safe).
	private Counter httpCounter;
	private Counter errorCounter;

	private boolean monitoringDisabled;
	private boolean logEnabled;
	private Pattern urlExcludePattern;
	private FilterContext filterContext;
	private HttpAuth httpAuth;
	private FilterConfig filterConfig;
	private String monitoringUrl;
	private boolean servletApi2;

	/**
	 * Constructeur.
	 */
	public MonitoringFilter() {
		super();
		if (instanceCreated) {
			// ce filter a déjà été chargé précédemment et est chargé une 2ème fois donc on désactive cette 2ème instance
			// (cela peut arriver par exemple dans glassfish v3 lorsque le filter est déclaré dans le fichier web.xml
			// et déclaré par ailleurs dans le fichier web-fragment.xml à l'intérieur du jar, issue 147),
			// mais il peut être réactivé dans init (issue 193)
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
	@Override
	public void init(FilterConfig config) throws ServletException {
		final long start = System.currentTimeMillis(); // NOPMD
		final String contextPath = Parameters.getContextPath(config.getServletContext());
		if (!instanceEnabled) {
			if (!CONTEXT_PATHS.contains(contextPath)) {
				// si jars dans tomcat/lib, il y a plusieurs instances mais dans des webapps différentes (issue 193)
				instanceEnabled = true;
			} else {
				return;
			}
		}
		CONTEXT_PATHS.add(contextPath);
		this.filterConfig = config;
		this.servletApi2 = config.getServletContext().getMajorVersion() < 3;
		Parameters.initialize(config);
		monitoringDisabled = Boolean.parseBoolean(Parameters.getParameter(Parameter.DISABLED));
		if (monitoringDisabled) {
			return;
		}

		LOG.debug("JavaMelody filter init started");

		this.filterContext = new FilterContext();
		this.httpAuth = new HttpAuth();
		config.getServletContext().setAttribute(ReportServlet.FILTER_CONTEXT_KEY, filterContext);
		final Collector collector = filterContext.getCollector();
		this.httpCounter = collector.getCounterByName(Counter.HTTP_COUNTER_NAME);
		this.errorCounter = collector.getCounterByName(Counter.ERROR_COUNTER_NAME);

		logEnabled = Boolean.parseBoolean(Parameters.getParameter(Parameter.LOG));
		if (Parameters.getParameter(Parameter.URL_EXCLUDE_PATTERN) != null) {
			// lance une PatternSyntaxException si la syntaxe du pattern est invalide
			urlExcludePattern = Pattern
					.compile(Parameters.getParameter(Parameter.URL_EXCLUDE_PATTERN));
		}

		final long duration = System.currentTimeMillis() - start;
		LOG.debug("JavaMelody filter init done in " + duration + " ms");
	}

	/** {@inheritDoc} */
	@Override
	public void destroy() {
		if (monitoringDisabled || !instanceEnabled) {
			return;
		}
		final long start = System.currentTimeMillis();
		try {
			if (filterContext != null) {
				filterContext.destroy();
			}
		} finally {
			final String contextPath = Parameters.getContextPath(filterConfig.getServletContext());
			CONTEXT_PATHS.remove(contextPath);
			// nettoyage avant le retrait de la webapp au cas où celui-ci ne suffise pas
			httpCounter = null;
			errorCounter = null;
			urlExcludePattern = null;
			filterConfig = null;
			filterContext = null;
		}
		final long duration = System.currentTimeMillis() - start;
		LOG.debug("JavaMelody filter destroy done in " + duration + " ms");
	}

	/** {@inheritDoc} */
	@Override
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
		final CounterServletResponseWrapper wrappedResponse = new CounterServletResponseWrapper(
				httpResponse);
		final HttpServletRequest wrappedRequest = createRequestWrapper(httpRequest,
				wrappedResponse);
		final long start = System.currentTimeMillis();
		final long startCpuTime = ThreadInformations.getCurrentThreadCpuTime();
		boolean systemError = false;
		Throwable systemException = null;
		String requestName = getRequestName(wrappedRequest);
		final String completeRequestName = getCompleteRequestName(wrappedRequest, true);
		try {
			JdbcWrapper.ACTIVE_THREAD_COUNT.incrementAndGet();
			// on binde le contexte de la requête http pour les requêtes sql
			httpCounter.bindContext(requestName, completeRequestName, httpRequest.getRemoteUser(),
					startCpuTime);
			// on binde la requête http (utilisateur courant et requête complète) pour les derniers logs d'erreurs
			httpRequest.setAttribute(CounterError.REQUEST_KEY, completeRequestName);
			CounterError.bindRequest(httpRequest);
			chain.doFilter(httpRequest, httpResponse);
			if (servletApi2 || !httpRequest.isAsyncStarted()) {
				wrappedResponse.flushBuffer();
			}
		} catch (final Throwable t) { // NOPMD
			// on catche Throwable pour avoir tous les cas d'erreur système
			systemException = t;
			throwException(t);
		} finally {
			if (httpCounter == null) {
				// "the destroy method is only called once all threads within the filter's doFilter method have exited
				// or after a timeout period has passed"
				// si timeout, alors on évite ici une NPE (cf  issue 262)
				return; // NOPMD
			}
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
				final long cpuUsedMillis = (ThreadInformations.getCurrentThreadCpuTime()
						- startCpuTime) / 1000000;

				JdbcWrapper.ACTIVE_THREAD_COUNT.decrementAndGet();

				putUserInfoInSession(httpRequest);

				if (systemException != null) {
					systemError = true;
					final StringWriter stackTrace = new StringWriter(200);
					systemException.printStackTrace(new PrintWriter(stackTrace));
					errorCounter.addRequestForSystemError(systemException.toString(), duration,
							cpuUsedMillis, stackTrace.toString());
				} else if (wrappedResponse.getCurrentStatus() >= HttpServletResponse.SC_BAD_REQUEST
						&& wrappedResponse
								.getCurrentStatus() != HttpServletResponse.SC_UNAUTHORIZED) {
					// SC_UNAUTHORIZED (401) is not an error, it is the first handshake of a Basic (or Digest) Auth (issue 455)
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

	protected HttpServletRequest createRequestWrapper(HttpServletRequest request,
			HttpServletResponse response) throws IOException {
		HttpServletRequest wrappedRequest = JspWrapper.createHttpRequestWrapper(request, response);
		final PayloadNameRequestWrapper payloadNameRequestWrapper = new PayloadNameRequestWrapper(
				wrappedRequest);
		payloadNameRequestWrapper.initialize();
		if (payloadNameRequestWrapper.getPayloadRequestType() != null) {
			wrappedRequest = payloadNameRequestWrapper;
		}
		return wrappedRequest;
	}

	protected String getRequestName(HttpServletRequest request) {
		return getCompleteRequestName(request, false);
	}

	protected final String getMonitoringUrl(HttpServletRequest httpRequest) {
		if (monitoringUrl == null) {
			monitoringUrl = httpRequest.getContextPath() + Parameters.getMonitoringPath();
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
		if (session.getAttribute(SessionInformations.SESSION_USER_AGENT) == null) {
			final String userAgent = httpRequest.getHeader("User-Agent");
			if (userAgent != null) {
				session.setAttribute(SessionInformations.SESSION_USER_AGENT, userAgent);
			}
		}
	}

	private void doMonitoring(HttpServletRequest httpRequest, HttpServletResponse httpResponse)
			throws IOException, ServletException {
		if (!isAllowed(httpRequest, httpResponse)) {
			return;
		}

		final Collector collector = filterContext.getCollector();
		final MonitoringController monitoringController = new MonitoringController(collector, null);
		monitoringController.doActionIfNeededAndReport(httpRequest, httpResponse,
				filterConfig.getServletContext());

		if ("stop".equalsIgnoreCase(httpRequest.getParameter(COLLECTOR_PARAMETER))) {
			// on a été appelé par un serveur de collecte qui fera l'aggrégation dans le temps,
			// le stockage et les courbes, donc on arrête le timer s'il est démarré
			// et on vide les stats pour que le serveur de collecte ne récupère que les deltas
			for (final Counter counter : collector.getCounters()) {
				counter.clear();
			}

			if (!collector.isStopped()) {
				LOG.debug(
						"Stopping the javamelody collector in this webapp, because a collector server from "
								+ httpRequest.getRemoteAddr()
								+ " wants to collect the data itself");
				filterContext.stopCollector();
			}
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
			//Check payload request to support GWT, SOAP, and XML-RPC statistic gathering
			if (httpRequest instanceof PayloadNameRequestWrapper) {
				final PayloadNameRequestWrapper wrapper = (PayloadNameRequestWrapper) httpRequest;
				return tmp + wrapper.getPayloadRequestName() + ' '
						+ wrapper.getPayloadRequestType();
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
		return urlExcludePattern != null && urlExcludePattern.matcher(
				httpRequest.getRequestURI().substring(httpRequest.getContextPath().length()))
				.matches();
	}

	// cette méthode est protected pour pouvoir être surchargée dans une classe définie par l'application
	protected boolean isAllowed(HttpServletRequest httpRequest, HttpServletResponse httpResponse)
			throws IOException {
		return httpAuth.isAllowed(httpRequest, httpResponse);
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
