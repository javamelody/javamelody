/*
 * Copyright 2008-2019 by Emeric Vernat
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

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.URL;
import java.net.URLEncoder;
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

import net.bull.javamelody.internal.common.HttpParameter;
import net.bull.javamelody.internal.common.HttpPart;
import net.bull.javamelody.internal.common.LOG;
import net.bull.javamelody.internal.common.Parameters;
import net.bull.javamelody.internal.model.Collector;
import net.bull.javamelody.internal.model.Counter;
import net.bull.javamelody.internal.model.CounterError;
import net.bull.javamelody.internal.model.CounterRequestContext;
import net.bull.javamelody.internal.model.LabradorRetriever;
import net.bull.javamelody.internal.model.ThreadInformations;
import net.bull.javamelody.internal.web.CounterServletResponseWrapper;
import net.bull.javamelody.internal.web.HttpAuth;
import net.bull.javamelody.internal.web.MonitoringController;
import net.bull.javamelody.internal.web.RumInjector;

/**
 * Filtre de servlet pour le monitoring.
 * C'est la classe de ce filtre qui doit être déclarée dans le fichier web.xml de la webapp.
 * @author Emeric Vernat
 */
public class MonitoringFilter implements Filter {

	private static boolean instanceCreated;

	private static final List<String> CONTEXT_PATHS = new ArrayList<String>();

	private static URL unregisterApplicationNodeInCollectServerUrl;

	private boolean instanceEnabled;

	// "Classic" by default
	// (not "Jenkins", "JIRA", "Confluence", "Bamboo", "Sonar", "Liferay", "Alfresco", "Grails", "Collector server")
	private String applicationType = "Classic";

	// Ces variables httpCounter et errorCounter conservent un état qui est global au filtre
	// et à l'application (donc thread-safe).
	private Counter httpCounter;
	private Counter errorCounter;

	private boolean monitoringDisabled;
	private boolean logEnabled;
	private boolean rumEnabled;
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

	/**
	 * @return Type of application
	 */
	public String getApplicationType() {
		return applicationType;
	}

	/**
	 * @param applicationType Type of application
	 */
	public void setApplicationType(final String applicationType) {
		this.applicationType = applicationType;
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
		monitoringDisabled = Parameter.DISABLED.getValueAsBoolean();
		if (monitoringDisabled) {
			return;
		}

		LOG.debug("JavaMelody filter init started");

		this.filterContext = new FilterContext(getApplicationType());
		this.httpAuth = new HttpAuth();
		config.getServletContext().setAttribute(ReportServlet.FILTER_CONTEXT_KEY, filterContext);
		final Collector collector = filterContext.getCollector();
		this.httpCounter = collector.getCounterByName(Counter.HTTP_COUNTER_NAME);
		this.errorCounter = collector.getCounterByName(Counter.ERROR_COUNTER_NAME);

		logEnabled = Parameter.LOG.getValueAsBoolean();
		rumEnabled = Parameter.RUM_ENABLED.getValueAsBoolean();
		if (Parameter.URL_EXCLUDE_PATTERN.getValue() != null) {
			// lance une PatternSyntaxException si la syntaxe du pattern est invalide
			urlExcludePattern = Pattern.compile(Parameter.URL_EXCLUDE_PATTERN.getValue());
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
		final long start = System.currentTimeMillis();
		final long startCpuTime = ThreadInformations.getCurrentThreadCpuTime();
		final long startAllocatedBytes = ThreadInformations.getCurrentThreadAllocatedBytes();
		final CounterServletResponseWrapper wrappedResponse = createResponseWrapper(httpRequest,
				httpResponse);
		final HttpServletRequest wrappedRequest = createRequestWrapper(httpRequest,
				wrappedResponse);
		boolean systemError = false;
		Throwable systemException = null;
		String requestName = getRequestName(wrappedRequest);
		final String completeRequestName = getCompleteRequestName(wrappedRequest, true);
		try {
			JdbcWrapper.ACTIVE_THREAD_COUNT.incrementAndGet();
			// on binde le contexte de la requête http pour les requêtes sql
			httpCounter.bindContext(requestName, completeRequestName, httpRequest, startCpuTime,
					startAllocatedBytes);
			// on binde la requête http (utilisateur courant et requête complète) pour les derniers logs d'erreurs
			httpRequest.setAttribute(CounterError.REQUEST_KEY, completeRequestName);
			CounterError.bindRequest(httpRequest);
			chain.doFilter(wrappedRequest, wrappedResponse);
			if (servletApi2 || !httpRequest.isAsyncStarted()) {
				wrappedResponse.flushStream();
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
				final int cpuUsedMillis = (int) ((ThreadInformations.getCurrentThreadCpuTime()
						- startCpuTime) / 1000000L);
				final int allocatedKBytes;
				if (startAllocatedBytes >= 0) {
					allocatedKBytes = (int) ((ThreadInformations.getCurrentThreadAllocatedBytes()
							- startAllocatedBytes) / 1024L);
				} else {
					allocatedKBytes = -1;
				}
				JdbcWrapper.ACTIVE_THREAD_COUNT.decrementAndGet();
				putUserInfoInSession(httpRequest);
				if (systemException != null) {
					systemError = true;
					final StringWriter stackTrace = new StringWriter(200);
					systemException.printStackTrace(new PrintWriter(stackTrace));
					errorCounter.addRequestForSystemError(systemException.toString(), duration,
							cpuUsedMillis, allocatedKBytes, stackTrace.toString());
				} else if (wrappedResponse.getCurrentStatus() >= HttpServletResponse.SC_BAD_REQUEST
						&& wrappedResponse
								.getCurrentStatus() != HttpServletResponse.SC_UNAUTHORIZED) {
					// SC_UNAUTHORIZED (401) is not an error, it is the first handshake of a Basic (or Digest) Auth (issue 455)
					systemError = true;
					errorCounter.addRequestForSystemError(
							"Error" + wrappedResponse.getCurrentStatus(), duration, cpuUsedMillis,
							allocatedKBytes, null);
				}
				// prise en compte de Spring bestMatchingPattern s'il y a
				requestName = CounterRequestContext.getHttpRequestName(httpRequest, requestName);
				// taille du flux sortant
				final long responseSize = wrappedResponse.getDataLength();
				// nom identifiant la requête
				if (wrappedResponse.getCurrentStatus() == HttpServletResponse.SC_NOT_FOUND) {
					// Sécurité : si status http est 404, alors requestName est Error404
					// pour éviter de saturer la mémoire avec potentiellement beaucoup d'url différentes
					requestName = "Error404";
				}

				// on enregistre la requête dans les statistiques
				httpCounter.addRequest(requestName, duration, cpuUsedMillis, allocatedKBytes,
						systemError, responseSize);
				// on log sur Log4J ou java.util.logging dans la catégorie correspond au nom du filtre dans web.xml
				log(httpRequest, requestName, duration, systemError,
						wrappedResponse.getCurrentStatus(), responseSize);
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

	protected CounterServletResponseWrapper createResponseWrapper(HttpServletRequest httpRequest,
			HttpServletResponse httpResponse) {
		HttpServletResponse httpResponse2 = httpResponse;
		if (rumEnabled) {
			httpResponse2 = RumInjector.createRumResponseWrapper(httpRequest, httpResponse,
					getRequestName(httpRequest));
		}
		return new CounterServletResponseWrapper(httpResponse2);
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

	protected String getMonitoringUrl(HttpServletRequest httpRequest) {
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
		if (session.getAttribute(SessionListener.SESSION_COUNTRY_KEY) == null) {
			// langue préférée du navigateur, getLocale ne peut être null
			final Locale locale = httpRequest.getLocale();
			if (!locale.getCountry().isEmpty()) {
				session.setAttribute(SessionListener.SESSION_COUNTRY_KEY, locale.getCountry());
			} else {
				session.setAttribute(SessionListener.SESSION_COUNTRY_KEY, locale.getLanguage());
			}
		}
		if (session.getAttribute(SessionListener.SESSION_REMOTE_ADDR) == null) {
			// adresse ip
			final String forwardedFor = httpRequest.getHeader("X-Forwarded-For");
			final String remoteAddr;
			if (forwardedFor == null) {
				remoteAddr = httpRequest.getRemoteAddr();
			} else {
				remoteAddr = httpRequest.getRemoteAddr() + " forwarded for " + forwardedFor;
			}
			session.setAttribute(SessionListener.SESSION_REMOTE_ADDR, remoteAddr);
		}
		if (session.getAttribute(SessionListener.SESSION_REMOTE_USER) == null) {
			// login utilisateur, peut être null
			final String remoteUser = httpRequest.getRemoteUser();
			if (remoteUser != null) {
				session.setAttribute(SessionListener.SESSION_REMOTE_USER, remoteUser);
			}
		}
		if (session.getAttribute(SessionListener.SESSION_USER_AGENT) == null) {
			final String userAgent = httpRequest.getHeader("User-Agent");
			session.setAttribute(SessionListener.SESSION_USER_AGENT, userAgent);
		}
	}

	private void doMonitoring(HttpServletRequest httpRequest, HttpServletResponse httpResponse)
			throws IOException, ServletException {
		if (isRumMonitoring(httpRequest, httpResponse)) {
			return;
		}

		if (!isAllowed(httpRequest, httpResponse)) {
			return;
		}

		final Collector collector = filterContext.getCollector();
		final MonitoringController monitoringController = new MonitoringController(collector, null);
		monitoringController.doActionIfNeededAndReport(httpRequest, httpResponse,
				filterConfig.getServletContext());

		if ("stop".equalsIgnoreCase(HttpParameter.COLLECTOR.getParameterFrom(httpRequest))) {
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

	protected final boolean isRumMonitoring(HttpServletRequest httpRequest,
			HttpServletResponse httpResponse) throws IOException {
		if (rumEnabled) {
			// these 2 ifs must be before isAllowed verification
			if (RumInjector.isRumResource(HttpParameter.RESOURCE.getParameterFrom(httpRequest))) {
				// this is to give the boomerang.min.js content
				MonitoringController.doResource(httpResponse,
						HttpParameter.RESOURCE.getParameterFrom(httpRequest));
				return true;
			} else if (HttpPart.RUM.isPart(httpRequest)) {
				// this is the call by the boomerang beacon to notify RUM data
				MonitoringController.noCache(httpResponse);
				httpResponse.setContentType("image/png");
				RumInjector.addRumHit(httpRequest, httpCounter);
				return true;
			}
		}
		return false;
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
			boolean systemError, int responseStatus, long responseSize) {
		if (!logEnabled) {
			return;
		}
		final String filterName = filterConfig.getFilterName();
		LOG.logHttpRequest(httpRequest, requestName, duration, systemError, responseStatus,
				responseSize, filterName);
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

	/**
	 * Asynchronously calls the optional collect server to register this application's node to be monitored.
	 * @param applicationName Name of the application in the collect server:<br/>
	 *     if it already exists the node will be added with the other nodes, if null name will be "contextPath_hostname".
	 * @param collectServerUrl Url of the collect server,
	 *     for example http://11.22.33.44:8080
	 * @param applicationNodeUrl Url of this application node to be called by the collect server,
	 *     for example http://55.66.77.88:8080/mywebapp
	 */
	public static void registerApplicationNodeInCollectServer(String applicationName,
			URL collectServerUrl, URL applicationNodeUrl) {
		if (collectServerUrl == null || applicationNodeUrl == null) {
			throw new IllegalArgumentException(
					"collectServerUrl and applicationNodeUrl must not be null");
		}
		final String appName;
		if (applicationName == null) {
			appName = Parameters.getCurrentApplication();
		} else {
			appName = applicationName;
		}
		final URL registerUrl;
		try {
			registerUrl = new URL(collectServerUrl.toExternalForm() + "?appName="
					+ URLEncoder.encode(appName, "UTF-8") + "&appUrls="
					// "UTF-8" as said in javadoc
					+ URLEncoder.encode(applicationNodeUrl.toExternalForm(), "UTF-8")
					+ "&action=registerNode");
			unregisterApplicationNodeInCollectServerUrl = new URL(
					registerUrl.toExternalForm().replace("registerNode", "unregisterNode"));
		} catch (final IOException e) {
			// can't happen if urls are ok
			throw new IllegalArgumentException(e);
		}

		// this is an asynchronous call because if this method is called when the webapp is starting,
		// the webapp can not respond to the collect server for the first collect of data
		final Thread thread = new Thread("javamelody registerApplicationNodeInCollectServer") {
			@Override
			public void run() {
				try {
					Thread.sleep(10000);
				} catch (final InterruptedException e) {
					throw new IllegalStateException(e);
				}
				try {
					new LabradorRetriever(registerUrl).post(null);
					LOG.info("application node added to the collect server");
				} catch (final IOException e) {
					LOG.warn("Unable to register application's node in the collect server ( " + e
							+ ')', e);
				}
			}
		};
		thread.setDaemon(true);
		thread.start();
	}

	/**
	 * Call the optional collect server to unregister this application's node.
	 * @throws IOException e
	 */
	public static void unregisterApplicationNodeInCollectServer() throws IOException {
		if (unregisterApplicationNodeInCollectServerUrl != null) {
			new LabradorRetriever(unregisterApplicationNodeInCollectServerUrl).post(null);
			LOG.info("application node removed from the collect server");
		}
	}
}
