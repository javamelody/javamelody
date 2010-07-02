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
import static net.bull.javamelody.HttpParameters.COLLECTOR_PARAMETER;
import static net.bull.javamelody.HttpParameters.CONNECTIONS_PART;
import static net.bull.javamelody.HttpParameters.CURRENT_REQUESTS_PART;
import static net.bull.javamelody.HttpParameters.DATABASE_PART;
import static net.bull.javamelody.HttpParameters.FORMAT_PARAMETER;
import static net.bull.javamelody.HttpParameters.LAST_VALUE_PART;
import static net.bull.javamelody.HttpParameters.PART_PARAMETER;
import static net.bull.javamelody.HttpParameters.PERIOD_PARAMETER;
import static net.bull.javamelody.HttpParameters.POM_XML_PART;
import static net.bull.javamelody.HttpParameters.PROCESSES_PART;
import static net.bull.javamelody.HttpParameters.REQUEST_PARAMETER;
import static net.bull.javamelody.HttpParameters.RESOURCE_PARAMETER;
import static net.bull.javamelody.HttpParameters.SESSIONS_PART;
import static net.bull.javamelody.HttpParameters.SESSION_ID_PARAMETER;
import static net.bull.javamelody.HttpParameters.USAGES_PART;
import static net.bull.javamelody.HttpParameters.WEB_XML_PART;
import static org.easymock.EasyMock.createNiceMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.expectLastCall;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.Locale;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.Timer;

import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.RequestDispatcher;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import net.sf.ehcache.CacheManager;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.Logger;

/**
 * Test unitaire de la classe MonitoringFilter.
 * @author Emeric Vernat
 */
// CHECKSTYLE:OFF
public class TestMonitoringFilter {
	private static final String FILTER_NAME = "monitoring";
	// CHECKSTYLE:ON
	// identique à HttpCookieManager.PERIOD_COOKIE_NAME
	private static final String PERIOD_COOKIE_NAME = "javamelody.period";
	private static final String REMOTE_ADDR = "127.0.0.1"; // NOPMD
	private static final String CONTEXT_PATH = "/test";
	private static final String GRAPH = "graph";
	private static final String TRUE = "true";
	private FilterConfig config;
	private ServletContext context;
	private MonitoringFilter monitoringFilter;

	/**
	 * Initialisation.
	 */
	@Before
	public void setUpFirst() {
		Utils.initialize();
	}

	/**
	 * Initialisation.
	 */
	@Before
	public void setUp() {
		// rq: pas setUpFirst ici car setUp est rappelée dans les méthodes
		tearDown();
		config = createNiceMock(FilterConfig.class);
		context = createNiceMock(ServletContext.class);
		expect(config.getServletContext()).andReturn(context).anyTimes();
		expect(config.getFilterName()).andReturn(FILTER_NAME).anyTimes();
		// anyTimes sur getInitParameter car TestJdbcDriver a pu fixer la propriété système à false
		expect(
				context.getInitParameter(Parameters.PARAMETER_SYSTEM_PREFIX
						+ Parameter.DISABLED.getCode())).andReturn(null).anyTimes();
		expect(config.getInitParameter(Parameter.DISABLED.getCode())).andReturn(null).anyTimes();
		expect(context.getMajorVersion()).andReturn(2).anyTimes();
		expect(context.getMinorVersion()).andReturn(5).anyTimes();
		expect(context.getServletContextName()).andReturn("test webapp").anyTimes();
		// mockJetty pour avoir un applicationServerIconName dans JavaInformations
		expect(context.getServerInfo()).andReturn("mockJetty").anyTimes();
		// dependencies pour avoir des dépendances dans JavaInformations
		final Set<String> dependencies = new LinkedHashSet<String>(Arrays.asList(
				"/WEB-INF/lib/jrobin.jar", "/WEB-INF/lib/javamelody.jar"));
		expect(context.getResourcePaths("/WEB-INF/lib/")).andReturn(dependencies).anyTimes();
		expect(context.getContextPath()).andReturn(CONTEXT_PATH).anyTimes();
		monitoringFilter = new MonitoringFilter();
	}

	/**
	 * Finalisation.
	 */
	@After
	public void tearDown() {
		destroy();
	}

	private void destroy() {
		// on désactive le stop sur le timer JRobin car sinon les tests suivants ne fonctionneront
		// plus si ils utilisent JRobin
		Utils.setProperty(Parameters.PARAMETER_SYSTEM_PREFIX + "jrobinStopDisabled", TRUE);
		if (monitoringFilter != null) {
			monitoringFilter.destroy();
		}
	}

	/** Test.
	 * @throws ServletException e */
	@Test
	public void testInit() throws ServletException {
		try {
			init();
			setUp();
			expect(config.getInitParameter(Parameter.DISPLAYED_COUNTERS.getCode())).andReturn(
					"http,sql").anyTimes();
			expect(config.getInitParameter(Parameter.HTTP_TRANSFORM_PATTERN.getCode())).andReturn(
					"[0-9]").anyTimes();
			init();
			setUp();
			expect(config.getInitParameter(Parameter.URL_EXCLUDE_PATTERN.getCode())).andReturn(
					"/static/*").anyTimes();
			init();
			setUp();
			expect(config.getInitParameter(Parameter.ALLOWED_ADDR_PATTERN.getCode())).andReturn(
					"127\\.0\\.0\\.1").anyTimes();
			init();
		} finally {
			destroy();
		}
	}

	private void init() throws ServletException {
		replay(config);
		replay(context);
		monitoringFilter.init(config);
		verify(config);
		verify(context);
	}

	/** Test.
	 * @throws ServletException e */
	@Test
	public void testLog() throws ServletException {
		try {
			final HttpServletRequest request = createNiceMock(HttpServletRequest.class);
			expect(request.getRemoteAddr()).andReturn(REMOTE_ADDR);
			expect(request.getRequestURI()).andReturn("/test/request");
			expect(request.getContextPath()).andReturn(CONTEXT_PATH);
			expect(request.getQueryString()).andReturn("param1=1");
			expect(request.getMethod()).andReturn("GET");

			replay(config);
			replay(context);
			monitoringFilter.init(config);
			monitoringFilter.log(request, "test", 1000, false, 10000);
			verify(config);
			verify(context);
		} finally {
			destroy();
		}
	}

	/** Test.
	 * @throws ServletException e
	 * @throws IOException e */
	@Test
	public void testDoFilterNoHttp() throws ServletException, IOException {
		try {
			final FilterChain servletChain = createNiceMock(FilterChain.class);
			final ServletRequest servletRequest = createNiceMock(ServletRequest.class);
			final ServletResponse servletResponse = createNiceMock(ServletResponse.class);
			replay(servletRequest);
			replay(servletResponse);
			replay(servletChain);
			monitoringFilter.doFilter(servletRequest, servletResponse, servletChain);
			verify(servletRequest);
			verify(servletResponse);
			verify(servletChain);

			final FilterChain servletChain2 = createNiceMock(FilterChain.class);
			final HttpServletRequest servletRequest2 = createNiceMock(HttpServletRequest.class);
			final ServletResponse servletResponse2 = createNiceMock(ServletResponse.class);
			replay(servletRequest2);
			replay(servletResponse2);
			replay(servletChain2);
			monitoringFilter.doFilter(servletRequest2, servletResponse2, servletChain2);
			verify(servletRequest2);
			verify(servletResponse2);
			verify(servletChain2);
		} finally {
			destroy();
		}
	}

	/** Test.
	 * @throws ServletException e
	 * @throws IOException e */
	@Test
	public void testDoFilter() throws ServletException, IOException {
		// displayed-counters
		setProperty(Parameter.DISPLAYED_COUNTERS, "sql");
		try {
			doFilter(createNiceMock(HttpServletRequest.class));
			setProperty(Parameter.DISPLAYED_COUNTERS, "unknown");
			try {
				doFilter(createNiceMock(HttpServletRequest.class));
			} catch (final IllegalArgumentException e) {
				assertNotNull("ok", e);
			}
		} finally {
			setProperty(Parameter.DISPLAYED_COUNTERS, null);
		}

		// standard
		doFilter(createNiceMock(HttpServletRequest.class));

		// log
		setProperty(Parameter.LOG, "true");
		try {
			((Logger) org.slf4j.LoggerFactory.getLogger(FILTER_NAME)).setLevel(Level.WARN);
			doFilter(createNiceMock(HttpServletRequest.class));

			((Logger) org.slf4j.LoggerFactory.getLogger(FILTER_NAME)).setLevel(Level.DEBUG);
			doFilter(createNiceMock(HttpServletRequest.class));

			final HttpServletRequest request = createNiceMock(HttpServletRequest.class);
			expect(request.getHeader("X-Forwarded-For")).andReturn("me").anyTimes();
			expect(request.getQueryString()).andReturn("param1=1").anyTimes();
			doFilter(request);
		} finally {
			setProperty(Parameter.LOG, null);
		}

		// ajax
		final HttpServletRequest request = createNiceMock(HttpServletRequest.class);
		expect(request.getHeader("X-Requested-With")).andReturn("XMLHttpRequest");
		doFilter(request);

		// erreur système http, avec log
		setProperty(Parameter.LOG, "true");
		try {
			final String test = "test";
			doFilter(createNiceMock(HttpServletRequest.class), new UnknownError(test));
			doFilter(createNiceMock(HttpServletRequest.class), new IllegalStateException(test));
			// pas possibles:
			//			doFilter(createNiceMock(HttpServletRequest.class), new IOException(test));
			//			doFilter(createNiceMock(HttpServletRequest.class), new ServletException(test));
			//			doFilter(createNiceMock(HttpServletRequest.class), new Exception(test));
		} finally {
			setProperty(Parameter.LOG, null);
		}
	}

	/** Test.
	 * @throws ServletException e
	 * @throws IOException e */
	@Test
	public void testDoFilterWithSession() throws ServletException, IOException {
		final HttpServletRequest request = createNiceMock(HttpServletRequest.class);
		final HttpSession session = createNiceMock(HttpSession.class);
		expect(request.getSession(false)).andReturn(session);
		expect(request.getLocale()).andReturn(Locale.FRANCE);
		replay(session);
		doFilter(request);
		verify(session);
	}

	/** Test.
	 * @throws ServletException e
	 * @throws IOException e */
	@Test
	public void testDoFilterWithSessionBis() throws ServletException, IOException {
		final HttpServletRequest request = createNiceMock(HttpServletRequest.class);
		final HttpSession session = createNiceMock(HttpSession.class);
		expect(request.getSession(false)).andReturn(session);
		// Locale sans pays
		expect(request.getLocale()).andReturn(Locale.FRENCH).anyTimes();
		// "X-Forwarded-For"
		expect(request.getHeader("X-Forwarded-For")).andReturn("somewhere").anyTimes();
		// getRemoteUser
		expect(request.getRemoteUser()).andReturn("me").anyTimes();
		replay(session);
		doFilter(request);
		verify(session);
	}

	/** Test.
	 * @throws ServletException e
	 * @throws IOException e */
	@Test
	public void testDoFilterWithSessionTer() throws ServletException, IOException {
		final HttpServletRequest request = createNiceMock(HttpServletRequest.class);
		final HttpSession session = createNiceMock(HttpSession.class);
		expect(request.getSession(false)).andReturn(session);
		expect(session.getAttribute(SessionInformations.SESSION_COUNTRY_KEY)).andReturn(
				Locale.FRANCE.getCountry()).anyTimes();
		expect(session.getAttribute(SessionInformations.SESSION_REMOTE_ADDR))
				.andReturn("somewhere").anyTimes();
		expect(session.getAttribute(SessionInformations.SESSION_REMOTE_USER)).andReturn("me")
				.anyTimes();
		replay(session);
		doFilter(request);
		verify(session);
	}

	private void doFilter(HttpServletRequest request) throws ServletException, IOException {
		doFilter(request, null);
	}

	private void doFilter(HttpServletRequest request, Throwable exceptionInDoFilter)
			throws ServletException, IOException {
		setUp();

		try {
			final FilterChain chain = createNiceMock(FilterChain.class);
			expect(request.getRequestURI()).andReturn("/test/request").anyTimes();
			expect(request.getContextPath()).andReturn(CONTEXT_PATH).anyTimes();
			if (exceptionInDoFilter != null) {
				// cela fera une erreur système http comptée dans les stats
				expect(request.getMethod()).andThrow(exceptionInDoFilter);
			} else {
				expect(request.getMethod()).andReturn("GET").anyTimes();
			}
			final HttpServletResponse response = createNiceMock(HttpServletResponse.class);

			replay(config);
			replay(context);
			replay(request);
			replay(response);
			replay(chain);
			monitoringFilter.init(config);
			if (exceptionInDoFilter != null) {
				try {
					monitoringFilter.doFilter(request, response, chain);
				} catch (final Throwable t) { // NOPMD
					assertNotNull("ok", t);
				}
			} else {
				monitoringFilter.doFilter(request, response, chain);
			}
			verify(config);
			verify(context);
			verify(request);
			verify(response);
			verify(chain);
		} finally {
			destroy();
		}
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testFilterServletResponseWrapper() throws IOException {
		try {
			final HttpServletResponse response = createNiceMock(HttpServletResponse.class);
			expect(response.getOutputStream()).andReturn(
					new FilterServletOutputStream(new ByteArrayOutputStream())).anyTimes();
			expect(response.getCharacterEncoding()).andReturn("ISO-8859-1").anyTimes();
			final CounterServletResponseWrapper wrappedResponse = new CounterServletResponseWrapper(
					response);
			replay(response);
			assertNotNull("getOutputStream", wrappedResponse.getOutputStream());
			assertNotNull("getOutputStream bis", wrappedResponse.getOutputStream());
			assertNotNull("getOutputStream", wrappedResponse.getCharacterEncoding());
			wrappedResponse.close();
			verify(response);

			final HttpServletResponse response2 = createNiceMock(HttpServletResponse.class);
			expect(response2.getOutputStream()).andReturn(
					new FilterServletOutputStream(new ByteArrayOutputStream())).anyTimes();
			expect(response2.getCharacterEncoding()).andReturn(null).anyTimes();
			final CounterServletResponseWrapper wrappedResponse2 = new CounterServletResponseWrapper(
					response);
			replay(response2);
			assertNotNull("getWriter", wrappedResponse2.getWriter());
			assertNotNull("getWriter bis", wrappedResponse2.getWriter());
			wrappedResponse2.close();
			verify(response2);
		} finally {
			destroy();
		}
	}

	/** Test.
	 * @throws ServletException e
	 * @throws IOException e */
	@Test
	public void testDoMonitoring() throws ServletException, IOException {
		monitoring(Collections.<String, String> emptyMap());
		monitoring(Collections.<String, String> singletonMap(FORMAT_PARAMETER, "html"));
		monitoring(Collections.<String, String> singletonMap(FORMAT_PARAMETER, "htmlbody"));
		setProperty(Parameter.DISABLED, Boolean.TRUE.toString());
		try {
			monitoring(Collections.<String, String> emptyMap(), false);
		} finally {
			setProperty(Parameter.DISABLED, Boolean.FALSE.toString());
		}
		setProperty(Parameter.NO_DATABASE, Boolean.TRUE.toString());
		try {
			monitoring(Collections.<String, String> emptyMap());
		} finally {
			setProperty(Parameter.NO_DATABASE, Boolean.FALSE.toString());
		}
		setProperty(Parameter.URL_EXCLUDE_PATTERN, ".*");
		try {
			monitoring(Collections.<String, String> emptyMap(), false);
		} finally {
			setProperty(Parameter.URL_EXCLUDE_PATTERN, "");
		}
		setProperty(Parameter.ALLOWED_ADDR_PATTERN, "256.*");
		try {
			monitoring(Collections.<String, String> emptyMap(), false);
			setProperty(Parameter.ALLOWED_ADDR_PATTERN, ".*");
			monitoring(Collections.<String, String> emptyMap(), false);
		} finally {
			setProperty(Parameter.ALLOWED_ADDR_PATTERN, null);
		}
		setProperty(Parameter.MONITORING_PATH, "/admin/monitoring");
		try {
			monitoring(Collections.<String, String> emptyMap(), false);
		} finally {
			setProperty(Parameter.MONITORING_PATH, "/monitoring");
		}
		setProperty(Parameter.MAIL_SESSION, "testmailsession");
		setProperty(Parameter.ADMIN_EMAILS, null);
		monitoring(Collections.<String, String> emptyMap());
		setProperty(Parameter.ADMIN_EMAILS, "evernat@free.fr");
		monitoring(Collections.<String, String> emptyMap());
	}

	/** Test.
	 * @throws ServletException e
	 * @throws IOException e */
	@Test
	public void testDoMonitoringWithPeriod() throws ServletException, IOException {
		monitoring(Collections.<String, String> singletonMap(PERIOD_PARAMETER,
				Period.JOUR.getCode()));
	}

	/** Test.
	 * @throws ServletException e
	 * @throws IOException e */
	@Test
	public void testDoMonitoringWithResource() throws ServletException, IOException {
		monitoring(Collections.<String, String> singletonMap(RESOURCE_PARAMETER, "monitoring.css"));
	}

	/** Test.
	 * @throws ServletException e
	 * @throws IOException e */
	@Test
	public void testDoMonitoringWithGraph() throws ServletException, IOException {
		final Map<String, String> parameters = new HashMap<String, String>();
		parameters.put(GRAPH, "usedMemory");
		parameters.put("width", "800");
		parameters.put("height", "600");
		monitoring(parameters);
		parameters.put(GRAPH, "unknown");
		monitoring(parameters, false);
	}

	/** Test.
	 * @throws ServletException e
	 * @throws IOException e */
	@Test
	public void testDoMonitoringWithParts() throws ServletException, IOException {
		final Map<String, String> parameters = new HashMap<String, String>();

		parameters.put(PART_PARAMETER, CURRENT_REQUESTS_PART);
		monitoring(parameters);

		setProperty(Parameter.SYSTEM_ACTIONS_ENABLED, TRUE);
		parameters.put(PART_PARAMETER, PROCESSES_PART);
		monitoring(parameters);
		monitorJdbcParts(parameters);
		parameters.remove(FORMAT_PARAMETER);
		parameters.put(REQUEST_PARAMETER, "0");
		monitoring(parameters);
		parameters.remove(REQUEST_PARAMETER);
		// il ne faut pas faire un heapHisto sans thread comme dans TestHtmlHeapHistogramReport
		//		parameters.put(PART_PARAMETER, HEAP_HISTO_PART);
		//		monitoring(parameters);
		parameters.put(PART_PARAMETER, SESSIONS_PART);
		monitoring(parameters);
		parameters.put(PART_PARAMETER, SESSIONS_PART);
		parameters.put(SESSION_ID_PARAMETER, "expired session");
		monitoring(parameters);
		parameters.remove(SESSION_ID_PARAMETER);
		parameters.put(PART_PARAMETER, WEB_XML_PART);
		monitoring(parameters, false);
		parameters.put(PART_PARAMETER, POM_XML_PART);
		monitoring(parameters, false);

		parameters.put(PART_PARAMETER, GRAPH);
		parameters.put(GRAPH, "usedMemory");
		monitoring(parameters);

		parameters.put(PART_PARAMETER, LAST_VALUE_PART);
		parameters.put(GRAPH, "usedMemory,cpu,unknown");
		monitoring(parameters);

		parameters.put(PART_PARAMETER, USAGES_PART);
		parameters.put(GRAPH, "unknown");
		monitoring(parameters);

		parameters.put(PART_PARAMETER, "unknown part");
		boolean exception = false;
		try {
			monitoring(parameters);
		} catch (final IllegalArgumentException e) {
			exception = true;
		}
		assertTrue("exception if unknown part", exception);
	}

	private void monitorJdbcParts(Map<String, String> parameters) throws IOException,
			ServletException {
		final Connection connection = TestDatabaseInformations.initH2();
		try {
			parameters.put(PART_PARAMETER, DATABASE_PART);
			monitoring(parameters);
			parameters.put(PART_PARAMETER, DATABASE_PART);
			parameters.put(REQUEST_PARAMETER, "0");
			monitoring(parameters);
			parameters.put(PART_PARAMETER, CONNECTIONS_PART);
			monitoring(parameters);
			parameters.put(PART_PARAMETER, CONNECTIONS_PART);
			parameters.put(FORMAT_PARAMETER, "htmlbody");
			monitoring(parameters);
		} finally {
			try {
				connection.close();
			} catch (final SQLException e) {
				Collector.printStackTrace(e);
			}
		}
	}

	/** Test.
	 * @throws ServletException e
	 * @throws IOException e */
	@Test
	public void testDoMonitoringWithActions() throws ServletException, IOException {
		final Map<String, String> parameters = new HashMap<String, String>();

		setProperty(Parameter.SYSTEM_ACTIONS_ENABLED, TRUE);
		parameters.put(ACTION_PARAMETER, Action.GC.toString());
		monitoring(parameters);
		parameters.put(ACTION_PARAMETER, Action.INVALIDATE_SESSIONS.toString());
		monitoring(parameters);
		parameters.put(ACTION_PARAMETER, Action.INVALIDATE_SESSION.toString());
		parameters.put(SESSION_ID_PARAMETER, "123456789");
		monitoring(parameters);
		parameters.put(ACTION_PARAMETER, Action.CLEAR_CACHES.toString());
		monitoring(parameters);
		if (CacheManager.getInstance().getCache("test clear") == null) {
			CacheManager.getInstance().addCache("test clear");
		}
		monitoring(parameters);
		parameters.put(ACTION_PARAMETER, Action.PAUSE_JOB.toString());
		parameters.put("jobId", "all");
		monitoring(parameters);
		parameters.put(ACTION_PARAMETER, Action.RESUME_JOB.toString());
		monitoring(parameters);
		parameters.put(ACTION_PARAMETER, Action.CLEAR_COUNTER.toString());
		parameters.put("counter", "all");
		monitoring(parameters);
	}

	/** Test.
	 * @throws ServletException e
	 * @throws IOException e */
	@Test
	public void testDoMonitoringWithFormatPdf() throws ServletException, IOException {
		monitoring(Collections.<String, String> singletonMap(FORMAT_PARAMETER, "pdf"));
	}

	/** Test.
	 * @throws ServletException e
	 * @throws IOException e */
	@Test
	public void testDoMonitoringWithFormatSerialized() throws ServletException, IOException {
		setProperty(Parameter.SYSTEM_ACTIONS_ENABLED, TRUE);
		final Map<String, String> parameters = new HashMap<String, String>();
		parameters.put(FORMAT_PARAMETER, TransportFormat.SERIALIZED.getCode());
		monitoring(parameters);
		parameters.put(PART_PARAMETER, SESSIONS_PART);
		monitoring(parameters);
		parameters.put(PART_PARAMETER, SESSIONS_PART);
		parameters.put(SESSION_ID_PARAMETER, "expired session");
		monitoring(parameters);
		parameters.remove(SESSION_ID_PARAMETER);
		parameters.put(PART_PARAMETER, PROCESSES_PART);
		monitoring(parameters);
		parameters.put(PART_PARAMETER, DATABASE_PART);
		monitoring(parameters);
		parameters.put(REQUEST_PARAMETER, "0");
		monitoring(parameters);
		parameters.remove(REQUEST_PARAMETER);
		parameters.put(PART_PARAMETER, CONNECTIONS_PART);
		monitoring(parameters);
		// il ne faut pas faire un heapHisto sans thread comme dans TestHtmlHeapHistogramReport
		//		parameters.put(PART_PARAMETER, HEAP_HISTO_PART);
		//		monitoring(parameters);
		parameters.put(PART_PARAMETER, null);
		parameters.put(FORMAT_PARAMETER, TransportFormat.SERIALIZED.getCode());
		parameters.put(COLLECTOR_PARAMETER, "stop");
		monitoring(parameters);
	}

	/** Test.
	 * @throws ServletException e
	 * @throws IOException e */
	@Test
	public void testDoMonitoringWithFormatXml() throws ServletException, IOException {
		setProperty(Parameter.SYSTEM_ACTIONS_ENABLED, TRUE);
		final Map<String, String> parameters = new HashMap<String, String>();
		parameters.put(FORMAT_PARAMETER, TransportFormat.XML.getCode());
		monitoring(parameters);
		parameters.put(PART_PARAMETER, SESSIONS_PART);
		monitoring(parameters);
		parameters.put(PART_PARAMETER, PROCESSES_PART);
		monitoring(parameters);
		parameters.put(PART_PARAMETER, DATABASE_PART);
		monitoring(parameters);
		parameters.put(PART_PARAMETER, CONNECTIONS_PART);
		monitoring(parameters);
		// il ne faut pas faire un heapHisto sans thread comme dans TestHtmlHeapHistogramReport
		//		parameters.put(PART_PARAMETER, HEAP_HISTO_PART);
		//		monitoring(parameters);
	}

	/** Test.
	 * @throws ServletException e
	 * @throws IOException e */
	@Test
	public void testDoMonitoringWithFormatJson() throws ServletException, IOException {
		setProperty(Parameter.SYSTEM_ACTIONS_ENABLED, TRUE);
		final Map<String, String> parameters = new HashMap<String, String>();
		parameters.put(FORMAT_PARAMETER, TransportFormat.JSON.getCode());
		monitoring(parameters);
		parameters.put(PART_PARAMETER, SESSIONS_PART);
		monitoring(parameters);
		parameters.put(PART_PARAMETER, PROCESSES_PART);
		monitoring(parameters);
		parameters.put(PART_PARAMETER, DATABASE_PART);
		monitoring(parameters);
		parameters.put(PART_PARAMETER, CONNECTIONS_PART);
		monitoring(parameters);
		// il ne faut pas faire un heapHisto sans thread comme dans TestHtmlHeapHistogramReport
		//		parameters.put(PART_PARAMETER, HEAP_HISTO_PART);
		//		monitoring(parameters);
	}

	private void monitoring(Map<String, String> parameters) throws IOException, ServletException {
		monitoring(parameters, true);
	}

	private void monitoring(Map<String, String> parameters, boolean checkResultContent)
			throws IOException, ServletException {
		setUp();

		try {
			final HttpServletRequest request = createNiceMock(HttpServletRequest.class);
			expect(request.getRequestURI()).andReturn("/test/monitoring").anyTimes();
			expect(request.getContextPath()).andReturn(CONTEXT_PATH).anyTimes();
			expect(request.getRemoteAddr()).andReturn("here").anyTimes();
			final Random random = new Random();
			if (random.nextBoolean()) {
				expect(request.getHeaders("Accept-Encoding")).andReturn(
						Collections.enumeration(Arrays.asList("application/gzip"))).anyTimes();
			} else {
				expect(request.getHeaders("Accept-Encoding")).andReturn(
						Collections.enumeration(Arrays.asList("text/html"))).anyTimes();
			}
			for (final Map.Entry<String, String> entry : parameters.entrySet()) {
				expect(request.getParameter(entry.getKey())).andReturn(entry.getValue()).anyTimes();
			}
			if (parameters.isEmpty()) {
				// dans au moins un cas on met un cookie
				final Cookie[] cookies = { new Cookie("dummy", "dummy"),
						new Cookie(PERIOD_COOKIE_NAME, Period.SEMAINE.getCode()), };
				expect(request.getCookies()).andReturn(cookies).anyTimes();
			}
			final HttpServletResponse response = createNiceMock(HttpServletResponse.class);
			final ByteArrayOutputStream output = new ByteArrayOutputStream();
			expect(response.getOutputStream()).andReturn(new FilterServletOutputStream(output))
					.anyTimes();
			final StringWriter stringWriter = new StringWriter();
			expect(response.getWriter()).andReturn(new PrintWriter(stringWriter)).anyTimes();
			final FilterChain chain = createNiceMock(FilterChain.class);

			replay(config);
			replay(context);
			replay(request);
			replay(response);
			replay(chain);
			monitoringFilter.init(config);
			monitoringFilter.doFilter(request, response, chain);
			verify(config);
			verify(context);
			verify(request);
			verify(response);
			verify(chain);

			if (checkResultContent) {
				assertTrue("result", output.size() != 0 || stringWriter.getBuffer().length() != 0);
			}
		} finally {
			destroy();
		}
	}

	/** Test.
	 * @throws ServletException e */
	@Test
	public void testWriteHtmlToLastShutdownFile() throws ServletException {
		try {
			replay(config);
			replay(context);
			monitoringFilter.init(config);
			final Timer timer = new Timer("test timer", true);
			try {
				final Counter sqlCounter = new Counter("sql", "db.png");
				final Collector collector = new Collector("test", Arrays.asList(sqlCounter), timer);
				timer.cancel();
				new MonitoringController(collector, null).writeHtmlToLastShutdownFile();
				verify(config);
				verify(context);
			} finally {
				timer.cancel();
			}
		} finally {
			destroy();
		}
	}

	/** Test.
	 * @throws ServletException e
	 * @throws IOException e */
	@Test
	public void testJiraMonitoringFilter() throws ServletException, IOException {
		final JiraMonitoringFilter jiraMonitoringFilter = new JiraMonitoringFilter();
		try {
			final FilterChain servletChain = createNiceMock(FilterChain.class);
			final ServletRequest servletRequest = createNiceMock(ServletRequest.class);
			final ServletResponse servletResponse = createNiceMock(ServletResponse.class);
			replay(servletRequest);
			replay(servletResponse);
			replay(servletChain);
			jiraMonitoringFilter.doFilter(servletRequest, servletResponse, servletChain);
			verify(servletRequest);
			verify(servletResponse);
			verify(servletChain);

			final HttpServletRequest request = createNiceMock(HttpServletRequest.class);
			expect(request.getRequestURI()).andReturn("/test/monitoring").anyTimes();
			expect(request.getContextPath()).andReturn(CONTEXT_PATH).anyTimes();
			expect(request.getHeaders("Accept-Encoding")).andReturn(
					Collections.enumeration(Arrays.asList("text/html"))).anyTimes();
			final HttpServletResponse response = createNiceMock(HttpServletResponse.class);
			final ByteArrayOutputStream output = new ByteArrayOutputStream();
			expect(response.getOutputStream()).andReturn(new FilterServletOutputStream(output))
					.anyTimes();
			final StringWriter stringWriter = new StringWriter();
			expect(response.getWriter()).andReturn(new PrintWriter(stringWriter)).anyTimes();
			final FilterChain chain = createNiceMock(FilterChain.class);

			replay(config);
			replay(context);
			replay(request);
			replay(response);
			replay(chain);
			jiraMonitoringFilter.init(config);
			jiraMonitoringFilter.doFilter(request, response, chain);
			verify(config);
			verify(context);
			verify(request);
			verify(response);
			verify(chain);
		} finally {
			jiraMonitoringFilter.destroy();
			destroy();
		}
	}

	/** Test.
	 * @throws IOException e
	 * @throws ServletException e */
	@Test
	public void testJspWrapper() throws ServletException, IOException {
		assertNotNull("getJspCounter", JspWrapper.getJspCounter());

		try {
			final HttpServletRequest request = createNiceMock(HttpServletRequest.class);
			final HttpServletResponse response = createNiceMock(HttpServletResponse.class);
			final RequestDispatcher requestDispatcher = createNiceMock(RequestDispatcher.class);
			final RequestDispatcher requestDispatcherWithError = createNiceMock(RequestDispatcher.class);
			final RequestDispatcher requestDispatcherWithException = createNiceMock(RequestDispatcher.class);
			final String url1 = "test.jsp";
			final String url2 = "test.jsp?param=test2";
			final String url3 = "test.jsp?param=test3";
			final String url4 = null;
			expect(request.getRequestDispatcher(url1)).andReturn(requestDispatcher);
			expect(request.getRequestDispatcher(url2)).andReturn(requestDispatcherWithError);
			requestDispatcherWithError.forward(request, response);
			expectLastCall().andThrow(new UnknownError("erreur dans forward"));
			expect(request.getRequestDispatcher(url3)).andReturn(requestDispatcherWithException);
			requestDispatcherWithException.forward(request, response);
			expectLastCall().andThrow(new IllegalStateException("erreur dans forward"));
			expect(request.getRequestDispatcher(url4)).andReturn(null);
			final HttpServletRequest wrappedRequest = JspWrapper.createHttpRequestWrapper(request);

			replay(request);
			replay(response);
			replay(requestDispatcher);
			replay(requestDispatcherWithError);
			replay(requestDispatcherWithException);
			final RequestDispatcher wrappedRequestDispatcher = wrappedRequest
					.getRequestDispatcher(url1);
			wrappedRequestDispatcher.toString();
			wrappedRequestDispatcher.include(wrappedRequest, response);
			final RequestDispatcher wrappedRequestDispatcher2 = wrappedRequest
					.getRequestDispatcher(url2);
			try {
				wrappedRequestDispatcher2.forward(request, response);
			} catch (final UnknownError e) {
				assertNotNull("ok", e);
			}
			final RequestDispatcher wrappedRequestDispatcher3 = wrappedRequest
					.getRequestDispatcher(url3);
			try {
				wrappedRequestDispatcher3.forward(request, response);
			} catch (final IllegalStateException e) {
				assertNotNull("ok", e);
			}
			final RequestDispatcher wrappedRequestDispatcher4 = wrappedRequest
					.getRequestDispatcher(url4);
			assertNull("getRequestDispatcher(null)", wrappedRequestDispatcher4);
			verify(request);
			verify(response);
			verify(requestDispatcher);
			// verify ne marche pas ici car on fait une Error, verify(requestDispatcherWithError);
			verify(requestDispatcherWithException);
		} finally {
			destroy();
		}
	}

	private static void setProperty(Parameter parameter, String value) {
		Utils.setProperty(parameter, value);
	}
}
