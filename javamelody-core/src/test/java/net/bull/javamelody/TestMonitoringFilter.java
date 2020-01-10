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
package net.bull.javamelody; // NOPMD

import static org.easymock.EasyMock.createNiceMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringReader;
import java.io.StringWriter;
import java.lang.reflect.Field;
import java.net.MalformedURLException;
import java.net.URL;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Random;
import java.util.Set;

import javax.cache.Caching;
import javax.cache.configuration.MutableConfiguration;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ReadListener;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.ServletInputStream;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;

import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.Logger;
import net.bull.javamelody.internal.common.HttpParameter;
import net.bull.javamelody.internal.common.HttpPart;
import net.bull.javamelody.internal.common.LOG;
import net.bull.javamelody.internal.common.Parameters;
import net.bull.javamelody.internal.model.Action;
import net.bull.javamelody.internal.model.Counter;
import net.bull.javamelody.internal.model.CounterRequest;
import net.bull.javamelody.internal.model.JavaInformations;
import net.bull.javamelody.internal.model.Period;
import net.bull.javamelody.internal.model.Range;
import net.bull.javamelody.internal.model.TestDatabaseInformations;
import net.bull.javamelody.internal.model.TransportFormat;
import net.bull.javamelody.internal.web.CounterServletResponseWrapper;
import net.bull.javamelody.internal.web.FilterServletOutputStream;
import net.bull.javamelody.internal.web.HttpCookieManager;
import net.sf.ehcache.CacheManager;
import net.sf.ehcache.Element;

/**
 * Test unitaire de la classe MonitoringFilter.
 * @author Emeric Vernat
 */
// CHECKSTYLE:OFF
public class TestMonitoringFilter {// NOPMD
	// CHECKSTYLE:ON
	private static final String FILTER_NAME = "monitoring";
	// identique à HttpCookieManager.PERIOD_COOKIE_NAME
	private static final String PERIOD_COOKIE_NAME = "javamelody.period";
	private static final String REMOTE_ADDR = "127.0.0.1"; // NOPMD
	private static final String TEST_REQUEST = "/request";
	private static final String CONTEXT_PATH = "/test";
	private static final String TRUE = "true";
	private MonitoringFilter monitoringFilter;

	/**
	 * Initialisation (deux Before ne garantissent pas l'ordre dans Eclipse).
	 */
	public TestMonitoringFilter() {
		super();
		Utils.initialize();
	}

	/**
	 * Initialisation.
	 * @throws ServletException e
	 */
	@Before
	public void setUp() throws ServletException {
		tearDown();
		try {
			final Field field = MonitoringFilter.class.getDeclaredField("instanceCreated");
			field.setAccessible(true);
			field.set(null, false);
		} catch (final IllegalAccessException e) {
			throw new IllegalStateException(e);
		} catch (final NoSuchFieldException e) {
			throw new IllegalStateException(e);
		}
		final FilterConfig config = createNiceMock(FilterConfig.class);
		final ServletContext context = createNiceMock(ServletContext.class);
		expect(config.getServletContext()).andReturn(context).anyTimes();
		expect(config.getFilterName()).andReturn(FILTER_NAME).anyTimes();
		// anyTimes sur getInitParameter car TestJdbcDriver a pu fixer la propriété système à false
		expect(context.getInitParameter(
				Parameters.PARAMETER_SYSTEM_PREFIX + Parameter.DISABLED.getCode())).andReturn(null)
						.anyTimes();
		expect(config.getInitParameter(Parameter.DISABLED.getCode())).andReturn(null).anyTimes();
		expect(context.getMajorVersion()).andReturn(2).anyTimes();
		expect(context.getMinorVersion()).andReturn(5).anyTimes();
		expect(context.getServletContextName()).andReturn("test webapp").anyTimes();
		// mockJetty pour avoir un applicationServerIconName dans JavaInformations
		expect(context.getServerInfo()).andReturn("mockJetty").anyTimes();
		// dependencies pour avoir des dépendances dans JavaInformations
		final Set<String> dependencies = new LinkedHashSet<String>(
				Arrays.asList("/WEB-INF/lib/jrobin.jar", "/WEB-INF/lib/javamelody.jar"));
		// et flags pour considérer que les ressources pom.xml et web.xml existent
		JavaInformations.setWebXmlExistsAndPomXmlExists(true, true);
		expect(context.getResourcePaths("/WEB-INF/lib/")).andReturn(dependencies).anyTimes();
		expect(context.getContextPath()).andReturn(CONTEXT_PATH).anyTimes();
		monitoringFilter = new MonitoringFilter();
		monitoringFilter.setApplicationType("Test");
		replay(config);
		replay(context);
		monitoringFilter.init(config);
		verify(config);
		verify(context);
	}

	@After
	public void tearDown() {
		if (monitoringFilter != null) {
			monitoringFilter.destroy();
			monitoringFilter = null;
		}
	}

	/** Test.
	 * @throws ServletException e */
	@Test
	public void testLog() throws ServletException {
		final HttpServletRequest request = createNiceMock(HttpServletRequest.class);
		expect(request.getRemoteAddr()).andReturn(REMOTE_ADDR);
		expect(request.getRequestURI()).andReturn(CONTEXT_PATH + TEST_REQUEST);
		expect(request.getContextPath()).andReturn(CONTEXT_PATH);
		expect(request.getQueryString()).andReturn("param1=1");
		expect(request.getMethod()).andReturn("GET");

		setProperty(Parameter.LOG, TRUE);

		setUp();

		replay(request);
		monitoringFilter.log(request, "test", 1000, false, 200, 10000);
		verify(request);
	}

	/** Test.
	 * @throws ServletException e
	 * @throws IOException e */
	@Test
	public void testDoFilterNoHttp() throws ServletException, IOException {
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
	}

	/** Test.
	 * @throws ServletException e
	 * @throws IOException e */
	@Test
	public void testDoFilter() throws ServletException, IOException {
		// displayed-counters
		setProperty(Parameter.DISPLAYED_COUNTERS, "sql");
		try {
			setUp();
			HttpServletRequest request = createNiceMock(HttpServletRequest.class);
			doFilter(request);
			setProperty(Parameter.DISPLAYED_COUNTERS, "");
			setUp();
			request = createNiceMock(HttpServletRequest.class);
			doFilter(request);
			setProperty(Parameter.DISPLAYED_COUNTERS, "unknown");
			try {
				setUp();
				request = createNiceMock(HttpServletRequest.class);
				doFilter(request);
			} catch (final IllegalArgumentException e) {
				assertNotNull("ok", e);
			}
		} finally {
			setProperty(Parameter.DISPLAYED_COUNTERS, null);
		}

		// url exclue
		setProperty(Parameter.URL_EXCLUDE_PATTERN, ".*");
		try {
			setUp();
			final HttpServletRequest request = createNiceMock(HttpServletRequest.class);
			doFilter(request);
		} finally {
			setProperty(Parameter.URL_EXCLUDE_PATTERN, "");
		}

		// standard
		setUp();
		HttpServletRequest request = createNiceMock(HttpServletRequest.class);
		doFilter(request);

		// log
		setUp();
		setProperty(Parameter.LOG, TRUE);
		try {
			((Logger) org.slf4j.LoggerFactory.getLogger(FILTER_NAME)).setLevel(Level.WARN);
			request = createNiceMock(HttpServletRequest.class);
			doFilter(request);

			((Logger) org.slf4j.LoggerFactory.getLogger(FILTER_NAME)).setLevel(Level.DEBUG);
			request = createNiceMock(HttpServletRequest.class);
			doFilter(request);

			request = createNiceMock(HttpServletRequest.class);
			expect(request.getHeader("X-Forwarded-For")).andReturn("me").anyTimes();
			expect(request.getQueryString()).andReturn("param1=1").anyTimes();
			doFilter(request);
		} finally {
			setProperty(Parameter.LOG, null);
		}

		// ajax
		request = createNiceMock(HttpServletRequest.class);
		expect(request.getHeader("X-Requested-With")).andReturn("XMLHttpRequest");
		doFilter(request);

		// spring mvc with @RequestMapping (read in CounterRequestContext.getHttpRequestName())
		request = createNiceMock(HttpServletRequest.class);
		expect(request
				.getAttribute("org.springframework.web.servlet.HandlerMapping.bestMatchingPattern"))
						.andReturn("/testspringmvc").anyTimes();
		doFilter(request);

		// erreur système http, avec log
		setProperty(Parameter.LOG, TRUE);
		try {
			final String test = "test";
			request = createNiceMock(HttpServletRequest.class);
			doFilter(request, new UnknownError(test));
			request = createNiceMock(HttpServletRequest.class);
			doFilter(request, new IllegalStateException(test));
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
		expect(request.getSession(false)).andReturn(session).anyTimes();
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
		expect(session.getAttribute(SessionListener.SESSION_COUNTRY_KEY))
				.andReturn(Locale.FRANCE.getCountry()).anyTimes();
		expect(session.getAttribute(SessionListener.SESSION_REMOTE_ADDR)).andReturn("somewhere")
				.anyTimes();
		expect(session.getAttribute(SessionListener.SESSION_REMOTE_USER)).andReturn("me")
				.anyTimes();
		replay(session);
		doFilter(request);
		verify(session);
	}

	/** Test.
	 * @throws ServletException e
	 * @throws IOException e */
	@Test
	public void testDoFilterWithGWT() throws ServletException, IOException {
		final HttpServletRequest request = createNiceMock(HttpServletRequest.class);
		final String textGwtRpc = "text/x-gwt-rpc";
		expect(request.getContentType()).andReturn(textGwtRpc).anyTimes();
		expect(request.getInputStream())
				.andReturn(createInputStreamForString("1|2|3|4|5|6|7|8|9|10")).anyTimes();
		doFilter(request);

		final HttpServletRequest request2a = createNiceMock(HttpServletRequest.class);
		expect(request2a.getContentType()).andReturn("not/x-gwt-rpc").anyTimes();
		expect(request2a.getInputStream())
				.andReturn(createInputStreamForString("1|2|3|4|5|6|7|8|9|10")).anyTimes();
		doFilter(request2a);

		final HttpServletRequest request2b = createNiceMock(HttpServletRequest.class);
		expect(request2b.getContentType()).andReturn(textGwtRpc).anyTimes();
		expect(request2b.getInputStream()).andReturn(createInputStreamForString("1|2|3|4|5|6"))
				.anyTimes();
		expect(request2b.getReader()).andReturn(new BufferedReader(new StringReader("1|2|3|4|5|6")))
				.anyTimes();
		replay(request2b);
		final PayloadNameRequestWrapper wrapper2b = new PayloadNameRequestWrapper(request2b);
		wrapper2b.getInputStream().read();
		wrapper2b.getReader().read();
		verify(request2b);

		final HttpServletRequest request2 = createNiceMock(HttpServletRequest.class);
		expect(request2.getContentType()).andReturn(textGwtRpc).anyTimes();
		expect(request2.getInputStream())
				.andReturn(createInputStreamForString("1|2|3|4|5|6||8|9|10")).anyTimes();
		expect(request2.getReader()).andReturn(new BufferedReader(new StringReader("1|2|3|4|5|6")))
				.anyTimes();
		replay(request2);
		final PayloadNameRequestWrapper wrapper2 = new PayloadNameRequestWrapper(request2);
		wrapper2.getInputStream().read();
		wrapper2.getReader().read();
		verify(request2);

		final HttpServletRequest request3 = createNiceMock(HttpServletRequest.class);
		expect(request3.getContentType()).andReturn(textGwtRpc).anyTimes();
		expect(request3.getCharacterEncoding()).andReturn("utf-8").anyTimes();
		expect(request3.getInputStream())
				.andReturn(createInputStreamForString("1|2|3|4|5|6||8|9|10")).anyTimes();
		expect(request3.getReader()).andReturn(new BufferedReader(new StringReader("1|2|3|4|5|6")))
				.anyTimes();
		replay(request3);
		final PayloadNameRequestWrapper wrapper3 = new PayloadNameRequestWrapper(request3);
		wrapper3.getInputStream().read();
		wrapper3.getInputStream().read();
		wrapper3.getReader().read();
		wrapper3.getReader().read();
		verify(request3);
	}

	private ServletInputStream createInputStreamForString(final String string) {
		final ByteArrayInputStream byteArrayInputStream = new ByteArrayInputStream(
				string.getBytes());
		// CHECKSTYLE:OFF
		return new ServletInputStream() {
			// CHECKSTYLE:ON
			@Override
			public int read() throws IOException {
				return byteArrayInputStream.read();
			}

			@Override
			public boolean isFinished() {
				return false;
			}

			@Override
			public boolean isReady() {
				return false;
			}

			@Override
			public void setReadListener(ReadListener readListener) {
				// nothing
			}
		};
	}

	private void doFilter(HttpServletRequest request) throws ServletException, IOException {
		doFilter(request, null);
	}

	private void doFilter(HttpServletRequest request, Throwable exceptionInDoFilter)
			throws ServletException, IOException {
		final FilterChain chain = createNiceMock(FilterChain.class);
		expect(request.getRequestURI()).andReturn(CONTEXT_PATH + TEST_REQUEST).anyTimes();
		expect(request.getContextPath()).andReturn(CONTEXT_PATH).anyTimes();
		expect(request.getMethod()).andReturn("GET").anyTimes();
		if (exceptionInDoFilter != null) {
			// cela fera une erreur système http comptée dans les stats
			expect(request.getRemoteUser()).andThrow(exceptionInDoFilter);
		}
		final HttpServletResponse response = createNiceMock(HttpServletResponse.class);

		replay(request);
		replay(response);
		replay(chain);
		if (exceptionInDoFilter != null) {
			try {
				monitoringFilter.doFilter(request, response, chain);
			} catch (final Throwable t) { // NOPMD
				assertNotNull("ok", t);
			}
		} else {
			monitoringFilter.doFilter(request, response, chain);
		}
		verify(request);
		verify(response);
		verify(chain);
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testFilterServletResponseWrapper() throws IOException {
		final HttpServletResponse response = createNiceMock(HttpServletResponse.class);
		expect(response.getOutputStream())
				.andReturn(new FilterServletOutputStream(new ByteArrayOutputStream())).anyTimes();
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
		expect(response2.getOutputStream())
				.andReturn(new FilterServletOutputStream(new ByteArrayOutputStream())).anyTimes();
		expect(response2.getCharacterEncoding()).andReturn(null).anyTimes();
		final CounterServletResponseWrapper wrappedResponse2 = new CounterServletResponseWrapper(
				response);
		replay(response2);
		assertNotNull("getWriter", wrappedResponse2.getWriter());
		assertNotNull("getWriter bis", wrappedResponse2.getWriter());
		wrappedResponse2.close();
		verify(response2);
	}

	/** Test.
	 * @throws ServletException e
	 * @throws IOException e */
	@Test
	public void testDoMonitoring() throws ServletException, IOException {
		monitoring(Collections.<HttpParameter, String> emptyMap());
		monitoring(Collections.singletonMap(HttpParameter.FORMAT, "html"));
		monitoring(Collections.singletonMap(HttpParameter.FORMAT, "htmlbody"));
		setProperty(Parameter.DISABLED, Boolean.TRUE.toString());
		try {
			setUp();
			monitoring(Collections.<HttpParameter, String> emptyMap(), false);
		} finally {
			setProperty(Parameter.DISABLED, Boolean.FALSE.toString());
		}
		setProperty(Parameter.NO_DATABASE, Boolean.TRUE.toString());
		try {
			setUp();
			monitoring(Collections.<HttpParameter, String> emptyMap());
		} finally {
			setProperty(Parameter.NO_DATABASE, Boolean.FALSE.toString());
		}
		setProperty(Parameter.ALLOWED_ADDR_PATTERN, "256.*");
		try {
			setUp();
			monitoring(Collections.<HttpParameter, String> emptyMap(), false);
			setProperty(Parameter.ALLOWED_ADDR_PATTERN, ".*");
			setUp();
			monitoring(Collections.<HttpParameter, String> emptyMap(), false);
		} finally {
			setProperty(Parameter.ALLOWED_ADDR_PATTERN, null);
		}
		setProperty(Parameter.AUTHORIZED_USERS, "admin:password, ");
		try {
			setUp();
			monitoring(Collections.<HttpParameter, String> emptyMap(), false);
			setProperty(Parameter.AUTHORIZED_USERS, "");
			setUp();
			monitoring(Collections.<HttpParameter, String> emptyMap(), false);
		} finally {
			setProperty(Parameter.AUTHORIZED_USERS, null);
		}
		setProperty(Parameter.MONITORING_PATH, "/admin/monitoring");
		try {
			setUp();
			monitoring(Collections.<HttpParameter, String> emptyMap(), false);
		} finally {
			setProperty(Parameter.MONITORING_PATH, "/monitoring");
		}
		try {
			setProperty(Parameter.JMX_EXPOSE_ENABLED, Boolean.TRUE.toString());
			setUp();
			monitoring(Collections.<HttpParameter, String> emptyMap());
		} finally {
			setProperty(Parameter.JMX_EXPOSE_ENABLED, null);
		}
	}

	/** Test.
	 * @throws ServletException e
	 * @throws IOException e */
	@Test
	public void testDoMonitoringWithRum() throws ServletException, IOException {
		try {
			setProperty(Parameter.RUM_ENABLED, Boolean.TRUE.toString());
			setUp();

			// simulate html page with RUM
			final HttpServletRequest requestForRum = createNiceMock(HttpServletRequest.class);
			expect(requestForRum.getHeader("accept")).andReturn("text/html");
			expect(requestForRum.getInputStream())
					.andReturn(createInputStreamForString("<html><body>test</body></html>"))
					.anyTimes();
			doFilter(requestForRum);

			// simulate call to monitoring?resource=boomerang.min.js
			monitoring(Collections.singletonMap(HttpParameter.RESOURCE, "boomerang.min.js"));
			monitoring(Collections.<HttpParameter, String> emptyMap());
			monitoring(Collections.singletonMap(HttpParameter.PART, HttpPart.RUM.getName()), false);

			// simulate call to monitoring?part=rum to register RUM data
			final Map<String, String> rumMap = new HashMap<String, String>();
			rumMap.put(HttpParameter.PART.getName(), HttpPart.RUM.getName());
			rumMap.put("requestName", TEST_REQUEST + " GET");
			rumMap.put("serverTime", "100");
			rumMap.put("timeToFirstByte", "100");
			rumMap.put("domProcessing", "50");
			rumMap.put("pageRendering", "50");
			monitoring0(rumMap, false);

			// simulate call to monitoring for details of request with RUM data in html (period=jour : rumHits=0)
			final Map<HttpParameter, String> graphMap = new HashMap<HttpParameter, String>();
			graphMap.put(HttpParameter.PART, HttpPart.GRAPH.getName());
			final String requestId = new CounterRequest(TEST_REQUEST + " GET",
					Counter.HTTP_COUNTER_NAME).getId();
			graphMap.put(HttpParameter.GRAPH, requestId);
			monitoring(graphMap);

			// simulate call to monitoring for details of request with RUM data in html (period=tout  : rumHits>0)
			graphMap.put(HttpParameter.PERIOD, Period.TOUT.getCode());
			monitoring(graphMap);
			// simulate call to monitoring for details of request with RUM data in pdf
			graphMap.put(HttpParameter.FORMAT, "pdf");
			monitoring(graphMap);
		} finally {
			setProperty(Parameter.RUM_ENABLED, null);
		}
	}

	/** Test.
	 * @throws ServletException e
	 * @throws IOException e */
	@Test
	public void testDoMonitoringWithMail() throws ServletException, IOException {
		setProperty(Parameter.MAIL_SESSION, "testmailsession");
		setProperty(Parameter.ADMIN_EMAILS, null);
		setUp();
		monitoring(Collections.<HttpParameter, String> emptyMap());
		setProperty(Parameter.ADMIN_EMAILS, "evernat@free.fr");
		setUp();
		monitoring(Collections.<HttpParameter, String> emptyMap());
		setProperty(Parameter.MAIL_SESSION, null);
		setProperty(Parameter.ADMIN_EMAILS, null);
	}

	/** Test.
	 * @throws ServletException e
	 * @throws IOException e */
	@Test
	public void testDoMonitoringWithPeriod() throws ServletException, IOException {
		final Map<HttpParameter, String> parameters = new HashMap<HttpParameter, String>();
		parameters.put(HttpParameter.PERIOD, Period.JOUR.getCode());
		monitoring(parameters);
		parameters.put(HttpParameter.PATTERN, "dd/MM/yyyy");
		parameters.put(HttpParameter.PERIOD, "1/1/2000|1/1/2001");
		monitoring(parameters);

		HttpCookieManager.setDefaultRange(Period.TOUT.getRange());
		HttpCookieManager.setDefaultRange(Period.JOUR.getRange());
	}

	/** Test.
	 * @throws ServletException e
	 * @throws IOException e */
	@Test
	public void testDoMonitoringWithResource() throws ServletException, IOException {
		monitoring(Collections.singletonMap(HttpParameter.RESOURCE, "monitoring.css"));
		monitoring(Collections.singletonMap(HttpParameter.RESOURCE, "beans.png"));
	}

	/** Test.
	 * @throws ServletException e
	 * @throws IOException e */
	@Test
	public void testDoMonitoringWithGraph() throws ServletException, IOException {
		final Map<HttpParameter, String> parameters = new HashMap<HttpParameter, String>();
		parameters.put(HttpParameter.GRAPH, "usedMemory");
		parameters.put(HttpParameter.WIDTH, "800");
		parameters.put(HttpParameter.HEIGHT, "600");
		monitoring(parameters);
		parameters.put(HttpParameter.GRAPH, "unknown");
		monitoring(parameters, false);
	}

	/** Test.
	 * @throws ServletException e
	 * @throws IOException e */
	@Test
	public void testDoMonitoringWithParts() throws ServletException, IOException {
		final Map<HttpParameter, String> parameters = new HashMap<HttpParameter, String>();

		parameters.put(HttpParameter.PART, HttpPart.CURRENT_REQUESTS.getName());
		monitoring(parameters);
		parameters.put(HttpParameter.PART, HttpPart.THREADS.getName());
		monitoring(parameters);
		parameters.put(HttpParameter.PART, HttpPart.THREADS_DUMP.getName());
		monitoring(parameters);
		final File hsErrPidFile = new File("./hs_err_pid12345.log");
		try {
			hsErrPidFile.createNewFile();
			parameters.put(HttpParameter.PART, HttpPart.CRASHES.getName());
			monitoring(parameters, false);
			parameters.put(HttpParameter.PATH, hsErrPidFile.getAbsolutePath().replace('\\', '/'));
			monitoring(parameters, false);
			parameters.put(HttpParameter.PATH, "unknown");
			monitoring(parameters, false);
			parameters.remove(HttpParameter.PATH);
		} finally {
			hsErrPidFile.delete();
		}
		parameters.put(HttpParameter.PART, HttpPart.CACHE_KEYS.getName());
		final String cacheName = getClass().getName();
		CacheManager.getInstance().addCache(cacheName);
		parameters.put(HttpParameter.CACHE_ID, cacheName);
		monitoring(parameters);
		CacheManager.getInstance().getCache(cacheName).put(new Element("1", "value"));
		monitoring(parameters);
		parameters.put(HttpParameter.FORMAT, "htmlbody");
		monitoring(parameters);
		setProperty(Parameter.SYSTEM_ACTIONS_ENABLED, "false");
		monitoring(parameters);
		CacheManager.getInstance().removeCache(cacheName);
		parameters.remove(HttpParameter.CACHE_ID);
		parameters.remove(HttpParameter.FORMAT);
		setProperty(Parameter.SYSTEM_ACTIONS_ENABLED, TRUE);

		parameters.put(HttpParameter.PART, HttpPart.JCACHE_KEYS.getName());
		final MutableConfiguration<Object, Object> conf = new MutableConfiguration<Object, Object>();
		conf.setManagementEnabled(true);
		conf.setStatisticsEnabled(true);
		Caching.getCachingProvider().getCacheManager().createCache(cacheName, conf);
		Caching.getCachingProvider().getCacheManager().createCache(cacheName + "2", conf);
		parameters.put(HttpParameter.CACHE_ID, cacheName);
		monitoring(parameters);
		Caching.getCachingProvider().getCacheManager().getCache(cacheName).put("1", "value");
		monitoring(parameters);
		parameters.put(HttpParameter.FORMAT, "htmlbody");
		monitoring(parameters);
		setProperty(Parameter.SYSTEM_ACTIONS_ENABLED, "false");
		monitoring(parameters);
		Caching.getCachingProvider().getCacheManager().destroyCache(cacheName);
		parameters.remove(HttpParameter.CACHE_ID);
		parameters.remove(HttpParameter.FORMAT);
		setProperty(Parameter.SYSTEM_ACTIONS_ENABLED, TRUE);

		parameters.put(HttpParameter.PART, HttpPart.JNLP.getName());
		monitoring(parameters);
		setProperty(Parameter.JAVAMELODY_SWING_URL, "http://dummy");
		monitoring(parameters);
		parameters.put(HttpParameter.PART, HttpPart.DEPENDENCIES.getName());
		monitoring(parameters);
		parameters.put(HttpParameter.PART, HttpPart.COUNTER_SUMMARY_PER_CLASS.getName());
		parameters.put(HttpParameter.COUNTER, "services");
		monitoring(parameters);
		parameters.put(HttpParameter.GRAPH, "unknown service");
		monitoring(parameters);
		parameters.remove(HttpParameter.COUNTER);

		doMonitoringWithGraphPart();

		doMonitoringWithSourcePart();

		doMonitoringWithUnknownPart();
	}

	/** Test.
	 * @throws ServletException e
	 * @throws IOException e */
	@Test
	public void testDoMonitoringWithPartsForSystemActions() throws ServletException, IOException {
		final Map<HttpParameter, String> parameters = new HashMap<HttpParameter, String>();
		setProperty(Parameter.SYSTEM_ACTIONS_ENABLED, TRUE);
		parameters.put(HttpParameter.PART, HttpPart.PROCESSES.getName());
		monitoring(parameters);
		monitorJdbcParts(parameters);
		// il ne faut pas faire un heapHisto sans thread comme dans TestHtmlHeapHistogramReport
		//		parameters.put(HttpParameter.PART, HttpPart.HEAP_HISTO.getName());
		//		monitoring(parameters);
		monitoringSessionsPart(parameters);
		parameters.put(HttpParameter.PART, HttpPart.WEB_XML.getName());
		monitoring(parameters, false);
		parameters.put(HttpParameter.PART, HttpPart.POM_XML.getName());
		monitoring(parameters, false);
		parameters.put(HttpParameter.PART, HttpPart.JNDI.getName());
		monitoring(parameters);
		parameters.put(HttpParameter.PART, HttpPart.MBEANS.getName());
		monitoring(parameters);
		final ConfigurableApplicationContext context = new ClassPathXmlApplicationContext(
				new String[] { "net/bull/javamelody/monitoring-spring.xml", });
		try {
			context.getBeanDefinitionNames();
			parameters.put(HttpParameter.PART, HttpPart.SPRING_BEANS.getName());
			monitoring(parameters);
			setProperty(Parameter.SAMPLING_SECONDS, "60");
			setUp();
			parameters.put(HttpParameter.PART, HttpPart.HOTSPOTS.getName());
			monitoring(parameters);
			parameters.remove(HttpParameter.PART);
			parameters.put(HttpParameter.JMX_VALUE,
					"java.lang:type=OperatingSystem.ProcessCpuTime");
			monitoring(parameters);
			parameters.remove(HttpParameter.JMX_VALUE);
		} finally {
			context.close();
		}
	}

	/**
	 * @throws ServletException e
	 * @throws IOException e */
	@Test
	public void testDoMonitoringWithReportParameter() throws ServletException, IOException {
		final Map<HttpParameter, String> parameters = new HashMap<HttpParameter, String>();
		parameters.put(HttpParameter.REPORT, "customReport");
		System.setProperty(Parameters.PARAMETER_SYSTEM_PREFIX + "customReport", "");
		monitoring(parameters, false);
		System.setProperty(Parameters.PARAMETER_SYSTEM_PREFIX + "customReport", "test");
		monitoring(parameters, false);
		System.setProperty(Parameters.PARAMETER_SYSTEM_PREFIX + "customReport", "/test");
		monitoring(parameters, false);
		parameters.remove(HttpParameter.REPORT);
	}

	private void doMonitoringWithUnknownPart() throws IOException, ServletException {
		final Map<HttpParameter, String> parameters = new HashMap<HttpParameter, String>();
		parameters.put(HttpParameter.PART, "unknown part");
		boolean exception = false;
		try {
			monitoring(parameters);
		} catch (final IllegalArgumentException e) {
			exception = true;
		}
		assertTrue("exception if unknown part", exception);
		parameters.put(HttpParameter.PART, HttpPart.JROBINS.getName());
		try {
			monitoring(parameters);
		} catch (final IllegalArgumentException e) {
			exception = true;
		}
		assertTrue("exception if unknown part", exception);
	}

	private void monitoringSessionsPart(final Map<HttpParameter, String> parameters)
			throws IOException, ServletException {
		parameters.put(HttpParameter.PART, HttpPart.SESSIONS.getName());
		monitoring(parameters);
		parameters.put(HttpParameter.PART, HttpPart.SESSIONS.getName());
		parameters.put(HttpParameter.SESSION_ID, "expired session");
		monitoring(parameters);
		parameters.remove(HttpParameter.SESSION_ID);
	}

	private void doMonitoringWithGraphPart() throws IOException, ServletException {
		final Map<HttpParameter, String> parameters = new HashMap<HttpParameter, String>();
		parameters.put(HttpParameter.PART, HttpPart.GRAPH.getName());
		parameters.put(HttpParameter.GRAPH, "usedMemory");
		monitoring(parameters);
		parameters.put(HttpParameter.PART, HttpPart.LAST_VALUE.getName());
		parameters.put(HttpParameter.GRAPH, "usedMemory,cpu,unknown");
		monitoring(parameters);
		parameters.put(HttpParameter.PART, HttpPart.USAGES.getName());
		parameters.put(HttpParameter.GRAPH, "unknown");
		monitoring(parameters);
	}

	private void doMonitoringWithSourcePart() throws IOException, ServletException {
		final Map<HttpParameter, String> parameters = new HashMap<HttpParameter, String>();
		parameters.put(HttpParameter.PART, HttpPart.SOURCE.getName());
		// classe java du jdk
		parameters.put(HttpParameter.CLASS, "java.lang.String");
		monitoring(parameters);
		// classe interne dans le même fichier
		parameters.put(HttpParameter.CLASS, "java.lang.Thread$State");
		monitoring(parameters);
		// classe javax du jdk
		parameters.put(HttpParameter.CLASS, "javax.naming.InitialContext");
		monitoring(parameters);
		// classe inexistante
		parameters.put(HttpParameter.CLASS, "java.dummy");
		monitoring(parameters);
		// classe d'un jar construit par Maven
		parameters.put(HttpParameter.CLASS, "org.jrobin.core.RrdDb");
		monitoring(parameters);
		// classe d'un jar construit par Maven, sans sources
		parameters.put(HttpParameter.CLASS, "org.apache.tomcat.dbcp.dbcp.BasicDataSource");
		monitoring(parameters);
		// classe d'un jar non construit par Maven
		parameters.put(HttpParameter.CLASS, "com.lowagie.text.Document");
		monitoring(parameters);
		parameters.remove(HttpParameter.CLASS);
	}

	private void monitorJdbcParts(Map<HttpParameter, String> parameters)
			throws IOException, ServletException {
		final Connection connection = TestDatabaseInformations.initH2();
		try {
			parameters.put(HttpParameter.PART, HttpPart.DATABASE.getName());
			monitoring(parameters);
			parameters.put(HttpParameter.PART, HttpPart.DATABASE.getName());
			parameters.put(HttpParameter.REQUEST, "0");
			monitoring(parameters);
			parameters.put(HttpParameter.PART, HttpPart.CONNECTIONS.getName());
			monitoring(parameters);
			parameters.put(HttpParameter.PART, HttpPart.CONNECTIONS.getName());
			parameters.put(HttpParameter.FORMAT, "htmlbody");
			monitoring(parameters);
			parameters.remove(HttpParameter.FORMAT);
			parameters.put(HttpParameter.REQUEST, "0");
			monitoring(parameters);
			parameters.remove(HttpParameter.REQUEST);
		} finally {
			try {
				connection.close();
			} catch (final SQLException e) {
				LOG.warn(e.toString(), e);
			}
		}
	}

	/** Test.
	 * @throws ServletException e
	 * @throws IOException e */
	@Test
	public void testDoMonitoringWithActions() throws ServletException, IOException {
		final Map<HttpParameter, String> parameters = new HashMap<HttpParameter, String>();
		setProperty(Parameter.SYSTEM_ACTIONS_ENABLED, TRUE);
		parameters.put(HttpParameter.ACTION, Action.GC.toString());
		monitoring(parameters);
		parameters.put(HttpParameter.ACTION, Action.INVALIDATE_SESSIONS.toString());
		monitoring(parameters);
		parameters.put(HttpParameter.ACTION, Action.INVALIDATE_SESSION.toString());
		parameters.put(HttpParameter.SESSION_ID, "123456789");
		monitoring(parameters);
		parameters.put(HttpParameter.ACTION, Action.CLEAR_CACHES.toString());
		monitoring(parameters);
		if (CacheManager.getInstance().getCache("test clear") == null) {
			CacheManager.getInstance().addCache("test clear");
		}
		monitoring(parameters);
		parameters.put(HttpParameter.ACTION, Action.PAUSE_JOB.toString());
		parameters.put(HttpParameter.JOB_ID, "all");
		monitoring(parameters);
		parameters.put(HttpParameter.ACTION, Action.RESUME_JOB.toString());
		monitoring(parameters);
		parameters.put(HttpParameter.ACTION, Action.CLEAR_COUNTER.toString());
		parameters.put(HttpParameter.COUNTER, "services");
		monitoring(parameters);
		parameters.put(HttpParameter.ACTION, Action.CLEAR_COUNTER.toString());
		parameters.put(HttpParameter.COUNTER, "all");
		monitoring(parameters);
	}

	/** Test.
	 * @throws ServletException e
	 * @throws IOException e */
	@Test
	public void testDoMonitoringWithFormatPdf() throws ServletException, IOException {
		final Map<HttpParameter, String> parameters = new HashMap<HttpParameter, String>();
		parameters.put(HttpParameter.FORMAT, "pdf");
		monitoring(parameters);
		parameters.put(HttpParameter.PART, HttpPart.RUNTIME_DEPENDENCIES.getName());
		parameters.put(HttpParameter.COUNTER, "services");
		monitoring(parameters);
		parameters.remove(HttpParameter.COUNTER);
		parameters.put(HttpParameter.PART, HttpPart.CURRENT_REQUESTS.getName());
		monitoring(parameters);
		parameters.put(HttpParameter.PART, HttpPart.THREADS.getName());
		monitoring(parameters);
		setProperty(Parameter.SYSTEM_ACTIONS_ENABLED, TRUE);
		parameters.put(HttpParameter.PART, HttpPart.SESSIONS.getName());
		monitoring(parameters);
		parameters.put(HttpParameter.PART, HttpPart.PROCESSES.getName());
		monitoring(parameters);
		boolean exception = false;
		try {
			parameters.put(HttpParameter.PART, HttpPart.JNDI.getName());
			monitoring(parameters);
		} catch (final Exception e) {
			exception = true;
		}
		assertTrue("exception caused by NoInitialContextException", exception);
		parameters.put(HttpParameter.PART, HttpPart.MBEANS.getName());
		monitoring(parameters);
		parameters.put(HttpParameter.PART, HttpPart.COUNTER_SUMMARY_PER_CLASS.getName());
		parameters.put(HttpParameter.COUNTER, "guice");
		monitoring(parameters);
		parameters.remove(HttpParameter.COUNTER);
		TestDatabaseInformations.initJdbcDriverParameters();
		parameters.put(HttpParameter.PART, HttpPart.DATABASE.getName());
		monitoring(parameters);
		setProperty(Parameter.SAMPLING_SECONDS, "60");
		setUp();
		parameters.put(HttpParameter.PART, HttpPart.HOTSPOTS.getName());
		monitoring(parameters);
		// il ne faut pas faire un heapHisto sans thread comme dans TestHtmlHeapHistogramReport
		//		parameters.put(HttpParameter.PART, HttpPart.HEAP_HISTO.getName());
		//		monitoring(parameters);
		parameters.put(HttpParameter.PART, HttpPart.GRAPH.getName());
		parameters.put(HttpParameter.GRAPH, "usedMemory");
		monitoring(parameters);
		parameters.remove(HttpParameter.GRAPH);
		parameters.put(HttpParameter.PART, "unknown part");
		try {
			monitoring(parameters);
		} catch (final Exception e) {
			exception = true;
		}
		assertTrue("exception if unknown part", exception);
	}

	/** Test.
	 * @throws ServletException e
	 * @throws IOException e */
	// CHECKSTYLE:OFF
	@Test
	// CHECKSTYLE:ON
	public void testDoMonitoringWithFormatSerialized() throws ServletException, IOException {
		final Map<HttpParameter, String> parameters = new HashMap<HttpParameter, String>();
		parameters.put(HttpParameter.FORMAT, TransportFormat.SERIALIZED.getCode());
		monitoring(parameters);
		parameters.put(HttpParameter.JMX_VALUE, "java.lang:type=OperatingSystem.ProcessCpuTime");
		monitoring(parameters);
		parameters.remove(HttpParameter.JMX_VALUE);
		parameters.put(HttpParameter.PART, HttpPart.LAST_VALUE.getName());
		monitoring(parameters);
		parameters.put(HttpParameter.GRAPH, "usedMemory,cpu,unknown");
		monitoring(parameters);
		parameters.remove(HttpParameter.GRAPH);
		setProperty(Parameter.SAMPLING_SECONDS, "60");
		setUp();
		parameters.put(HttpParameter.PART, HttpPart.HOTSPOTS.getName());
		monitoring(parameters);
		parameters.put(HttpParameter.PART, HttpPart.JVM.getName());
		monitoring(parameters);
		parameters.put(HttpParameter.PART, HttpPart.THREADS.getName());
		monitoring(parameters);
		parameters.put(HttpParameter.PART, HttpPart.CURRENT_REQUESTS.getName());
		monitoring(parameters);
		parameters.put(HttpParameter.PART, HttpPart.DEFAULT_WITH_CURRENT_REQUESTS.getName());
		monitoring(parameters);
		parameters.put(HttpParameter.WIDTH, "80");
		parameters.put(HttpParameter.HEIGHT, "80");
		parameters.put(HttpParameter.PART, HttpPart.JROBINS.getName());
		monitoring(parameters);
		parameters.put(HttpParameter.GRAPH, "cpu");
		parameters.put(HttpParameter.PART, HttpPart.JROBINS.getName());
		monitoring(parameters);
		parameters.put(HttpParameter.PART, HttpPart.GRAPH.getName());
		monitoring(parameters);
		parameters.remove(HttpParameter.GRAPH);
		parameters.put(HttpParameter.PART, HttpPart.OTHER_JROBINS.getName());
		monitoring(parameters);
		parameters.remove(HttpParameter.WIDTH);
		parameters.remove(HttpParameter.HEIGHT);
		setProperty(Parameter.SYSTEM_ACTIONS_ENABLED, TRUE);
		monitoring(parameters);
		monitoringSessionsPart(parameters);
		parameters.put(HttpParameter.PART, HttpPart.PROCESSES.getName());
		monitoring(parameters);
		parameters.put(HttpParameter.PART, HttpPart.JNDI.getName());
		monitoring(parameters);
		parameters.put(HttpParameter.PART, HttpPart.MBEANS.getName());
		monitoring(parameters);
		parameters.put(HttpParameter.PART, HttpPart.DEPENDENCIES.getName());
		monitoring(parameters);
		parameters.put(HttpParameter.PART, HttpPart.CRASHES.getName());
		monitoring(parameters);
		TestDatabaseInformations.initJdbcDriverParameters();
		parameters.put(HttpParameter.PART, HttpPart.DATABASE.getName());
		monitoring(parameters);
		parameters.put(HttpParameter.REQUEST, "0");
		monitoring(parameters);
		parameters.put(HttpParameter.PART, HttpPart.EXPLAIN_PLAN.getName());
		parameters.put(HttpParameter.REQUEST, "select 1 from dual");
		monitoring(parameters);
		parameters.remove(HttpParameter.REQUEST);
		parameters.put(HttpParameter.PART, HttpPart.CONNECTIONS.getName());
		monitoring(parameters);
		parameters.put(HttpParameter.PART, HttpPart.COUNTER_SUMMARY_PER_CLASS.getName());
		parameters.put(HttpParameter.COUNTER, "guice");
		monitoring(parameters);
		parameters.put(HttpParameter.PERIOD, "jour");
		monitoring(parameters);
		parameters.put(HttpParameter.PATTERN, "dd/MM/yyyy");
		parameters.put(HttpParameter.PERIOD, "1/1/2000|1/1/2001");
		monitoring(parameters);
		parameters.remove(HttpParameter.PATTERN);
		parameters.remove(HttpParameter.COUNTER);
		parameters.remove(HttpParameter.PERIOD);
		parameters.put(HttpParameter.PART, HttpPart.WEBAPP_VERSIONS.getName());
		monitoring(parameters);
		// il ne faut pas faire un heapHisto sans thread comme dans TestHtmlHeapHistogramReport
		//		parameters.put(HttpParameter.PART, HttpPart.HEAP_HISTO.getName());
		//		monitoring(parameters);
		parameters.put(HttpParameter.PART, null);
		parameters.put(HttpParameter.COLLECTOR, "stop");
		monitoring(parameters);
		parameters.put(HttpParameter.ACTION, Action.GC.toString());
		monitoring(parameters);
	}

	/** Test.
	 * @throws ServletException e
	 * @throws IOException e */
	@Test
	public void testDoMonitoringWithFormatXml() throws ServletException, IOException {
		final Map<HttpParameter, String> parameters = new HashMap<HttpParameter, String>();
		parameters.put(HttpParameter.FORMAT, TransportFormat.XML.getCode());
		monitoring(parameters);
		setProperty(Parameter.SYSTEM_ACTIONS_ENABLED, TRUE);
		parameters.put(HttpParameter.PART, HttpPart.SESSIONS.getName());
		monitoring(parameters);
		parameters.put(HttpParameter.PART, HttpPart.PROCESSES.getName());
		monitoring(parameters);
		TestDatabaseInformations.initJdbcDriverParameters();
		parameters.put(HttpParameter.PART, HttpPart.DATABASE.getName());
		monitoring(parameters);
		parameters.put(HttpParameter.PART, HttpPart.CONNECTIONS.getName());
		monitoring(parameters);
		// il ne faut pas faire un heapHisto sans thread comme dans TestHtmlHeapHistogramReport
		//		parameters.put(HttpParameter.PART, HttpPart.HEAP_HISTO.getName());
		//		monitoring(parameters);
	}

	/** Test.
	 * @throws ServletException e
	 * @throws IOException e */
	@Test
	public void testDoMonitoringWithFormatJson() throws ServletException, IOException {
		final Map<HttpParameter, String> parameters = new HashMap<HttpParameter, String>();
		parameters.put(HttpParameter.FORMAT, TransportFormat.JSON.getCode());
		monitoring(parameters);
		parameters.put(HttpParameter.PART, HttpPart.THREADS.getName());
		monitoring(parameters);
		setProperty(Parameter.SYSTEM_ACTIONS_ENABLED, TRUE);
		parameters.put(HttpParameter.PART, HttpPart.SESSIONS.getName());
		monitoring(parameters);
		parameters.put(HttpParameter.PART, HttpPart.PROCESSES.getName());
		monitoring(parameters);
		TestDatabaseInformations.initJdbcDriverParameters();
		parameters.put(HttpParameter.PART, HttpPart.DATABASE.getName());
		monitoring(parameters);
		parameters.put(HttpParameter.PART, HttpPart.CONNECTIONS.getName());
		monitoring(parameters);
		// il ne faut pas faire un heapHisto sans thread comme dans TestHtmlHeapHistogramReport
		//		parameters.put(HttpParameter.PART, HttpPart.HEAP_HISTO.getName());
		//		monitoring(parameters);
	}

	/** Test.
	 * @throws ServletException e
	 * @throws IOException e
	 */
	@Test
	public void testDoMonitoringWithFormatPrometheus() throws ServletException, IOException {
		final Map<String, String> parameters = new HashMap<String, String>();
		parameters.put("format", "prometheus");
		monitoring0(parameters, true);
		parameters.put("includeLastValue", "true");
		monitoring0(parameters, true);
	}

	private void monitoring(Map<HttpParameter, String> parameters)
			throws IOException, ServletException {
		monitoring(parameters, true);
	}

	private void monitoring(Map<HttpParameter, String> parameters, boolean checkResultContent)
			throws IOException, ServletException {
		final Map<String, String> params = new HashMap<String, String>();
		for (final Map.Entry<HttpParameter, String> entry : parameters.entrySet()) {
			params.put(entry.getKey().getName(), entry.getValue());
		}
		monitoring0(params, checkResultContent);
	}

	private void monitoring0(Map<String, String> parameters, boolean checkResultContent)
			throws IOException, ServletException {
		final HttpServletRequest request = createNiceMock(HttpServletRequest.class);
		expect(request.getRequestURI()).andReturn("/test/monitoring").anyTimes();
		expect(request.getRequestURL()).andReturn(new StringBuffer("/test/monitoring")).anyTimes();
		expect(request.getContextPath()).andReturn(CONTEXT_PATH).anyTimes();
		expect(request.getRemoteAddr()).andReturn("here").anyTimes();
		final Random random = new Random();
		if (random.nextBoolean()) {
			expect(request.getHeaders("Accept-Encoding"))
					.andReturn(Collections.enumeration(Arrays.asList("application/gzip")))
					.anyTimes();
		} else {
			expect(request.getHeaders("Accept-Encoding"))
					.andReturn(Collections.enumeration(Arrays.asList("text/html"))).anyTimes();
		}
		for (final Map.Entry<String, String> entry : parameters.entrySet()) {
			if (HttpParameter.REQUEST.getName().equals(entry.getKey())) {
				expect(request.getHeader(entry.getKey())).andReturn(entry.getValue()).anyTimes();
			} else {
				expect(request.getParameter(entry.getKey())).andReturn(entry.getValue()).anyTimes();
			}
		}
		final Range range = Period.TOUT.getRange();
		final boolean includeDetails = "threads".equals(parameters.get("part"));
		final List<JavaInformations> javaInformationsList = Collections
				.singletonList(new JavaInformations(null, includeDetails));
		// getAttribute("range") et getAttribute("javaInformationsList") pour PdfController
		expect(request.getAttribute("range")).andReturn(range).anyTimes();
		expect(request.getAttribute("javaInformationsList")).andReturn(javaInformationsList)
				.anyTimes();
		if (parameters.isEmpty()
				|| HttpPart.JNLP.getName().equals(parameters.get(HttpParameter.PART.getName()))) {
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

		replay(request);
		replay(response);
		replay(chain);
		monitoringFilter.doFilter(request, response, chain);
		verify(request);
		verify(response);
		verify(chain);

		if (checkResultContent) {
			assertTrue("result", output.size() != 0 || stringWriter.getBuffer().length() != 0);
		}
	}

	@Test
	public void testRegisterApplicationNodeInCollectServer() throws MalformedURLException {
		MonitoringFilter.registerApplicationNodeInCollectServer(null,
				new URL("http://localhost:8080"), new URL("http://localhost:8081"));
		MonitoringFilter.registerApplicationNodeInCollectServer("test",
				new URL("http://localhost:8080"), new URL("http://localhost:8081"));
		try {
			MonitoringFilter.registerApplicationNodeInCollectServer(null, null,
					new URL("http://localhost:8081"));
		} catch (final IllegalArgumentException e) {
			assertNotNull("e", e);
		}
		try {
			MonitoringFilter.registerApplicationNodeInCollectServer(null,
					new URL("http://localhost:8080"), null);
		} catch (final IllegalArgumentException e) {
			assertNotNull("e", e);
		}
	}

	@Test
	public void testUnregisterApplicationNodeInCollectServer() throws IOException {
		MonitoringFilter.unregisterApplicationNodeInCollectServer();
	}

	private static void setProperty(Parameter parameter, String value) {
		Utils.setProperty(parameter, value);
	}
}
