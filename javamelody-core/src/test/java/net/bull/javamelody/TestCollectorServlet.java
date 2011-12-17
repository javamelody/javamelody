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
import static net.bull.javamelody.HttpParameters.COUNTER_PARAMETER;
import static net.bull.javamelody.HttpParameters.COUNTER_SUMMARY_PER_CLASS_PART;
import static net.bull.javamelody.HttpParameters.CURRENT_REQUESTS_PART;
import static net.bull.javamelody.HttpParameters.DATABASE_PART;
import static net.bull.javamelody.HttpParameters.EXPLAIN_PLAN_PART;
import static net.bull.javamelody.HttpParameters.FORMAT_PARAMETER;
import static net.bull.javamelody.HttpParameters.HEAP_HISTO_PART;
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
import static org.easymock.EasyMock.createNiceMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;
import static org.junit.Assert.assertNotNull;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;

import javax.servlet.ServletConfig;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * Test unitaire de la classe CollectorServlet.
 * @author Emeric Vernat
 */
public class TestCollectorServlet {
	private static final String TRUE = "true";
	private static final String REMOTE_ADDR = "127.0.0.1"; // NOPMD
	private static final String TEST = "test";
	private ServletConfig config;
	private ServletContext context;
	private CollectorServlet collectorServlet;

	/**
	 * Initialisation.
	 * @throws IOException e
	 */
	@Before
	public void setUp() throws IOException {
		tearDown();
		Utils.initialize();
		Utils.setProperty(Parameters.PARAMETER_SYSTEM_PREFIX + "mockLabradorRetriever", TRUE);
		Utils.setProperty(Parameter.SYSTEM_ACTIONS_ENABLED, TRUE);
		config = createNiceMock(ServletConfig.class);
		context = createNiceMock(ServletContext.class);
		expect(config.getServletContext()).andReturn(context).anyTimes();
		collectorServlet = new CollectorServlet();
	}

	/**
	 * Terminaison.
	 * @throws IOException e
	 */
	@After
	public void tearDown() throws IOException {
		// on désactive le stop sur le timer JRobin car sinon les tests suivants ne fonctionneront
		// plus si ils utilisent JRobin
		if (collectorServlet != null) {
			Utils.setProperty(Parameters.PARAMETER_SYSTEM_PREFIX + "jrobinStopDisabled", TRUE);
			collectorServlet.destroy();
		}
		Parameters.removeCollectorApplication(TEST);
	}

	/** Test.
	 * @throws ServletException e
	 * @throws IOException e */
	@Test
	public void testInit() throws ServletException, IOException {
		replay(config);
		replay(context);
		collectorServlet.init(config);
		verify(config);
		verify(context);

		setUp();
		expect(
				context.getInitParameter(Parameters.PARAMETER_SYSTEM_PREFIX
						+ Parameter.LOG.getCode())).andReturn(TRUE).anyTimes();
		expect(
				context.getInitParameter(Parameters.PARAMETER_SYSTEM_PREFIX
						+ Parameter.ALLOWED_ADDR_PATTERN.getCode())).andReturn("127\\.0\\.0\\.1")
				.anyTimes();
		replay(config);
		replay(context);
		collectorServlet.init(config);
		verify(config);
		verify(context);
	}

	/** Test.
	 * @throws ServletException e
	 * @throws IOException e */
	@Test
	public void testDoGet() throws ServletException, IOException {
		doGet("a", null);
		doGet(null, null);
		doGet(".*", null);
		doGet(null, TEST);
	}

	private void doGet(String pattern, String application) throws IOException, ServletException {
		setUp();
		final HttpServletRequest request = createNiceMock(HttpServletRequest.class);
		expect(request.getRequestURI()).andReturn("/test/request").anyTimes();
		final HttpServletResponse response = createNiceMock(HttpServletResponse.class);
		final FilterServletOutputStream servletOutputStream = new FilterServletOutputStream(
				new ByteArrayOutputStream());
		expect(response.getOutputStream()).andReturn(servletOutputStream).anyTimes();
		if (application != null) {
			expect(request.getParameter("application")).andReturn(application).anyTimes();
		}
		if (pattern != null) {
			expect(
					context.getInitParameter(Parameters.PARAMETER_SYSTEM_PREFIX
							+ Parameter.ALLOWED_ADDR_PATTERN.getCode())).andReturn(pattern)
					.anyTimes();
			expect(request.getRemoteAddr()).andReturn(REMOTE_ADDR);
		}
		replay(config);
		replay(context);
		replay(request);
		replay(response);
		collectorServlet.init(config);
		// note: sans serveur de http, il n'est pas possible d'avoir une application et un collector
		collectorServlet.doGet(request, response);
		verify(config);
		verify(context);
		verify(request);
		verify(response);
	}

	/** Test.
	 * @throws ServletException e
	 * @throws IOException e */
	@Test
	public void testDoPost() throws ServletException, IOException {
		doPost(null, null, false);
		doPost(null, null, true);
		doPost(TEST, null, true);
		doPost(TEST, "http://localhost:8090/test", true);
		doPost(TEST, "https://localhost:8090/test", true);
		doPost(TEST, "ftp://localhost:8090/test", true);
		doPost(TEST, "http://une url,pas une url", true);
	}

	private void doPost(String appName, String appUrls, boolean allowed) throws IOException,
			ServletException {
		setUp();
		final HttpServletRequest request = createNiceMock(HttpServletRequest.class);
		expect(request.getRequestURI()).andReturn("/test/request").anyTimes();
		// un cookie d'une application (qui n'existe pas)
		final Cookie[] cookies = { new Cookie("javamelody.application", "anothertest") };
		expect(request.getCookies()).andReturn(cookies).anyTimes();
		final HttpServletResponse response = createNiceMock(HttpServletResponse.class);
		final FilterServletOutputStream servletOutputStream = new FilterServletOutputStream(
				new ByteArrayOutputStream());
		expect(response.getOutputStream()).andReturn(servletOutputStream).anyTimes();
		if (appName != null) {
			Parameters.removeCollectorApplication(appName);
		}
		expect(request.getParameter("appName")).andReturn(appName).anyTimes();
		expect(request.getParameter("appUrls")).andReturn(appUrls).anyTimes();
		if (!allowed) {
			expect(
					context.getInitParameter(Parameters.PARAMETER_SYSTEM_PREFIX
							+ Parameter.ALLOWED_ADDR_PATTERN.getCode())).andReturn("none")
					.anyTimes();
			expect(request.getRemoteAddr()).andReturn(REMOTE_ADDR);
		}
		replay(config);
		replay(context);
		replay(request);
		replay(response);
		collectorServlet.init(config);
		collectorServlet.doPost(request, response);
		verify(config);
		verify(context);
		verify(request);
		verify(response);
	}

	/** Test.
	 * @throws ServletException e
	 * @throws IOException e */
	@Test
	public void testDoPart() throws IOException, ServletException {
		final Map<String, String> parameters = new LinkedHashMap<String, String>();
		// partParameter null: monitoring principal
		parameters.put(PART_PARAMETER, null);
		doPart(parameters);
		parameters.put(FORMAT_PARAMETER, "pdf");
		doPart(parameters);
		parameters.remove(FORMAT_PARAMETER);
		parameters.put(PART_PARAMETER, WEB_XML_PART);
		doPart(parameters);
		parameters.put(PART_PARAMETER, POM_XML_PART);
		doPart(parameters);
		parameters.put(PART_PARAMETER, JNDI_PART);
		doPart(parameters);
		parameters.put(PATH_PARAMETER, "/");
		doPart(parameters);
		parameters.remove(PATH_PARAMETER);
		parameters.put(PART_PARAMETER, MBEANS_PART);
		doPart(parameters);
		parameters.remove(PART_PARAMETER);
		parameters.put(JMX_VALUE, "JMImplementation:type=MBeanServerDelegate.MBeanServerId");
		doPart(parameters);
		parameters.remove(JMX_VALUE);
		parameters.put(PART_PARAMETER, CURRENT_REQUESTS_PART);
		doPart(parameters);
		parameters.put(PART_PARAMETER, PROCESSES_PART);
		doPart(parameters);
		parameters.put(FORMAT_PARAMETER, "pdf");
		doPart(parameters);
		parameters.remove(FORMAT_PARAMETER);
		final TestDatabaseInformations testDatabaseInformations = new TestDatabaseInformations();
		testDatabaseInformations.setUp();
		try {
			parameters.put(PART_PARAMETER, DATABASE_PART);
			doPart(parameters);
			parameters.put(REQUEST_PARAMETER, "0");
			doPart(parameters);
		} finally {
			testDatabaseInformations.tearDown();
		}
		parameters.put(PART_PARAMETER, CONNECTIONS_PART);
		doPart(parameters);
		parameters.put(PART_PARAMETER, HEAP_HISTO_PART);
		doPart(parameters);
		parameters.put(PART_PARAMETER, SESSIONS_PART);
		doPart(parameters);
		parameters.put(PART_PARAMETER, CURRENT_REQUESTS_PART);
		doPart(parameters);
		parameters.put(PART_PARAMETER, PROCESSES_PART);
		doPart(parameters);
	}

	/** Test.
	 * @throws ServletException e
	 * @throws IOException e */
	@Test
	public void testDoCompressedSerializable() throws IOException, ServletException {
		final Map<String, String> parameters = new LinkedHashMap<String, String>();
		parameters.put(FORMAT_PARAMETER, "xml");
		// partParameter null: monitoring principal
		parameters.put(PART_PARAMETER, null);
		doPart(parameters);
		parameters.put(PART_PARAMETER, PROCESSES_PART);
		doPart(parameters);
		parameters.put(PART_PARAMETER, COUNTER_SUMMARY_PER_CLASS_PART);
		parameters.put(COUNTER_PARAMETER, Counter.HTTP_COUNTER_NAME);
		doPart(parameters);
		parameters.remove(COUNTER_PARAMETER);
		final TestDatabaseInformations testDatabaseInformations = new TestDatabaseInformations();
		testDatabaseInformations.setUp();
		try {
			parameters.put(PART_PARAMETER, DATABASE_PART);
			doPart(parameters);
			parameters.put(REQUEST_PARAMETER, "0");
			doPart(parameters);
		} finally {
			testDatabaseInformations.tearDown();
		}
		parameters.put(PART_PARAMETER, CONNECTIONS_PART);
		doPart(parameters);
		parameters.put(PART_PARAMETER, HEAP_HISTO_PART);
		doPart(parameters);
		parameters.put(PART_PARAMETER, SESSIONS_PART);
		doPart(parameters);
		parameters.put(PART_PARAMETER, THREADS_PART);
		doPart(parameters);
		parameters.put(PART_PARAMETER, EXPLAIN_PLAN_PART);
		parameters.put(REQUEST_PARAMETER, "select 1 from dual");
		doPart(parameters);
	}

	/** Test.
	 * @throws ServletException e
	 * @throws IOException e */
	@Test
	public void testAction() throws IOException, ServletException {
		final Map<String, String> parameters = new LinkedHashMap<String, String>();
		parameters.put("application", TEST);
		parameters.put(ACTION_PARAMETER, Action.GC.toString());
		doPart(parameters);
		parameters.put(ACTION_PARAMETER, Action.CLEAR_COUNTER.toString());
		parameters.put(COUNTER_PARAMETER, "all");
		doPart(parameters);
		parameters.put(ACTION_PARAMETER, Action.MAIL_TEST.toString());
		doPart(parameters);
		parameters.put(ACTION_PARAMETER, Action.INVALIDATE_SESSION.toString());
		parameters.put(SESSION_ID_PARAMETER, "aSessionId");
		doPart(parameters);
		parameters.put(ACTION_PARAMETER, Action.KILL_THREAD.toString());
		parameters.put(THREAD_ID_PARAMETER, "aThreadId");
		doPart(parameters);
		parameters.put(ACTION_PARAMETER, Action.PAUSE_JOB.toString());
		parameters.put(JOB_ID_PARAMETER, "all");
		doPart(parameters);
		parameters.put(ACTION_PARAMETER, "remove_application");
		doPart(parameters);
	}

	private void doPart(Map<String, String> parameters) throws IOException, ServletException {
		setUp();
		final HttpServletRequest request = createNiceMock(HttpServletRequest.class);
		expect(request.getRequestURI()).andReturn("/test/monitoring").anyTimes();
		if (MBEANS_PART.equals(parameters.get(PART_PARAMETER))) {
			expect(request.getHeaders("Accept-Encoding")).andReturn(
					Collections.enumeration(Collections.singleton("application/gzip"))).anyTimes();
		} else {
			expect(request.getHeaders("Accept-Encoding")).andReturn(
					Collections.enumeration(Collections.singleton("text/html"))).anyTimes();
		}
		Parameters.removeCollectorApplication(TEST);
		expect(request.getParameter("appName")).andReturn(TEST).anyTimes();
		expect(request.getParameter("appUrls")).andReturn(
				"http://localhost/test,http://localhost:8080/test2").anyTimes();
		// un cookie d'une application (qui existe)
		final Cookie[] cookies = { new Cookie("javamelody.application", TEST) };
		expect(request.getCookies()).andReturn(cookies).anyTimes();
		for (final Map.Entry<String, String> entry : parameters.entrySet()) {
			expect(request.getParameter(entry.getKey())).andReturn(entry.getValue()).anyTimes();
		}
		final HttpServletResponse response = createNiceMock(HttpServletResponse.class);
		final FilterServletOutputStream servletOutputStream = new FilterServletOutputStream(
				new ByteArrayOutputStream());
		expect(response.getOutputStream()).andReturn(servletOutputStream).anyTimes();
		InputStream webXmlStream = null;
		try {
			webXmlStream = getClass().getResourceAsStream("/WEB-INF/web.xml");
			InputStream webXmlStream2 = null;
			try {
				webXmlStream2 = context.getResourceAsStream("/WEB-INF/web.xml");
				expect(webXmlStream2).andReturn(webXmlStream).anyTimes();
				final String javamelodyDir = "/META-INF/maven/net.bull.javamelody/";
				final String webapp = javamelodyDir + "javamelody-test-webapp/";
				expect(context.getResourcePaths("/META-INF/maven/")).andReturn(
						Collections.singleton(javamelodyDir)).anyTimes();
				expect(context.getResourcePaths(javamelodyDir)).andReturn(
						Collections.singleton(webapp)).anyTimes();
				expect(context.getResourceAsStream(webapp + "pom.xml")).andReturn(
						getClass().getResourceAsStream("/pom.xml")).anyTimes();
				replay(config);
				replay(context);
				replay(request);
				replay(response);
				collectorServlet.init(config);
				collectorServlet.doPost(request, response);
				collectorServlet.doGet(request, response);
				verify(config);
				verify(context);
				verify(request);
				verify(response);
			} finally {
				if (webXmlStream2 != null) {
					webXmlStream2.close();
				}
			}
		} finally {
			if (webXmlStream != null) {
				webXmlStream.close();
			}
		}
	}

	/** Test. */
	@Test
	public void testMainWinstone() {
		try {
			Main.main(new String[] { "--help" });
		} catch (final Exception e) {
			// cela s'arrête sur le jar winstone qui n'est pas disponible en tests unitaires
			assertNotNull("ok", e);
		}
	}
}
