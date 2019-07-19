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

import net.bull.javamelody.internal.common.HttpParameter;
import net.bull.javamelody.internal.common.HttpPart;
import net.bull.javamelody.internal.common.Parameters;
import net.bull.javamelody.internal.model.Action;
import net.bull.javamelody.internal.model.TestDatabaseInformations;
import net.bull.javamelody.internal.model.TransportFormat;
import net.bull.javamelody.internal.web.FilterServletOutputStream;

/**
 * Test unitaire de la classe CollectorServlet.
 * @author Emeric Vernat
 */
public class TestCollectorServletWithParts {
	private static final String TRUE = "true";
	private static final String TEST = "test";
	private CollectorServlet collectorServlet;

	/**
	 * Initialisation.
	 * @throws IOException e
	 * @throws ServletException e
	 */
	@Before
	public void setUp() throws IOException, ServletException {
		tearDown();
		Utils.initialize();
		Utils.setProperty(Parameters.PARAMETER_SYSTEM_PREFIX + "mockLabradorRetriever", TRUE);
		Utils.setProperty(Parameter.SYSTEM_ACTIONS_ENABLED, TRUE);
		final ServletConfig config = createNiceMock(ServletConfig.class);
		final ServletContext context = createNiceMock(ServletContext.class);
		expect(config.getServletContext()).andReturn(context).anyTimes();
		collectorServlet = new CollectorServlet();
		InputStream webXmlStream = null;
		try {
			webXmlStream = getClass().getResourceAsStream("/WEB-INF/web.xml");
			InputStream webXmlStream2 = null;
			try {
				webXmlStream2 = context.getResourceAsStream("/WEB-INF/web.xml");
				expect(webXmlStream2).andReturn(webXmlStream).anyTimes();
				final String javamelodyDir = "/META-INF/maven/net.bull.javamelody/";
				final String webapp = javamelodyDir + "javamelody-test-webapp/";
				expect(context.getResourcePaths("/META-INF/maven/"))
						.andReturn(Collections.singleton(javamelodyDir)).anyTimes();
				expect(context.getResourcePaths(javamelodyDir))
						.andReturn(Collections.singleton(webapp)).anyTimes();
				expect(context.getResourceAsStream(webapp + "pom.xml"))
						.andReturn(getClass().getResourceAsStream("/pom.xml")).anyTimes();
				replay(config);
				replay(context);
				collectorServlet.init(config);
				verify(config);
				verify(context);
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

	/**
	 * Terminaison.
	 * @throws IOException e
	 */
	@After
	public void tearDown() throws IOException {
		if (collectorServlet != null) {
			collectorServlet.destroy();
		}
		Parameters.removeCollectorApplication(TEST);
	}

	/** Test.
	 * @throws ServletException e
	 * @throws IOException e */
	@Test
	public void testDoPart() throws IOException, ServletException {
		final Map<HttpParameter, String> parameters = new LinkedHashMap<HttpParameter, String>();
		// partParameter null: monitoring principal
		parameters.put(HttpParameter.PART, null);
		doPart(parameters);
		parameters.put(HttpParameter.FORMAT, "pdf");
		doPart(parameters);
		parameters.remove(HttpParameter.FORMAT);
		parameters.put(HttpParameter.PART, HttpPart.POM_XML.getName());
		doPart(parameters);
		parameters.put(HttpParameter.PART, HttpPart.CURRENT_REQUESTS.getName());
		doPart(parameters);
		parameters.put(HttpParameter.FORMAT, "pdf");
		doPart(parameters);
		parameters.remove(HttpParameter.FORMAT);
		parameters.put(HttpParameter.PART, HttpPart.CURRENT_REQUESTS.getName());
		doPart(parameters);
		parameters.put(HttpParameter.PART, HttpPart.DEPENDENCIES.getName());
		doPart(parameters);
		parameters.put(HttpParameter.PART, HttpPart.CACHE_KEYS.getName());
		doPart(parameters);
		parameters.put(HttpParameter.PART, HttpPart.JCACHE_KEYS.getName());
		doPart(parameters);
		parameters.put(HttpParameter.PART, HttpPart.SOURCE.getName());
		parameters.put(HttpParameter.CLASS, "java.lang.String");
		doPart(parameters);
		parameters.remove(HttpParameter.CLASS);
		parameters.put(HttpParameter.PART, HttpPart.CRASHES.getName());
		doPart(parameters);
		parameters.put(HttpParameter.PATH, "unknown");
		doPart(parameters);
		parameters.remove(HttpParameter.PATH);
	}

	/** Test.
	 * @throws ServletException e
	 * @throws IOException e */
	@Test
	public void testDoPartForSystemActions() throws IOException, ServletException {
		final Map<HttpParameter, String> parameters = new LinkedHashMap<HttpParameter, String>();
		parameters.put(HttpParameter.PART, HttpPart.WEB_XML.getName());
		doPart(parameters);
		parameters.put(HttpParameter.PART, HttpPart.JNDI.getName());
		doPart(parameters);
		parameters.put(HttpParameter.PATH, "/");
		doPart(parameters);
		parameters.remove(HttpParameter.PATH);
		parameters.put(HttpParameter.PART, HttpPart.MBEANS.getName());
		doPart(parameters);
		parameters.remove(HttpParameter.PART);
		parameters.put(HttpParameter.JMX_VALUE,
				"JMImplementation:type=MBeanServerDelegate.MBeanServerId");
		doPart(parameters);
		parameters.remove(HttpParameter.JMX_VALUE);
		parameters.put(HttpParameter.PART, HttpPart.PROCESSES.getName());
		doPart(parameters);
		parameters.put(HttpParameter.FORMAT, "pdf");
		doPart(parameters);
		parameters.remove(HttpParameter.FORMAT);
		TestDatabaseInformations.initJdbcDriverParameters();
		parameters.put(HttpParameter.PART, HttpPart.DATABASE.getName());
		doPart(parameters);
		parameters.put(HttpParameter.REQUEST, "0");
		doPart(parameters);
		parameters.put(HttpParameter.PART, HttpPart.CONNECTIONS.getName());
		doPart(parameters);
		parameters.put(HttpParameter.PART, HttpPart.HEAP_HISTO.getName());
		doPart(parameters);
		parameters.put(HttpParameter.PART, HttpPart.SESSIONS.getName());
		doPart(parameters);
		parameters.put(HttpParameter.PART, HttpPart.PROCESSES.getName());
		doPart(parameters);
		parameters.put(HttpParameter.PART, HttpPart.SPRING_BEANS.getName());
		doPart(parameters);
	}

	/** Test.
	 * @throws ServletException e
	 * @throws IOException e */
	@Test
	public void testDoCompressedSerializable() throws IOException, ServletException {
		final Map<HttpParameter, String> parameters = new LinkedHashMap<HttpParameter, String>();
		parameters.put(HttpParameter.FORMAT, "xml");
		// partParameter null: monitoring principal
		parameters.put(HttpParameter.PART, null);
		doPart(parameters);
		parameters.put(HttpParameter.PART, HttpPart.APPLICATIONS.getName());
		doPart(parameters);
		parameters.put(HttpParameter.PART, HttpPart.COUNTER_SUMMARY_PER_CLASS.getName());
		parameters.put(HttpParameter.COUNTER, "services");
		doPart(parameters);
		parameters.remove(HttpParameter.COUNTER);
		TestDatabaseInformations.initJdbcDriverParameters();
		parameters.put(HttpParameter.PART, HttpPart.THREADS.getName());
		doPart(parameters);
		parameters.put(HttpParameter.PART, HttpPart.CURRENT_REQUESTS.getName());
		doPart(parameters);
		parameters.put(HttpParameter.WIDTH, "80");
		parameters.put(HttpParameter.HEIGHT, "80");
		parameters.put(HttpParameter.PART, HttpPart.JROBINS.getName());
		doPart(parameters);
		parameters.put(HttpParameter.GRAPH, "cpu");
		parameters.put(HttpParameter.PART, HttpPart.JROBINS.getName());
		doPart(parameters);
		parameters.remove(HttpParameter.GRAPH);
		parameters.put(HttpParameter.PART, HttpPart.OTHER_JROBINS.getName());
		doPart(parameters);
		parameters.remove(HttpParameter.WIDTH);
		parameters.remove(HttpParameter.HEIGHT);
		parameters.put(HttpParameter.PART, HttpPart.EXPLAIN_PLAN.getName());
		parameters.put(HttpParameter.REQUEST, "select 1 from dual");
		doPart(parameters);
		parameters.remove(HttpParameter.REQUEST);
	}

	/** Test.
	 * @throws ServletException e
	 * @throws IOException e */
	@Test
	public void testDoCompressedSerializableForSystemActions()
			throws IOException, ServletException {
		final Map<HttpParameter, String> parameters = new LinkedHashMap<HttpParameter, String>();
		parameters.put(HttpParameter.FORMAT, "xml");
		parameters.put(HttpParameter.PART, HttpPart.PROCESSES.getName());
		doPart(parameters);
		parameters.put(HttpParameter.PART, HttpPart.JNDI.getName());
		doPart(parameters);
		parameters.put(HttpParameter.PART, HttpPart.MBEANS.getName());
		doPart(parameters);
		TestDatabaseInformations.initJdbcDriverParameters();
		parameters.put(HttpParameter.PART, HttpPart.DATABASE.getName());
		doPart(parameters);
		parameters.put(HttpParameter.REQUEST, "0");
		doPart(parameters);
		parameters.put(HttpParameter.PART, HttpPart.CONNECTIONS.getName());
		doPart(parameters);
		parameters.put(HttpParameter.PART, HttpPart.HEAP_HISTO.getName());
		doPart(parameters);
		parameters.put(HttpParameter.PART, HttpPart.SESSIONS.getName());
		doPart(parameters);
		parameters.put(HttpParameter.PART, HttpPart.JVM.getName());
		doPart(parameters);
		parameters.put(HttpParameter.PART, HttpPart.HOTSPOTS.getName());
		doPart(parameters);
		parameters.put(HttpParameter.PART, HttpPart.CRASHES.getName());
		doPart(parameters);
	}

	/** Test.
	 * @throws ServletException e
	 * @throws IOException e */
	@Test
	public void testAction() throws IOException, ServletException {
		final Map<HttpParameter, String> parameters = new LinkedHashMap<HttpParameter, String>();
		parameters.put(HttpParameter.APPLICATION, TEST);
		parameters.put(HttpParameter.ACTION, Action.GC.toString());
		doPart(parameters);
		parameters.put(HttpParameter.ACTION, Action.CLEAR_COUNTER.toString());
		parameters.put(HttpParameter.COUNTER, "all");
		doPart(parameters);
		parameters.put(HttpParameter.FORMAT, TransportFormat.SERIALIZED.getCode());
		doPart(parameters);
		parameters.remove(HttpParameter.FORMAT);
		parameters.put(HttpParameter.ACTION, Action.MAIL_TEST.toString());
		doPart(parameters);
		parameters.put(HttpParameter.ACTION, Action.PURGE_OBSOLETE_FILES.toString());
		doPart(parameters);
		parameters.put(HttpParameter.ACTION, Action.INVALIDATE_SESSION.toString());
		parameters.put(HttpParameter.SESSION_ID, "aSessionId");
		doPart(parameters);
		parameters.put(HttpParameter.ACTION, Action.KILL_THREAD.toString());
		parameters.put(HttpParameter.THREAD_ID, "aThreadId");
		doPart(parameters);
		parameters.put(HttpParameter.ACTION, Action.PAUSE_JOB.toString());
		parameters.put(HttpParameter.JOB_ID, "all");
		doPart(parameters);
		parameters.put(HttpParameter.ACTION, Action.CLEAR_CACHE.toString());
		parameters.put(HttpParameter.CACHE_ID, "aCacheId");
		doPart(parameters);
		parameters.put(HttpParameter.ACTION, Action.CLEAR_CACHE_KEY.toString());
		parameters.put(HttpParameter.CACHE_ID, "aCacheId");
		parameters.put(HttpParameter.CACHE_KEY, "aCacheKey");
		doPart(parameters);
		parameters.put(HttpParameter.ACTION, "remove_application");
		doPart(parameters);
	}

	private void doPart(Map<HttpParameter, String> parameters)
			throws IOException, ServletException {
		final HttpServletRequest request = createNiceMock(HttpServletRequest.class);
		expect(request.getRequestURI()).andReturn("/test/monitoring").anyTimes();
		final ServletContext servletContext = createNiceMock(ServletContext.class);
		expect(servletContext.getServerInfo()).andReturn("Mock").anyTimes();
		if (HttpPart.MBEANS.getName().equals(parameters.get(HttpParameter.PART))) {
			expect(request.getHeaders("Accept-Encoding"))
					.andReturn(Collections.enumeration(Collections.singleton("application/gzip")))
					.anyTimes();
		} else {
			expect(request.getHeaders("Accept-Encoding"))
					.andReturn(Collections.enumeration(Collections.singleton("text/html")))
					.anyTimes();
		}
		Parameters.removeCollectorApplication(TEST);
		expect(request.getParameter("appName")).andReturn(TEST).anyTimes();
		expect(request.getParameter("appUrls"))
				.andReturn("http://localhost/test,http://localhost:8080/test2").anyTimes();
		// un cookie d'une application (qui existe)
		final Cookie[] cookies = { new Cookie("javamelody.application", TEST) };
		expect(request.getCookies()).andReturn(cookies).anyTimes();
		for (final Map.Entry<HttpParameter, String> entry : parameters.entrySet()) {
			expect(entry.getKey().getParameterFrom(request)).andReturn(entry.getValue()).anyTimes();
		}
		final HttpServletResponse response = createNiceMock(HttpServletResponse.class);
		final FilterServletOutputStream servletOutputStream = new FilterServletOutputStream(
				new ByteArrayOutputStream());
		expect(response.getOutputStream()).andReturn(servletOutputStream).anyTimes();
		replay(request);
		replay(response);
		replay(servletContext);
		Parameters.initialize(servletContext);
		collectorServlet.doPost(request, response);
		collectorServlet.doGet(request, response);
		verify(request);
		verify(response);
		verify(servletContext);
	}
}
