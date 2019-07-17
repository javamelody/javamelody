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

import static org.easymock.EasyMock.createNiceMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;
import static org.junit.Assert.assertNotNull;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;

import javax.servlet.ServletConfig;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import net.bull.javamelody.internal.common.Parameters;
import net.bull.javamelody.internal.web.FilterServletOutputStream;

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
		if (collectorServlet != null) {
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
		expect(context
				.getInitParameter(Parameters.PARAMETER_SYSTEM_PREFIX + Parameter.LOG.getCode()))
						.andReturn(TRUE).anyTimes();
		expect(context.getInitParameter(
				Parameters.PARAMETER_SYSTEM_PREFIX + Parameter.ALLOWED_ADDR_PATTERN.getCode()))
						.andReturn("127\\.0\\.0\\.1").anyTimes();
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
			expect(context.getInitParameter(
					Parameters.PARAMETER_SYSTEM_PREFIX + Parameter.ALLOWED_ADDR_PATTERN.getCode()))
							.andReturn(pattern).anyTimes();
			expect(request.getRemoteAddr()).andReturn(REMOTE_ADDR).anyTimes();
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
		final List<String> nullUrl = Arrays.asList((String) null);
		doPost(null, nullUrl, false);
		doPost(null, nullUrl, true);
		doPost(TEST, nullUrl, true);
		doPost(TEST, Arrays.asList("http://localhost:8090/test", "http://localhost:8090/test"),
				true);
		doPost(TEST, Arrays.asList("https://localhost:8090/test", "http://localhost:8090/test"),
				true);
		doPost(TEST, Arrays.asList("ftp://localhost:8090/test"), true);
		doPost(TEST, Arrays.asList("http://une url,pas une url"), true);
	}

	private void doPost(String appName, List<String> appUrlsList, boolean allowed)
			throws IOException, ServletException {
		setUp();
		if (appName != null) {
			Parameters.removeCollectorApplication(appName);
		}
		if (!allowed) {
			expect(context.getInitParameter(
					Parameters.PARAMETER_SYSTEM_PREFIX + Parameter.ALLOWED_ADDR_PATTERN.getCode()))
							.andReturn("none").anyTimes();
		}
		replay(config);
		replay(context);
		collectorServlet.init(config);
		verify(config);
		verify(context);

		for (final String appUrls : appUrlsList) {
			final HttpServletRequest request = createNiceMock(HttpServletRequest.class);
			expect(request.getRequestURI()).andReturn("/test/request").anyTimes();
			// un cookie d'une application (qui n'existe pas)
			final Cookie[] cookies = { new Cookie("javamelody.application", "anothertest") };
			expect(request.getCookies()).andReturn(cookies).anyTimes();
			final HttpServletResponse response = createNiceMock(HttpServletResponse.class);
			final FilterServletOutputStream servletOutputStream = new FilterServletOutputStream(
					new ByteArrayOutputStream());
			expect(response.getOutputStream()).andReturn(servletOutputStream).anyTimes();
			expect(request.getParameter("appName")).andReturn(appName).anyTimes();
			if (!allowed) {
				expect(request.getRemoteAddr()).andReturn(REMOTE_ADDR).anyTimes();
			}
			expect(request.getParameter("appUrls")).andReturn(appUrls).anyTimes();
			replay(request);
			replay(response);
			collectorServlet.doPost(request, response);
			verify(request);
			verify(response);
		}

		for (final String appUrls : appUrlsList) {
			final HttpServletRequest request = createNiceMock(HttpServletRequest.class);
			expect(request.getParameter("action")).andReturn("unregisterNode").anyTimes();

			expect(request.getRequestURI()).andReturn("/test/request").anyTimes();
			// un cookie d'une application (qui n'existe pas)
			final Cookie[] cookies = { new Cookie("javamelody.application", "anothertest") };
			expect(request.getCookies()).andReturn(cookies).anyTimes();
			final HttpServletResponse response = createNiceMock(HttpServletResponse.class);
			final FilterServletOutputStream servletOutputStream = new FilterServletOutputStream(
					new ByteArrayOutputStream());
			expect(response.getOutputStream()).andReturn(servletOutputStream).anyTimes();
			expect(request.getParameter("appName")).andReturn(appName).anyTimes();
			if (!allowed) {
				expect(request.getRemoteAddr()).andReturn(REMOTE_ADDR).anyTimes();
			}
			expect(request.getParameter("appUrls")).andReturn(appUrls).anyTimes();
			replay(request);
			replay(response);
			collectorServlet.doPost(request, response);
			verify(request);
			verify(response);
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

	@Test
	public void testAddCollectorApplication() throws IOException {
		CollectorServlet.addCollectorApplication("test", "http://localhost:8090/test");
		CollectorServlet.removeCollectorApplication("test");
	}
}
