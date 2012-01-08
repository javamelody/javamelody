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
package net.bull.javamelody; // NOPMD

import static org.easymock.EasyMock.createNiceMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.reflect.Field;
import java.util.Arrays;
import java.util.Collections;

import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * Test unitaire de la classe JiraMonitoringFilter.
 * @author Emeric Vernat
 */
public class TestJiraMonitoringFilter { // NOPMD
	private static final String FILTER_NAME = "monitoring";
	private static final String CONTEXT_PATH = "/test";
	private static final String TRUE = "true";
	private FilterConfig config;
	private ServletContext context;
	private JiraMonitoringFilter jiraMonitoringFilter;

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
		try {
			final Field field = MonitoringFilter.class.getDeclaredField("instanceCreated");
			field.setAccessible(true);
			field.set(null, false);
		} catch (final IllegalAccessException e) {
			throw new IllegalStateException(e);
		} catch (final NoSuchFieldException e) {
			throw new IllegalStateException(e);
		}
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
		expect(context.getServerInfo()).andReturn("mockJetty").anyTimes();
		expect(context.getContextPath()).andReturn(CONTEXT_PATH).anyTimes();
		jiraMonitoringFilter = new JiraMonitoringFilter();
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
		if (jiraMonitoringFilter != null) {
			jiraMonitoringFilter.destroy();
		}
	}

	/** Test.
	 * @throws ServletException e
	 * @throws IOException e */
	@Test
	public void testNoHttp() throws ServletException, IOException {
		// non http
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
	}

	/** Test.
	 * @throws ServletException e
	 * @throws IOException e */
	@Test
	public void testNoSession() throws ServletException, IOException {
		// pas de session
		doJiraFilter(null);
	}

	/** Test.
	 * @throws ServletException e
	 * @throws IOException e */
	@Test
	public void testValidSession() throws ServletException, IOException {
		// session valide dans PluginMonitoringFilter
		final HttpSession session = createNiceMock(HttpSession.class);
		expect(session.getId()).andReturn("sessionId").anyTimes();
		doJiraFilter(session);
	}

	/** Test.
	 * @throws ServletException e
	 * @throws IOException e */
	@Test
	public void testInvalidatedSession() throws ServletException, IOException {
		jiraMonitoringFilter.unregisterInvalidatedSessions();

		// session invalidée dans PluginMonitoringFilter
		final HttpSession session = createNiceMock(HttpSession.class);
		expect(session.getId()).andReturn("sessionId2").anyTimes();
		expect(session.getCreationTime()).andThrow(new IllegalStateException("invalidated"))
				.anyTimes();
		doJiraFilter(session);
	}

	private void doJiraFilter(HttpSession session) throws IOException, ServletException {
		final HttpServletRequest request = createNiceMock(HttpServletRequest.class);
		expect(request.getRequestURI()).andReturn("/test/monitoring").anyTimes();
		expect(request.getContextPath()).andReturn(CONTEXT_PATH).anyTimes();
		expect(request.getHeaders("Accept-Encoding")).andReturn(
				Collections.enumeration(Arrays.asList("text/html"))).anyTimes();
		if (session != null) {
			expect(request.isRequestedSessionIdValid()).andReturn(Boolean.TRUE).anyTimes();
			expect(request.getSession(false)).andReturn(session).anyTimes();
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
		if (session != null) {
			replay(session);
		}
		jiraMonitoringFilter.init(config);
		jiraMonitoringFilter.doFilter(request, response, chain);
		verify(config);
		verify(context);
		verify(request);
		verify(response);
		verify(chain);
		if (session != null) {
			verify(session);
		}
	}
}
