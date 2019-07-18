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
import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.util.Locale;

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
 * Test unitaire pour PluginMonitoringFilter.
 * @author Emeric Vernat
 */
public class TestPluginMonitoringFilter {
	private static final String TEST_REQUEST = "/request";
	private static final String CONTEXT_PATH = "/test";
	private PluginMonitoringFilter pluginMonitoringFilter;

	public static class MyPluginMonitoringFilter extends PluginMonitoringFilter {
		// ras
	}

	@Before
	public void init() throws ServletException {
		pluginMonitoringFilter = new MyPluginMonitoringFilter();
		final FilterConfig config = createNiceMock(FilterConfig.class);
		final ServletContext context = createNiceMock(ServletContext.class);
		expect(config.getServletContext()).andReturn(context).anyTimes();
		//		expect(config.getFilterName()).andReturn(FILTER_NAME).anyTimes();
		expect(context.getMajorVersion()).andReturn(2).anyTimes();
		expect(context.getMinorVersion()).andReturn(5).anyTimes();
		expect(context.getContextPath()).andReturn(CONTEXT_PATH).anyTimes();
		expect(context.getServerInfo()).andReturn("mockJetty").anyTimes();
		replay(config);
		replay(context);
		pluginMonitoringFilter.init(config);
		verify(config);
		verify(context);
	}

	@After
	public void destroy() {
		pluginMonitoringFilter.destroy();
		pluginMonitoringFilter = null;
	}

	@Test
	public void testNoHttp() throws IOException, ServletException {
		final ServletRequest request = createNiceMock(ServletRequest.class);
		final ServletResponse response = createNiceMock(ServletResponse.class);
		final FilterChain chain = createNiceMock(FilterChain.class);
		replay(request);
		replay(response);
		replay(chain);
		pluginMonitoringFilter.doFilter(request, response, chain);
		verify(request);
		verify(response);
		verify(chain);
	}

	@Test
	public void test() throws IOException, ServletException {
		final HttpServletRequest request = createNiceMock(HttpServletRequest.class);
		final HttpServletResponse response = createNiceMock(HttpServletResponse.class);
		final FilterChain chain = createNiceMock(FilterChain.class);
		expect(request.getRequestURI()).andReturn(CONTEXT_PATH + TEST_REQUEST).anyTimes();
		expect(request.getContextPath()).andReturn(CONTEXT_PATH).anyTimes();
		replay(request);
		replay(response);
		replay(chain);
		pluginMonitoringFilter.doFilter(request, response, chain);
		verify(request);
		verify(response);
		verify(chain);

		pluginMonitoringFilter.unregisterInvalidatedSessions();
	}

	@Test
	public void testWithRequestSessionIdValid() throws IOException, ServletException {
		final int sessionCount = SessionListener.getSessionCount();
		final HttpServletRequest request = createNiceMock(HttpServletRequest.class);
		final HttpServletResponse response = createNiceMock(HttpServletResponse.class);
		final FilterChain chain = createNiceMock(FilterChain.class);
		expect(request.getRequestURI()).andReturn(CONTEXT_PATH + TEST_REQUEST).anyTimes();
		expect(request.getContextPath()).andReturn(CONTEXT_PATH).anyTimes();

		expect(request.isRequestedSessionIdValid()).andReturn(true).anyTimes();
		expect(request.getLocale()).andReturn(Locale.FRANCE).anyTimes();

		replay(request);
		replay(response);
		replay(chain);
		pluginMonitoringFilter.doFilter(request, response, chain);
		assertEquals("sessionCount", sessionCount, SessionListener.getSessionCount());
		verify(request);
		verify(response);
		verify(chain);
	}

	@Test
	public void testWithSessionCreated() throws IOException, ServletException {
		final int sessionCount = SessionListener.getSessionCount();
		final HttpServletRequest request = createNiceMock(HttpServletRequest.class);
		final HttpServletResponse response = createNiceMock(HttpServletResponse.class);
		final FilterChain chain = createNiceMock(FilterChain.class);
		expect(request.getRequestURI()).andReturn(CONTEXT_PATH + TEST_REQUEST).anyTimes();
		expect(request.getContextPath()).andReturn(CONTEXT_PATH).anyTimes();

		final HttpSession session = createNiceMock(HttpSession.class);
		expect(request.isRequestedSessionIdValid()).andReturn(true).anyTimes();
		expect(request.getSession(false)).andReturn(session).anyTimes();
		expect(request.getLocale()).andReturn(Locale.FRANCE).anyTimes();
		expect(session.getId()).andReturn("1234567890").anyTimes();

		replay(request);
		replay(response);
		replay(chain);
		replay(session);
		pluginMonitoringFilter.doFilter(request, response, chain);
		assertEquals("sessionCount", sessionCount + 1, SessionListener.getSessionCount());
		pluginMonitoringFilter.doFilter(request, response, chain);
		assertEquals("sessionCount", sessionCount + 1, SessionListener.getSessionCount());
		verify(request);
		verify(response);
		verify(chain);
		verify(session);
	}

	@Test
	public void testWithSessionDestroyed() throws IOException, ServletException {
		final int sessionCount = SessionListener.getSessionCount();

		pluginMonitoringFilter.unregisterInvalidatedSessions();

		assertEquals("sessionCount", sessionCount, SessionListener.getSessionCount());

		final HttpServletRequest request2 = createNiceMock(HttpServletRequest.class);
		final HttpServletResponse response2 = createNiceMock(HttpServletResponse.class);
		final FilterChain chain2 = createNiceMock(FilterChain.class);
		expect(request2.getRequestURI()).andReturn(CONTEXT_PATH + TEST_REQUEST).anyTimes();
		expect(request2.getContextPath()).andReturn(CONTEXT_PATH).anyTimes();

		final HttpSession session2 = createNiceMock(HttpSession.class);
		expect(request2.isRequestedSessionIdValid()).andReturn(true).anyTimes();
		expect(request2.getSession(false)).andReturn(session2).anyTimes();
		expect(request2.getLocale()).andReturn(Locale.FRANCE).anyTimes();
		expect(session2.getId()).andReturn("0987654321").anyTimes();
		expect(session2.getLastAccessedTime()).andThrow(new IllegalStateException("test"))
				.anyTimes();

		replay(request2);
		replay(response2);
		replay(chain2);
		replay(session2);
		pluginMonitoringFilter.doFilter(request2, response2, chain2);
		assertEquals("sessionCount", sessionCount, SessionListener.getSessionCount());
		pluginMonitoringFilter.doFilter(request2, response2, chain2);
		assertEquals("sessionCount", sessionCount, SessionListener.getSessionCount());
		pluginMonitoringFilter.unregisterInvalidatedSessions();
		assertEquals("sessionCount", sessionCount, SessionListener.getSessionCount());
		verify(request2);
		verify(response2);
		verify(chain2);
		verify(session2);
		PluginMonitoringFilter.logForDebug("test");
	}
}
