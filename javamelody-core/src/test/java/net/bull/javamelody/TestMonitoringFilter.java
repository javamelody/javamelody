/*
 * Copyright 2008-2009 by Emeric Vernat, Bull
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

import static org.easymock.EasyMock.createNiceMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collections;

import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.junit.Before;
import org.junit.Test;

/**
 * Test unitaire de la classe MonitoringFilter.
 * @author Emeric Vernat
 */
public class TestMonitoringFilter {
	private FilterConfig config;
	private ServletContext context;
	private MonitoringFilter monitoringFilter;

	/**
	 * Initialisation.
	 */
	@Before
	public void setUp() {
		config = createNiceMock(FilterConfig.class);
		context = createNiceMock(ServletContext.class);
		expect(config.getServletContext()).andReturn(context).anyTimes();
		// anyTimes sur getInitParameter car TestJdbcDriver a pu fixer la propriété système à false
		expect(
				context.getInitParameter(Parameters.PARAMETER_SYSTEM_PREFIX
						+ Parameter.DISABLED.getCode())).andReturn(null).anyTimes();
		expect(config.getInitParameter(Parameter.DISABLED.getCode())).andReturn(null).anyTimes();
		expect(context.getMajorVersion()).andReturn(2).anyTimes();
		expect(context.getMinorVersion()).andReturn(5).anyTimes();
		expect(context.getContextPath()).andReturn("/test").anyTimes();
		monitoringFilter = new MonitoringFilter();
	}

	/** Test.
	 * @throws ServletException e */
	@Test
	public void testInit() throws ServletException {
		setUp();

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
		setUp();

		final HttpServletRequest request = createNiceMock(HttpServletRequest.class);
		expect(request.getRemoteAddr()).andReturn("127.0.0.1");
		expect(request.getRequestURI()).andReturn("/test/request");
		expect(request.getContextPath()).andReturn("/test");
		expect(request.getQueryString()).andReturn("param1=1");
		expect(request.getMethod()).andReturn("GET");

		replay(config);
		replay(context);
		monitoringFilter.init(config);
		monitoringFilter.log(request, "test", 1000, false, 10000);
		verify(config);
		verify(context);
	}

	/** Test.
	 * @throws ServletException e 
	 * @throws IOException e */
	@Test
	public void testdoFilter() throws ServletException, IOException {
		setUp();

		final HttpServletRequest request = createNiceMock(HttpServletRequest.class);
		expect(request.getRequestURI()).andReturn("/test/request").anyTimes();
		expect(request.getContextPath()).andReturn("/test").anyTimes();
		expect(request.getQueryString()).andReturn("param1=1");
		expect(request.getMethod()).andReturn("GET");
		final HttpServletResponse response = createNiceMock(HttpServletResponse.class);
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
	}

	/** Test.
	 * @throws ServletException e 
	 * @throws IOException e */
	@Test
	public void testdoMonitoring() throws ServletException, IOException {
		setUp();

		final HttpServletRequest request = createNiceMock(HttpServletRequest.class);
		expect(request.getRequestURI()).andReturn("/test/monitoring").anyTimes();
		expect(request.getContextPath()).andReturn("/test").anyTimes();
		expect(request.getHeaders("Accept-Encoding")).andReturn(
				Collections.enumeration(Arrays.asList("application/gzip")));
		final HttpServletResponse response = createNiceMock(HttpServletResponse.class);
		final ByteArrayOutputStream output = new ByteArrayOutputStream();
		expect(response.getOutputStream()).andReturn(new FilterServletOutputStream(output))
				.anyTimes();
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

		assertTrue("result", output.size() != 0);
	}

	// on ne teste pas MonitoringFilter.destroy car si le timer de JRobin est arrêté,
	// on ne peut plus faire les autres tests
}
