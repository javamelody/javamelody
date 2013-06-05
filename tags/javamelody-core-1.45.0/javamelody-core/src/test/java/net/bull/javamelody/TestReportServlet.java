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
package net.bull.javamelody;

import static net.bull.javamelody.HttpParameters.REQUEST_PARAMETER;
import static org.easymock.EasyMock.createNiceMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.reflect.Field;
import java.util.Arrays;
import java.util.Collections;
import java.util.Map;

import javax.servlet.ServletConfig;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.junit.Before;
import org.junit.Test;

/**
 * Test unitaire de la classe ReportServlet.
 * @author Emeric Vernat
 */
public class TestReportServlet {
	private static final String CONTEXT_PATH = "/test";
	private ReportServlet reportServlet;

	/**
	 * Initialisation.
	 */
	@Before
	public void setUp() {
		Utils.initialize();
		try {
			final Field field = MonitoringFilter.class.getDeclaredField("instanceCreated");
			field.setAccessible(true);
			field.set(null, false);
		} catch (final IllegalAccessException e) {
			throw new IllegalStateException(e);
		} catch (final NoSuchFieldException e) {
			throw new IllegalStateException(e);
		}
		final ServletContext parametersContext = createNiceMock(ServletContext.class);
		expect(parametersContext.getMajorVersion()).andReturn(2).anyTimes();
		expect(parametersContext.getMinorVersion()).andReturn(5).anyTimes();
		expect(parametersContext.getContextPath()).andReturn(CONTEXT_PATH).anyTimes();
		expect(parametersContext.getServletContextName()).andReturn("test webapp").anyTimes();
		expect(parametersContext.getServerInfo()).andReturn("mock").anyTimes();
		replay(parametersContext);
		Parameters.initialize(parametersContext);
		verify(parametersContext);

		final ServletConfig config = createNiceMock(ServletConfig.class);
		final ServletContext context = createNiceMock(ServletContext.class);
		expect(config.getServletContext()).andReturn(context).anyTimes();
		// anyTimes sur getInitParameter car TestJdbcDriver a pu fixer la propriété système à false
		expect(
				context.getInitParameter(Parameters.PARAMETER_SYSTEM_PREFIX
						+ Parameter.DISABLED.getCode())).andReturn(null).anyTimes();
		expect(config.getInitParameter(Parameter.DISABLED.getCode())).andReturn(null).anyTimes();
		expect(context.getMajorVersion()).andReturn(2).anyTimes();
		expect(context.getMinorVersion()).andReturn(5).anyTimes();
		expect(context.getContextPath()).andReturn(CONTEXT_PATH).anyTimes();
		expect(context.getAttribute(ReportServlet.FILTER_CONTEXT_KEY)).andReturn(
				new FilterContext()).anyTimes();
		reportServlet = new ReportServlet();
		replay(config);
		replay(context);
		reportServlet.init(config);
		verify(config);
		verify(context);
	}

	/** Test. */
	@Test
	public void testDestroy() {
		reportServlet.destroy();
	}

	/** Test.
	 * @throws ServletException e
	 * @throws IOException e */
	@Test
	public void testDoGet() throws ServletException, IOException {
		doGet(Collections.<String, String> emptyMap(), true);

		setProperty(Parameter.ALLOWED_ADDR_PATTERN, "256.*");
		try {
			setUp();
			doGet(Collections.<String, String> emptyMap(), false);
			setProperty(Parameter.ALLOWED_ADDR_PATTERN, ".*");
			setUp();
			doGet(Collections.<String, String> emptyMap(), false);
		} finally {
			setProperty(Parameter.ALLOWED_ADDR_PATTERN, null);
		}
	}

	private void doGet(Map<String, String> parameters, boolean checkResultContent)
			throws IOException, ServletException {
		final HttpServletRequest request = createNiceMock(HttpServletRequest.class);
		expect(request.getRequestURI()).andReturn("/test/monitoring").anyTimes();
		expect(request.getRequestURL()).andReturn(new StringBuffer("/test/monitoring")).anyTimes();
		expect(request.getContextPath()).andReturn(CONTEXT_PATH).anyTimes();
		expect(request.getRemoteAddr()).andReturn("here").anyTimes();
		for (final Map.Entry<String, String> entry : parameters.entrySet()) {
			if (REQUEST_PARAMETER.equals(entry.getKey())) {
				expect(request.getHeader(entry.getKey())).andReturn(entry.getValue()).anyTimes();
			} else {
				expect(request.getParameter(entry.getKey())).andReturn(entry.getValue()).anyTimes();
			}
		}
		expect(request.getHeaders("Accept-Encoding")).andReturn(
				Collections.enumeration(Arrays.asList("application/gzip"))).anyTimes();
		final HttpServletResponse response = createNiceMock(HttpServletResponse.class);
		final ByteArrayOutputStream output = new ByteArrayOutputStream();
		expect(response.getOutputStream()).andReturn(new FilterServletOutputStream(output))
				.anyTimes();
		final StringWriter stringWriter = new StringWriter();
		expect(response.getWriter()).andReturn(new PrintWriter(stringWriter)).anyTimes();

		replay(request);
		replay(response);
		reportServlet.doGet(request, response);
		verify(request);
		verify(response);

		if (checkResultContent) {
			assertTrue("result", output.size() != 0 || stringWriter.getBuffer().length() != 0);
		}
	}

	private static void setProperty(Parameter parameter, String value) {
		Utils.setProperty(parameter, value);
	}
}
