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

import net.bull.javamelody.internal.common.HttpParameter;
import net.bull.javamelody.internal.common.Parameters;
import net.bull.javamelody.internal.web.FilterServletOutputStream;

/**
 * Test unitaire de la classe ReportServlet.
 * @author Emeric Vernat
 */
public class TestReportServlet {
	private static final String CONTEXT_PATH = "/test";

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
	}

	/** Test. */
	@Test
	public void testDestroy() {
		new ReportServlet().destroy();
	}

	/** Test.
	 * @throws ServletException e
	 * @throws IOException e */
	@Test
	public void testDoGet() throws ServletException, IOException {
		doGet(Collections.<HttpParameter, String> emptyMap(), true);

		setProperty(Parameter.ALLOWED_ADDR_PATTERN, "256.*");
		try {
			doGet(Collections.<HttpParameter, String> emptyMap(), false);
			setProperty(Parameter.ALLOWED_ADDR_PATTERN, ".*");
			doGet(Collections.<HttpParameter, String> emptyMap(), false);
		} finally {
			setProperty(Parameter.ALLOWED_ADDR_PATTERN, null);
		}
	}

	private void doGet(Map<HttpParameter, String> parameters, boolean checkResultContent)
			throws IOException, ServletException {
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
		expect(context.getInitParameter(
				Parameters.PARAMETER_SYSTEM_PREFIX + Parameter.DISABLED.getCode())).andReturn(null)
						.anyTimes();
		expect(config.getInitParameter(Parameter.DISABLED.getCode())).andReturn(null).anyTimes();
		expect(context.getMajorVersion()).andReturn(2).anyTimes();
		expect(context.getMinorVersion()).andReturn(5).anyTimes();
		expect(context.getContextPath()).andReturn(CONTEXT_PATH).anyTimes();
		final FilterContext filterContext = new FilterContext("Classic");
		expect(context.getAttribute(ReportServlet.FILTER_CONTEXT_KEY)).andReturn(filterContext)
				.anyTimes();
		final ReportServlet reportServlet = new ReportServlet();
		replay(config);
		replay(context);
		reportServlet.init(config);
		verify(config);
		verify(context);

		final HttpServletRequest request = createNiceMock(HttpServletRequest.class);
		expect(request.getRequestURI()).andReturn("/test/monitoring").anyTimes();
		expect(request.getRequestURL()).andReturn(new StringBuffer("/test/monitoring")).anyTimes();
		expect(request.getContextPath()).andReturn(CONTEXT_PATH).anyTimes();
		expect(request.getRemoteAddr()).andReturn("here").anyTimes();
		for (final Map.Entry<HttpParameter, String> entry : parameters.entrySet()) {
			if (HttpParameter.REQUEST == entry.getKey()) {
				expect(request.getHeader(entry.getKey().getName())).andReturn(entry.getValue())
						.anyTimes();
			} else {
				expect(request.getParameter(entry.getKey().getName())).andReturn(entry.getValue())
						.anyTimes();
			}
		}
		expect(request.getHeaders("Accept-Encoding"))
				.andReturn(Collections.enumeration(Arrays.asList("application/gzip"))).anyTimes();
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
		filterContext.destroy();
	}

	private static void setProperty(Parameter parameter, String value) {
		Utils.setProperty(parameter, value);
	}
}
