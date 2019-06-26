/*
 * Copyright 2019 by Roland Praml
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
package net.bull.javamelody.internal.web;

import static org.easymock.EasyMock.createNiceMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Timer;

import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import net.bull.javamelody.Utils;
import net.bull.javamelody.internal.common.HttpParameter;
import net.bull.javamelody.internal.common.Parameters;
import net.bull.javamelody.internal.model.Collector;
import net.bull.javamelody.internal.model.Counter;
import net.bull.javamelody.internal.model.JRobin;
import net.bull.javamelody.internal.model.JavaInformations;

/**
 * Tests for the prometheus code path.
 *
 * @author Roland Praml, FOCONIS AG
 *
 */
public class TestPrometheus {

	private HttpServletRequest request;
	private HttpServletResponse response;
	private Collector collector;
	private List<JavaInformations> javaInformationsList;

	/**
	 * Initialisation.
	 * @throws IOException e
	 */
	@Before
	public void setUp() throws IOException {
		Utils.initialize();
		JRobin.initBackendFactory(new Timer(getClass().getSimpleName(), true));
		final ServletContext context = createNiceMock(ServletContext.class);
		expect(context.getMajorVersion()).andReturn(2).anyTimes();
		expect(context.getMinorVersion()).andReturn(5).anyTimes();
		expect(context.getServletContextName()).andReturn("test webapp").anyTimes();
		expect(context.getServerInfo()).andReturn("mockJetty").anyTimes();
		expect(context.getContextPath()).andReturn("/test").anyTimes();
		replay(context);
		Parameters.initialize(context);

		final Counter sqlCounter = new Counter("sql", "db.png");
		collector = new Collector("test collector", Arrays.asList(sqlCounter));
		javaInformationsList = new ArrayList<JavaInformations>();
		javaInformationsList.add(new JavaInformations(null, false));

		collector.collectWithoutErrors(javaInformationsList);

		request = createNiceMock(HttpServletRequest.class);
		response = createNiceMock(HttpServletResponse.class);
	}

	/** After. */
	@After
	public void tearDown() {
		JRobin.stop();
	}

	/**
	 * Sends the request to the monitoringcontroller and returns the response.
	 * @return the response of all PrometheusControllers
	 * @throws IOException e
	 * @throws ServletException e
	 */
	private String doReport() throws IOException, ServletException {
		final StringWriter stringWriter = new StringWriter();
		expect(response.getWriter()).andReturn(new PrintWriter(stringWriter)).anyTimes();

		replay(request);
		replay(response);
		new MonitoringController(collector, null).doReport(request, response, javaInformationsList);
		verify(request);
		verify(response);

		String result = stringWriter.toString();
		return result;
	}

	/** 
	 * Test 
	 * @throws Exception e 
	 */
	@Test
	public void testPrometheus() throws Exception {

		expect(HttpParameter.FORMAT.getParameterFrom(request)).andReturn("prometheus");

		String result = doReport();

		// we use the first metric, which should always be the memory used metric and test
		// for proper formatting (line breaks etc.)
		assertTrue("test metric format",
				result.startsWith("# HELP javamelody_memory_used_bytes used memory in bytes\n"
						+ "# TYPE javamelody_memory_used_bytes gauge\n"
						+ "javamelody_memory_used_bytes "));
		// we do not expect last values here
		assertFalse("last values found", result.contains("javamelody_last_value_"));
	}

	/** 
	 * Test 
	 * @throws Exception e 
	 */
	@Test
	public void testPrometheusLastValue() throws Exception {

		expect(HttpParameter.FORMAT.getParameterFrom(request)).andReturn("prometheus");
		expect(request.getParameter("includeLastValue")).andReturn("true");

		String result = doReport();

		assertTrue("last values not found", result.contains("javamelody_last_value_"));

	}

	/** 
	 * Test 
	 * @throws Exception e 
	 */
	@Test
	public void testPrometheusServiceLoaded() throws Exception {

		expect(HttpParameter.FORMAT.getParameterFrom(request)).andReturn("prometheus");

		String result = doReport();

		assertTrue("custom1_answer not found", result.contains("custom1_answer 42"));
		assertTrue("diskspace a", result.contains("custom2_diskspace{letter=\"a\"} 1.44"));
		assertTrue("diskspace c", result.contains("custom2_diskspace{letter=\"c\"} 300000"));

	}

}
