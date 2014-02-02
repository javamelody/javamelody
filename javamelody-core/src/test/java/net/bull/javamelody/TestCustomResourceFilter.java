/*
 * Copyright 2008-2014 by Emeric Vernat
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

import java.io.IOException;
import java.util.Arrays;
import java.util.Collections;
import java.util.Map;

import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.RequestDispatcher;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * Test unitaire de la classe CustomResourceFilter.
 * @author Emeric Vernat
 */
public class TestCustomResourceFilter { // NOPMD
	private static final String MONITORING_CSS = "monitoring.css";
	private static final Map<String, String> CUSTOM_RESOURCES = Collections.singletonMap(
			MONITORING_CSS, "customMonitoring.css");
	private FilterConfig config;
	private CustomResourceFilter customResourceFilter;

	/**
	 * Initialisation.
	 */
	@Before
	public void setUp() {
		Utils.initialize();
		config = createNiceMock(FilterConfig.class);
		expect(config.getInitParameterNames()).andReturn(
				Collections.enumeration(Arrays.asList(MONITORING_CSS)));
		for (final Map.Entry<String, String> entry : CUSTOM_RESOURCES.entrySet()) {
			expect(config.getInitParameter(entry.getKey())).andReturn(entry.getValue());
		}

		final ServletContext context = createNiceMock(ServletContext.class);
		replay(context);
		Parameters.initialize(context);

		expect(config.getServletContext()).andReturn(context).anyTimes();

		customResourceFilter = new CustomResourceFilter();
	}

	/**
	 * Finalisation.
	 */
	@After
	public void tearDown() {
		if (customResourceFilter != null) {
			customResourceFilter.destroy();
		}
	}

	/**
	 * testRessource.
	 * @throws IOException e
	 * @throws ServletException e
	 */
	@Test
	public void testCustomResource() throws IOException, ServletException {
		doTestResource(MONITORING_CSS);
	}

	/**
	 * testRessource.
	 * @throws IOException e
	 * @throws ServletException e
	 */
	@Test
	public void testResource() throws IOException, ServletException {
		doTestResource("alert.png");
	}

	/**
	 * testRessource.
	 * @throws IOException e
	 * @throws ServletException e
	 */
	@Test
	public void testNoResource() throws IOException, ServletException {
		doTestResource(null);
	}

	private void doTestResource(String resource) throws IOException, ServletException {
		final HttpServletRequest request = createNiceMock(HttpServletRequest.class);
		final RequestDispatcher requestDispatcher = createNiceMock(RequestDispatcher.class);
		final HttpServletResponse response = createNiceMock(HttpServletResponse.class);
		final FilterChain chain = createNiceMock(FilterChain.class);
		expect(request.getParameter("resource")).andReturn(resource);
		if (CUSTOM_RESOURCES.get(resource) != null) {
			expect(request.getRequestDispatcher(CUSTOM_RESOURCES.get(resource))).andReturn(
					requestDispatcher);
		}

		replay(config);
		replay(request);
		replay(response);
		replay(chain);
		replay(requestDispatcher);
		customResourceFilter.init(config);
		customResourceFilter.doFilter(request, response, chain);
		verify(config);
		verify(request);
		verify(response);
		verify(chain);
		verify(requestDispatcher);
	}
}
