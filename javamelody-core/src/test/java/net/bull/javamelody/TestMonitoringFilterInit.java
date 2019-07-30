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

import java.io.IOException;
import java.lang.reflect.Field;

import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import net.bull.javamelody.internal.common.Parameters;

/**
 * Test unitaire de la classe MonitoringFilter.
 * @author Emeric Vernat
 */
public class TestMonitoringFilterInit {
	private static final String FILTER_NAME = "monitoring";
	private FilterConfig config;
	private ServletContext context;
	private MonitoringFilter monitoringFilter;

	/**
	 * Initialisation (deux Before ne garantissent pas l'ordre dans Eclipse).
	 */
	public TestMonitoringFilterInit() {
		super();
		Utils.initialize();
	}

	/**
	 * Initialisation.
	 */
	@Before
	public void setUp() {
		tearDown();
		// rq: pas setUpFirst ici car setUp est rappelée dans les méthodes
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
		expect(context.getInitParameter(
				Parameters.PARAMETER_SYSTEM_PREFIX + Parameter.DISABLED.getCode())).andReturn(null)
						.anyTimes();
		expect(config.getInitParameter(Parameter.DISABLED.getCode())).andReturn(null).anyTimes();
		expect(context.getMajorVersion()).andReturn(2).anyTimes();
		expect(context.getMinorVersion()).andReturn(5).anyTimes();
		expect(context.getServletContextName()).andReturn("test webapp").anyTimes();
		expect(context.getServerInfo()).andReturn("mockJetty").anyTimes();
		expect(context.getContextPath()).andReturn("/test").anyTimes();
		monitoringFilter = new MonitoringFilter();
	}

	@After
	public void tearDown() {
		if (monitoringFilter != null) {
			monitoringFilter.destroy();
			monitoringFilter = null;
		}
	}

	/** Test.
	 * @throws ServletException e
	 * @throws IOException e */
	@Test
	public void testInit() throws ServletException, IOException {
		init();
		setUp();
		expect(context
				.getAttribute(Parameters.PARAMETER_SYSTEM_PREFIX + Parameter.DISABLED.getCode()))
						.andReturn("false").anyTimes();
		expect(config.getInitParameter(Parameter.DISPLAYED_COUNTERS.getCode()))
				.andReturn("http,sql").anyTimes();
		expect(config.getInitParameter(Parameter.HTTP_TRANSFORM_PATTERN.getCode()))
				.andReturn("[0-9]").anyTimes();
		init();
		setUp();
		expect(config.getInitParameter(Parameter.URL_EXCLUDE_PATTERN.getCode()))
				.andReturn("/static/*").anyTimes();
		init();
		setUp();
		expect(config.getInitParameter(Parameter.ALLOWED_ADDR_PATTERN.getCode()))
				.andReturn("127\\.0\\.0\\.1").anyTimes();
		init();

		// pour ce MonitoringFilter, instanceEnabled sera false
		final MonitoringFilter monitoringFilter2 = new MonitoringFilter();
		monitoringFilter2.init(config);
		final HttpServletRequest request = createNiceMock(HttpServletRequest.class);
		final HttpServletResponse response = createNiceMock(HttpServletResponse.class);
		final FilterChain filterChain = createNiceMock(FilterChain.class);
		monitoringFilter2.doFilter(request, response, filterChain);
		monitoringFilter2.destroy();
	}

	private void init() throws ServletException {
		replay(config);
		replay(context);
		monitoringFilter.init(config);
		verify(config);
		verify(context);
	}
}
