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

import java.util.Arrays;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.junit.Before;
import org.junit.Test;

import net.bull.javamelody.internal.common.HttpParameter;
import net.bull.javamelody.internal.common.Parameters;
import net.bull.javamelody.internal.model.Collector;
import net.bull.javamelody.internal.model.Counter;
import net.bull.javamelody.internal.web.MonitoringController;

/**
 * Test unitaire de la classe MonitoringController.
 * @author Emeric Vernat
 */
public class TestMonitoringController {
	/**
	 * Initialisation.
	 */
	@Before
	public void setUp() {
		Utils.initialize();
		final ServletContext context = createNiceMock(ServletContext.class);
		expect(context.getMajorVersion()).andReturn(2).anyTimes();
		expect(context.getMinorVersion()).andReturn(5).anyTimes();
		expect(context.getServletContextName()).andReturn("test webapp").anyTimes();
		expect(context.getServerInfo()).andReturn("mockJetty").anyTimes();
		expect(context.getContextPath()).andReturn("/test").anyTimes();
		replay(context);
		Parameters.initialize(context);
	}

	/** Test. */
	@Test
	public void testWriteHtmlToLastShutdownFile() {
		final Counter sqlCounter = new Counter("sql", "db.png");
		final Collector collector = new Collector("test", Arrays.asList(sqlCounter));
		new MonitoringController(collector, null).writeHtmlToLastShutdownFile();
	}

	/** Test. */
	@Test
	public void testAddPdfContentTypeAndDisposition() {
		final Counter sqlCounter = new Counter("sql", "db.png");
		final Collector collector = new Collector("test collector", Arrays.asList(sqlCounter));
		final HttpServletRequest httpRequest = createNiceMock(HttpServletRequest.class);
		final HttpServletResponse httpResponse = createNiceMock(HttpServletResponse.class);
		expect(httpRequest.getHeader("user-agent")).andReturn("Firefox").anyTimes();
		replay(httpRequest);
		replay(httpResponse);
		new MonitoringController(collector, null).addPdfContentTypeAndDisposition(httpRequest,
				httpResponse);
		verify(httpRequest);
		verify(httpResponse);

		final HttpServletRequest httpRequest2 = createNiceMock(HttpServletRequest.class);
		final HttpServletResponse httpResponse2 = createNiceMock(HttpServletResponse.class);
		expect(httpRequest2.getHeader("user-agent")).andReturn("MSIE").anyTimes();
		replay(httpRequest2);
		replay(httpResponse2);
		new MonitoringController(collector, null).addPdfContentTypeAndDisposition(httpRequest2,
				httpResponse2);
		verify(httpRequest2);
		verify(httpResponse2);
	}

	/** Test. */
	@Test
	public void testCheckCsrfToken() {
		final HttpServletRequest httpRequest0 = createNiceMock(HttpServletRequest.class);
		replay(httpRequest0);
		try {
			MonitoringController.checkCsrfToken(httpRequest0);
		} catch (final Exception e) {
			assertNotNull("e", e);
		}
		verify(httpRequest0);

		final HttpServletRequest httpRequest1 = createNiceMock(HttpServletRequest.class);
		final HttpSession httpSession1 = createNiceMock(HttpSession.class);
		final String token = "dummy token";
		expect(HttpParameter.TOKEN.getParameterFrom(httpRequest1)).andReturn(token);
		expect(httpRequest1.getSession(false)).andReturn(httpSession1);
		expect(httpSession1.getAttribute(SessionListener.CSRF_TOKEN_SESSION_NAME)).andReturn(token);
		replay(httpRequest1);
		replay(httpSession1);
		MonitoringController.checkCsrfToken(httpRequest1);
		verify(httpRequest1);
		verify(httpSession1);

		final HttpServletRequest httpRequest2 = createNiceMock(HttpServletRequest.class);
		final HttpSession httpSession2 = createNiceMock(HttpSession.class);
		expect(HttpParameter.TOKEN.getParameterFrom(httpRequest2)).andReturn(token);
		expect(httpRequest2.getSession(false)).andReturn(httpSession2);
		expect(httpSession2.getAttribute(SessionListener.CSRF_TOKEN_SESSION_NAME))
				.andReturn("unknown");
		replay(httpRequest2);
		replay(httpSession2);
		try {
			MonitoringController.checkCsrfToken(httpRequest2);
		} catch (final Exception e) {
			assertNotNull("e", e);
		}
		verify(httpRequest2);
		verify(httpSession2);
	}
}
