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

import java.util.Arrays;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.junit.Before;
import org.junit.Test;

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
}
