/*
 * Copyright 2008-2010 by Emeric Vernat
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

import java.util.Arrays;

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
