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

import static org.easymock.EasyMock.createNiceMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.io.StringWriter;
import java.util.Collections;

import javax.servlet.http.HttpServletRequest;

import org.junit.Before;
import org.junit.Test;

/**
 * Test unitaire de la classe HtmlCounterErrorReport.
 * @author Emeric Vernat
 */
public class TestHtmlCounterErrorReport {
	/** Check. */
	@Before
	public void setUp() {
		Utils.initialize();
	}

	private static void assertNotEmptyAndClear(StringWriter writer) {
		assertTrue("rapport vide", writer.getBuffer().length() > 0);
		writer.getBuffer().setLength(0);
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testCounterError() throws IOException {
		final Counter errorCounter = new Counter(Counter.ERROR_COUNTER_NAME, null);
		final StringWriter writer = new StringWriter();
		final HtmlCounterErrorReport report = new HtmlCounterErrorReport(errorCounter, writer);
		report.toHtml();
		assertNotEmptyAndClear(writer);

		while (errorCounter.getErrorsCount() < Counter.MAX_ERRORS_COUNT) {
			errorCounter.addErrors(Collections.singletonList(new CounterError("erreur", null)));
		}
		report.toHtml();
		assertNotEmptyAndClear(writer);

		final HttpServletRequest httpRequest = createNiceMock(HttpServletRequest.class);
		expect(httpRequest.getAttribute(CounterError.REQUEST_KEY)).andReturn("/test GET");
		expect(httpRequest.getRemoteUser()).andReturn("me");
		replay(httpRequest);
		CounterError.bindRequest(httpRequest);
		errorCounter.addErrors(Collections.singletonList(new CounterError("with request", null)));
		CounterError.unbindRequest();
		verify(httpRequest);
		report.toHtml();
		assertNotEmptyAndClear(writer);
	}
}
