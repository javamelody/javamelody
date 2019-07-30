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
package net.bull.javamelody.internal.web.html;

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

import net.bull.javamelody.Utils;
import net.bull.javamelody.internal.model.Counter;
import net.bull.javamelody.internal.model.CounterError;

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
