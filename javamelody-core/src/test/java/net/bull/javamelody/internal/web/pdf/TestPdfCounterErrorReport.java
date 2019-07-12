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
package net.bull.javamelody.internal.web.pdf;

import static org.easymock.EasyMock.createNiceMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Collections;

import javax.servlet.http.HttpServletRequest;

import org.junit.Before;
import org.junit.Test;

import net.bull.javamelody.Utils;
import net.bull.javamelody.internal.model.Collector;
import net.bull.javamelody.internal.model.Counter;
import net.bull.javamelody.internal.model.CounterError;
import net.bull.javamelody.internal.model.JavaInformations;
import net.bull.javamelody.internal.model.Period;

/**
 * Test unitaire de la classe PdfCounterErrorReport.
 * @author Emeric Vernat
 */
public class TestPdfCounterErrorReport {
	/** Check. */
	@Before
	public void setUp() {
		Utils.initialize();
	}

	/** Test.
		 * @throws IOException e */
	@Test
	public void testCounterError() throws IOException {
		final Counter errorCounter = new Counter(Counter.ERROR_COUNTER_NAME, null);
		final Collector collector = new Collector("test", Collections.singletonList(errorCounter));
		final JavaInformations javaInformations = new JavaInformations(null, true);
		final ByteArrayOutputStream output = new ByteArrayOutputStream();
		final PdfReport pdfReport = new PdfReport(collector, false,
				Collections.singletonList(javaInformations), Period.TOUT, output);
		while (errorCounter.getErrorsCount() < Counter.MAX_ERRORS_COUNT) {
			errorCounter.addErrors(Collections.singletonList(new CounterError("erreur", null)));
		}
		pdfReport.toPdf();
		assertNotEmptyAndClear(output);

		final HttpServletRequest httpRequest = createNiceMock(HttpServletRequest.class);
		expect(httpRequest.getAttribute(CounterError.REQUEST_KEY)).andReturn("/test GET");
		expect(httpRequest.getRemoteUser()).andReturn("me");
		replay(httpRequest);
		CounterError.bindRequest(httpRequest);
		errorCounter.addErrors(Collections.singletonList(new CounterError("with request", null)));
		CounterError.unbindRequest();
		verify(httpRequest);
		final PdfReport pdfReport2 = new PdfReport(collector, false,
				Collections.singletonList(javaInformations), Period.TOUT, output);
		pdfReport2.toPdf();
		assertNotEmptyAndClear(output);
	}

	private void assertNotEmptyAndClear(ByteArrayOutputStream output) {
		assertTrue("rapport vide", output.size() > 0);
		output.reset();
	}
}
