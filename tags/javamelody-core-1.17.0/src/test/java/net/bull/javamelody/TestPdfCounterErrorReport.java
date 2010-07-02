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
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Collections;
import java.util.Timer;

import javax.servlet.http.HttpServletRequest;

import org.junit.Before;
import org.junit.Test;

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
		final Timer timer = new Timer("test", true);
		try {
			final Collector collector = new Collector("test",
					Collections.singletonList(errorCounter), timer);
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
			errorCounter.addErrors(Collections
					.singletonList(new CounterError("with request", null)));
			CounterError.unbindRequest();
			verify(httpRequest);
			final PdfReport pdfReport2 = new PdfReport(collector, false,
					Collections.singletonList(javaInformations), Period.TOUT, output);
			pdfReport2.toPdf();
			assertNotEmptyAndClear(output);
		} finally {
			timer.cancel();
		}
	}

	private void assertNotEmptyAndClear(ByteArrayOutputStream output) {
		assertTrue("rapport vide", output.size() > 0);
		output.reset();
	}
}
