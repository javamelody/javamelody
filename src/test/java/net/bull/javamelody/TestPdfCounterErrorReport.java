/*
 * Copyright 2008-2009 by Emeric Vernat, Bull
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

import static org.junit.Assert.assertTrue;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Collections;
import java.util.Timer;

import org.junit.Test;

/**
 * Test unitaire de la classe PdfCounterErrorReport.
 * @author Emeric Vernat
 */
public class TestPdfCounterErrorReport {
	/** Test.
	 * @throws IOException e */
	@Test
	public void testCounterError() throws IOException {
		final Counter errorCounter = new Counter(Counter.ERROR_COUNTER_NAME, null);
		final Timer timer = new Timer("test", true);
		try {
			final Collector collector = new Collector("test", Collections
					.singletonList(errorCounter), timer);
			final JavaInformations javaInformations = new JavaInformations(null, true);
			final ByteArrayOutputStream output = new ByteArrayOutputStream();
			final PdfReport pdfReport = new PdfReport(collector, false, Collections
					.singletonList(javaInformations), Period.TOUT, output);
			while (errorCounter.getErrorsCount() < Counter.MAX_ERRORS_COUNT) {
				errorCounter.addErrors(Collections.singletonList(new CounterError("erreur", null)));
			}
			pdfReport.toPdf();
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
