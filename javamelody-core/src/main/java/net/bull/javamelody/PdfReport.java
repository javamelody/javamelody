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

import java.io.IOException;
import java.io.OutputStream;
import java.util.List;
import java.util.Map;

import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;

/**
 * Rapport pdf (avec iText).
 * @author Emeric Vernat
 */
class PdfReport {
	private final Document document;
	private final OutputStream output;
	private final PdfCoreReport pdfCoreReport;

	PdfReport(Collector collector, boolean collectorServer,
			List<JavaInformations> javaInformationsList, Range range, OutputStream output)
			throws IOException {
		super();
		assert output != null;
		this.output = output;

		try {
			final PdfDocumentFactory pdfDocumentFactory = new PdfDocumentFactory(
					collector.getApplication(), range, output);
			this.document = pdfDocumentFactory.createDocument();
			this.pdfCoreReport = new PdfCoreReport(collector, collectorServer,
					javaInformationsList, range, pdfDocumentFactory, document);
		} catch (final DocumentException e) {
			throw createIOException(e);
		}
	}

	PdfReport(Collector collector, boolean collectorServer,
			List<JavaInformations> javaInformationsList, Period period, OutputStream output)
			throws IOException {
		this(collector, collectorServer, javaInformationsList, period.getRange(), output);
	}

	private static IOException createIOException(DocumentException e) {
		// Rq: le constructeur de IOException avec message et cause n'existe qu'en jdk 1.6
		final IOException ex = new IOException(e.getMessage());
		ex.initCause(e);
		return ex;
	}

	static String getFileName(String application) {
		return "JavaMelody_" + application.replace(' ', '_').replace("/", "") + '_'
				+ I18N.getCurrentDate().replace('/', '_') + ".pdf";
	}

	void toPdf() throws IOException {
		try {
			document.open();

			// il serait possible d'ouvrir la boîte de dialogue Imprimer de Adobe Reader
			//		      if (writer instanceof PdfWriter) {
			//		        ((PdfWriter) writer).addJavaScript("this.print(true);", false);
			//		      }

			pdfCoreReport.toPdf();
		} catch (final DocumentException e) {
			throw createIOException(e);
		}

		document.close();
	}

	// cette méthode est utilisée dans l'ihm Swing
	void preInitGraphs(Map<String, byte[]> newSmallGraphs, Map<String, byte[]> newSmallOtherGraphs,
			Map<String, byte[]> newLargeGraphs) {
		pdfCoreReport.preInitGraphs(newSmallGraphs, newSmallOtherGraphs, newLargeGraphs);
	}

	// cette méthode est utilisée dans l'ihm Swing
	void setCounterRange(Range counterRange) {
		pdfCoreReport.setCounterRange(counterRange);
	}

	// cette méthode est utilisée dans l'ihm Swing
	void setCurrentRequests(List<CounterRequestContext> currentRequests) {
		pdfCoreReport.setCurrentRequests(currentRequests);
	}

	// cette méthode est utilisée dans l'ihm Swing
	void close() throws IOException {
		output.close();
	}
}
