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
