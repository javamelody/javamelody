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

import java.io.IOException;
import java.io.OutputStream;
import java.util.List;
import java.util.Map;

import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;

import net.bull.javamelody.internal.common.I18N;
import net.bull.javamelody.internal.model.Collector;
import net.bull.javamelody.internal.model.CounterRequestContext;
import net.bull.javamelody.internal.model.JavaInformations;
import net.bull.javamelody.internal.model.Period;
import net.bull.javamelody.internal.model.Range;

/**
 * Rapport pdf (avec iText v2.1.7).
 * @author Emeric Vernat
 */
public class PdfReport {
	private final Document document;
	private final OutputStream output;
	private final PdfCoreReport pdfCoreReport;

	public PdfReport(Collector collector, boolean collectorServer,
			List<JavaInformations> javaInformationsList, Range range, OutputStream output)
			throws IOException {
		super();
		assert output != null;
		this.output = output;

		try {
			final PdfDocumentFactory pdfDocumentFactory = new PdfDocumentFactory(
					collector.getApplication(), range, output);
			this.document = pdfDocumentFactory.createDocument();
			this.pdfCoreReport = new PdfCoreReport(collector, collectorServer, javaInformationsList,
					range, pdfDocumentFactory, document);
		} catch (final DocumentException e) {
			throw createIOException(e);
		}
	}

	public PdfReport(Collector collector, boolean collectorServer,
			List<JavaInformations> javaInformationsList, Period period, OutputStream output)
			throws IOException {
		this(collector, collectorServer, javaInformationsList, period.getRange(), output);
	}

	private static IOException createIOException(DocumentException e) {
		// Rq: le constructeur de IOException avec message et cause n'existe qu'en jdk 1.6
		return new IOException(e.getMessage(), e);
	}

	public static String getFileName(String application) {
		return "JavaMelody_" + application.replace(' ', '_').replace("/", "") + '_'
				+ I18N.getCurrentDate().replace('/', '_') + ".pdf";
	}

	public static boolean shouldUseEnglishInsteadOfUkrainian() {
		return PdfFonts.shouldUseEnglishInsteadOfUkrainian();
	}

	public void toPdf() throws IOException {
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
	public void preInitGraphs(Map<String, byte[]> newSmallGraphs,
			Map<String, byte[]> newSmallOtherGraphs, Map<String, byte[]> newLargeGraphs) {
		pdfCoreReport.preInitGraphs(newSmallGraphs, newSmallOtherGraphs, newLargeGraphs);
	}

	// cette méthode est utilisée dans l'ihm Swing
	public void setCounterRange(Range counterRange) {
		pdfCoreReport.setCounterRange(counterRange);
	}

	// cette méthode est utilisée dans l'ihm Swing
	public void setCurrentRequests(List<CounterRequestContext> currentRequests) {
		pdfCoreReport.setCurrentRequests(currentRequests);
	}

	// cette méthode est utilisée dans l'ihm Swing
	public void close() throws IOException {
		output.close();
	}
}
