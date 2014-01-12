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
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import net.bull.javamelody.SamplingProfiler.SampledMethod;

import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.Font;
import com.lowagie.text.Phrase;

/**
 * Rapports pdf secondaires (avec iText).
 * @author Emeric Vernat
 */
class PdfOtherReport {
	private final OutputStream output;
	private final Document document;
	private final PdfDocumentFactory pdfDocumentFactory;

	PdfOtherReport(String application, OutputStream output) throws IOException {
		super();
		assert output != null;
		this.output = output;

		try {
			pdfDocumentFactory = new PdfDocumentFactory(application, null, output);
			this.document = pdfDocumentFactory.createDocument();
		} catch (final DocumentException e) {
			throw createIOException(e);
		}
	}

	void close() throws IOException {
		output.close();
	}

	void writeSessionInformations(List<SessionInformations> sessionsInformations)
			throws IOException {
		try {
			document.open();
			addParagraph(getString("Sessions"), "system-users.png");
			new PdfSessionInformationsReport(sessionsInformations, document).toPdf();
		} catch (final DocumentException e) {
			throw createIOException(e);
		}
		document.close();
	}

	void writeHeapHistogram(HeapHistogram heapHistogram) throws IOException {
		try {
			document.open();
			addParagraph(
					getFormattedString("heap_histo_du",
							I18N.createDateAndTimeFormat().format(heapHistogram.getTime())),
					"memory.png");
			new PdfHeapHistogramReport(heapHistogram, document).toPdf();
		} catch (final DocumentException e) {
			throw createIOException(e);
		}
		document.close();
	}

	void writeHotspots(List<SampledMethod> hotspots) throws IOException {
		try {
			document.open();
			addParagraph(getString("hotspots"), "clock.png");
			new PdfHotspotsReport(hotspots, document).toPdf();
		} catch (final DocumentException e) {
			throw createIOException(e);
		}
		document.close();
	}

	void writeProcessInformations(List<ProcessInformations> processInformations) throws IOException {
		try {
			document.open();
			addParagraph(getString("Processus"), "processes.png");
			new PdfProcessInformationsReport(processInformations, document).toPdf();
		} catch (final DocumentException e) {
			throw createIOException(e);
		}
		document.close();
	}

	void writeProcessInformations(Map<String, List<ProcessInformations>> processInformationsByTitle)
			throws IOException {
		try {
			document.open();
			for (final Map.Entry<String, List<ProcessInformations>> entry : processInformationsByTitle
					.entrySet()) {
				addParagraph(entry.getKey(), "processes.png");
				new PdfProcessInformationsReport(entry.getValue(), document).toPdf();
			}
		} catch (final DocumentException e) {
			throw createIOException(e);
		}
		document.close();
	}

	void writeDatabaseInformations(DatabaseInformations databaseInformations) throws IOException {
		try {
			document.open();
			final String selectedRequestName = databaseInformations.getSelectedRequestName();
			addParagraph(getString("database") + " : " + getString(selectedRequestName), "db.png");
			new PdfDatabaseInformationsReport(databaseInformations, document).toPdf();
		} catch (final DocumentException e) {
			throw createIOException(e);
		}
		document.close();
	}

	void writeJndi(List<JndiBinding> jndiBindings, String path) throws IOException {
		try {
			document.open();
			if (path.length() == 0) {
				addParagraph(getString("Arbre_JNDI"), "jndi.png");
			} else {
				addParagraph(getFormattedString("Arbre_JNDI_pour_contexte", path), "jndi.png");
			}
			new PdfJndiReport(jndiBindings, document).toPdf();
		} catch (final DocumentException e) {
			throw createIOException(e);
		}
		document.close();
	}

	void writeMBeans(List<MBeanNode> mbeans) throws IOException {
		try {
			document.open();
			addParagraph(getString("MBeans"), "mbeans.png");
			new PdfMBeansReport(mbeans, document).toPdf();
		} catch (final DocumentException e) {
			throw createIOException(e);
		}
		document.close();
	}

	void writeMBeans(Map<String, List<MBeanNode>> mbeansByTitle) throws IOException {
		try {
			document.open();
			for (final Map.Entry<String, List<MBeanNode>> entry : mbeansByTitle.entrySet()) {
				addParagraph(entry.getKey(), "mbeans.png");
				new PdfMBeansReport(entry.getValue(), document).toPdf();
			}
		} catch (final DocumentException e) {
			throw createIOException(e);
		}
		document.close();
	}

	void writeAllCurrentRequestsAsPart(
			Map<JavaInformations, List<CounterRequestContext>> currentRequests,
			Collector collector, List<Counter> counters, long timeOfSnapshot) throws IOException {
		try {
			document.open();

			// on remplace les parentCounters
			final List<CounterRequestContext> allCurrentRequests = new ArrayList<CounterRequestContext>();
			for (final List<CounterRequestContext> rootCurrentContexts : currentRequests.values()) {
				allCurrentRequests.addAll(rootCurrentContexts);
			}
			CounterRequestContext.replaceParentCounters(allCurrentRequests, counters);
			final List<PdfCounterReport> pdfCounterReports = new ArrayList<PdfCounterReport>();
			// ce range n'a pas d'importance pour ce pdf
			final Range range = Period.TOUT.getRange();
			for (final Counter counter : counters) {
				final PdfCounterReport pdfCounterReport = new PdfCounterReport(collector, counter,
						range, false, document);
				pdfCounterReports.add(pdfCounterReport);
			}
			final Font normalFont = PdfFonts.NORMAL.getFont();
			for (final Map.Entry<JavaInformations, List<CounterRequestContext>> entry : currentRequests
					.entrySet()) {
				final JavaInformations javaInformations = entry.getKey();
				final List<CounterRequestContext> rootCurrentContexts = entry.getValue();
				addParagraph(getString("Requetes_en_cours"), "hourglass.png");
				if (rootCurrentContexts.isEmpty()) {
					addToDocument(new Phrase(getString("Aucune_requete_en_cours"), normalFont));
				} else {
					final PdfCounterRequestContextReport pdfCounterRequestContextReport = new PdfCounterRequestContextReport(
							rootCurrentContexts, pdfCounterReports,
							javaInformations.getThreadInformationsList(),
							javaInformations.isStackTraceEnabled(), pdfDocumentFactory, document);
					pdfCounterRequestContextReport.setTimeOfSnapshot(timeOfSnapshot);
					pdfCounterRequestContextReport.writeContextDetails();
				}
			}
		} catch (final DocumentException e) {
			throw createIOException(e);
		}
		document.close();
	}

	void writeRuntimeDependencies(Counter counter, Range range) throws IOException {
		try {
			final Document myDocument = pdfDocumentFactory.createDocument(true);
			myDocument.open();
			final String counterLabel = getString(counter.getName() + "Label");
			final String paragraphTitle = getFormattedString("Dependance_compteur", counterLabel)
					+ " - " + range.getLabel();
			myDocument.add(pdfDocumentFactory.createParagraphElement(paragraphTitle,
					counter.getIconName()));
			new PdfRuntimeDependenciesReport(counter, myDocument).toPdf();
			myDocument.close();
		} catch (final DocumentException e) {
			throw createIOException(e);
		}
	}

	void writeCounterSummaryPerClass(Collector collector, Counter counter, String requestId,
			Range range) throws IOException {
		final List<CounterRequest> requestList = new CounterRequestAggregation(counter)
				.getRequestsAggregatedOrFilteredByClassName(requestId);
		try {
			document.open();
			final String counterLabel = getString(counter.getName() + "Label");
			final String title = getFormattedString("Statistiques_compteur", counterLabel) + " - "
					+ range.getLabel();
			addParagraph(title, counter.getIconName());
			new PdfCounterReport(collector, counter, range, false, document).writeRequests(
					counter.getChildCounterName(), requestList);
		} catch (final DocumentException e) {
			throw createIOException(e);
		}
		document.close();
	}

	private static IOException createIOException(Exception e) {
		// Rq: le constructeur de IOException avec message et cause n'existe qu'en jdk 1.6
		final IOException ex = new IOException(e.getMessage());
		ex.initCause(e);
		return ex;
	}

	private void addParagraph(String paragraphTitle, String iconName) throws DocumentException,
			IOException {
		addToDocument(pdfDocumentFactory.createParagraphElement(paragraphTitle, iconName));
	}

	private static String getString(String key) {
		return I18N.getString(key);
	}

	private static String getFormattedString(String key, Object... arguments) {
		return I18N.getFormattedString(key, arguments);
	}

	private void addToDocument(Element element) throws DocumentException {
		document.add(element);
	}
}
