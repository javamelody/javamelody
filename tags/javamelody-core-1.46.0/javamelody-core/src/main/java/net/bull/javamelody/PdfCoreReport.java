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
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import com.lowagie.text.Chunk;
import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.Font;
import com.lowagie.text.FontFactory;
import com.lowagie.text.Image;
import com.lowagie.text.Paragraph;
import com.lowagie.text.Phrase;
import com.lowagie.text.pdf.PdfPTable;

/**
 * Rapport pdf principal.
 * @author Emeric Vernat
 */
class PdfCoreReport extends PdfAbstractReport {
	static final int SMALL_GRAPH_WIDTH = 200;
	static final int SMALL_GRAPH_HEIGHT = 50;
	static final int LARGE_GRAPH_WIDTH = 960;
	static final int LARGE_GRAPH_HEIGHT = 370;
	private final Collector collector;
	private final List<JavaInformations> javaInformationsList;
	private final Range range;
	private Range counterRange;
	private List<CounterRequestContext> currentRequests;
	private final boolean collectorServer;
	private final PdfDocumentFactory pdfDocumentFactory;
	private final Font normalFont = PdfFonts.NORMAL.getFont();
	private final Font cellFont = PdfFonts.TABLE_CELL.getFont();
	private final Font boldFont = PdfFonts.BOLD.getFont();
	private final long start = System.currentTimeMillis();
	private Map<String, byte[]> smallGraphs;
	private Map<String, byte[]> smallOtherGraphs;
	private Map<String, byte[]> largeGraphs;

	PdfCoreReport(Collector collector, boolean collectorServer,
			List<JavaInformations> javaInformationsList, Range range,
			PdfDocumentFactory pdfDocumentFactory, Document document) {
		super(document);
		assert collector != null;
		assert javaInformationsList != null && !javaInformationsList.isEmpty();
		assert range != null;
		assert pdfDocumentFactory != null;

		this.collector = collector;
		this.collectorServer = collectorServer;
		this.javaInformationsList = javaInformationsList;
		this.range = range;
		this.counterRange = range; // par défaut, c'est le même range
		this.pdfDocumentFactory = pdfDocumentFactory;
	}

	// cette méthode est utilisée dans l'ihm Swing
	void preInitGraphs(Map<String, byte[]> newSmallGraphs, Map<String, byte[]> newSmallOtherGraphs,
			Map<String, byte[]> newLargeGraphs) {
		this.smallGraphs = newSmallGraphs;
		this.smallOtherGraphs = newSmallOtherGraphs;
		this.largeGraphs = newLargeGraphs;
	}

	// cette méthode est utilisée dans l'ihm Swing
	void setCounterRange(Range counterRange) {
		this.counterRange = counterRange;
	}

	// cette méthode est utilisée dans l'ihm Swing
	void setCurrentRequests(List<CounterRequestContext> currentRequests) {
		this.currentRequests = currentRequests;
	}

	@Override
	void toPdf() throws IOException, DocumentException {
		addParagraph(buildSummary(), "systemmonitor.png");
		writeGraphs(collector.getCounterJRobins(), smallGraphs);

		final List<Counter> counters = collector.getRangeCountersToBeDisplayed(counterRange);
		final List<PdfCounterReport> pdfCounterReports = writeCounters(counters);

		final List<PdfCounterRequestContextReport> pdfCounterRequestContextReports = new ArrayList<PdfCounterRequestContextReport>();
		if (!collectorServer) {
			addParagraph(getString("Requetes_en_cours"), "hourglass.png");
			// si on n'est pas sur le serveur de collecte il n'y a qu'un javaInformations
			pdfCounterRequestContextReports.addAll(writeCurrentRequests(
					javaInformationsList.get(0), counters, pdfCounterReports));
		}

		addToDocument(new Phrase("\n", normalFont));
		addParagraph(getString("Informations_systemes"), "systeminfo.png");
		new PdfJavaInformationsReport(javaInformationsList, getDocument()).toPdf();

		addParagraph(getString("Threads"), "threads.png");
		writeThreads(false);

		PdfCounterReport pdfJobCounterReport = null;
		Counter rangeJobCounter = null;
		if (isJobEnabled()) {
			rangeJobCounter = collector.getRangeCounter(counterRange, Counter.JOB_COUNTER_NAME);
			addToDocument(new Phrase("\n", normalFont));
			addParagraph(getString("Jobs"), "jobs.png");
			writeJobs(rangeJobCounter, false);
			pdfJobCounterReport = writeCounter(rangeJobCounter);
		}

		if (isCacheEnabled()) {
			addToDocument(new Phrase("\n", normalFont));
			addParagraph(getString("Caches"), "caches.png");
			writeCaches(false);
		}

		newPage();
		addParagraph(getString("Statistiques_detaillees"), "systemmonitor.png");
		writeGraphs(collector.getOtherJRobins(), smallOtherGraphs);
		writeGraphDetails();

		writeCountersDetails(pdfCounterReports);

		if (!collectorServer) {
			addParagraph(getString("Requetes_en_cours_detaillees"), "hourglass.png");
			// si on n'est pas sur le serveur de collecte il n'y a qu'un javaInformations
			writeCurrentRequestsDetails(pdfCounterRequestContextReports);
		}

		addParagraph(getString("Informations_systemes_detaillees"), "systeminfo.png");
		new PdfJavaInformationsReport(javaInformationsList, getDocument())
				.writeInformationsDetails();

		addParagraph(getString("Threads_detailles"), "threads.png");
		writeThreads(true);

		if (isJobEnabled()) {
			addToDocument(new Phrase("\n", normalFont));
			addParagraph(getString("Jobs_detailles"), "jobs.png");
			writeJobs(rangeJobCounter, true);
			writeCounterDetails(pdfJobCounterReport);
		}

		if (isCacheEnabled()) {
			addToDocument(new Phrase("\n", normalFont));
			addParagraph(getString("Caches_detailles"), "caches.png");
			writeCaches(true);
		}

		writeDurationAndOverhead();
	}

	private String buildSummary() {
		final String tmp;
		if (range.getPeriod() == Period.TOUT) {
			final String startDate = I18N.createDateAndTimeFormat().format(
					collector.getCounters().get(0).getStartDate());
			tmp = getFormattedString("Statistiques", "JavaMelody", I18N.getCurrentDateAndTime(),
					startDate, collector.getApplication());
		} else {
			tmp = getFormattedString("Statistiques_sans_depuis", "JavaMelody",
					I18N.getCurrentDateAndTime(), collector.getApplication());
		}
		if (javaInformationsList.get(0).getContextDisplayName() != null) {
			return tmp + " (" + javaInformationsList.get(0).getContextDisplayName() + ')';
		}
		return tmp;
	}

	private void writeGraphs(Collection<JRobin> jrobins, Map<String, byte[]> mySmallGraphs)
			throws IOException, DocumentException {
		final Paragraph jrobinParagraph = new Paragraph("", FontFactory.getFont(
				FontFactory.HELVETICA, 9f, Font.NORMAL));
		jrobinParagraph.setAlignment(Element.ALIGN_CENTER);
		jrobinParagraph.add(new Phrase("\n\n\n\n"));
		int i = 0;
		if (mySmallGraphs != null) {
			// si les graphiques ont été préinitialisés (en Swing) alors on les utilise
			for (final byte[] imageData : mySmallGraphs.values()) {
				if (i % 3 == 0 && i != 0) {
					// un retour après httpSessions et avant activeThreads pour l'alignement
					jrobinParagraph.add(new Phrase("\n\n\n\n\n"));
				}
				final Image image = Image.getInstance(imageData);
				image.scalePercent(50);
				jrobinParagraph.add(new Phrase(new Chunk(image, 0, 0)));
				jrobinParagraph.add(new Phrase(" "));
				i++;
			}
		} else {
			if (jrobins.isEmpty()) {
				return;
			}
			for (final JRobin jrobin : jrobins) {
				if (collector.isJRobinDisplayed(jrobin)) {
					if (i % 3 == 0 && i != 0) {
						// un retour après httpSessions et avant activeThreads pour l'alignement
						jrobinParagraph.add(new Phrase("\n\n\n\n\n"));
					}
					final Image image = Image.getInstance(jrobin.graph(range, SMALL_GRAPH_WIDTH,
							SMALL_GRAPH_HEIGHT));
					image.scalePercent(50);
					jrobinParagraph.add(new Phrase(new Chunk(image, 0, 0)));
					jrobinParagraph.add(new Phrase(" "));
					i++;
				}
			}
		}
		jrobinParagraph.add(new Phrase("\n"));
		addToDocument(jrobinParagraph);
	}

	private void writeGraphDetails() throws IOException, DocumentException {
		final PdfPTable jrobinTable = new PdfPTable(1);
		jrobinTable.setHorizontalAlignment(Element.ALIGN_CENTER);
		jrobinTable.setWidthPercentage(100);
		jrobinTable.getDefaultCell().setBorder(0);
		if (largeGraphs != null) {
			// si les graphiques ont été préinitialisés (en Swing) alors on les utilise
			for (final byte[] imageData : largeGraphs.values()) {
				final Image image = Image.getInstance(imageData);
				jrobinTable.addCell(image);
			}
		} else {
			final Collection<JRobin> counterJRobins = collector.getCounterJRobins();
			if (counterJRobins.isEmpty()) {
				return;
			}
			for (final JRobin jrobin : counterJRobins) {
				if (collector.isJRobinDisplayed(jrobin)) {
					// la hauteur de l'image est prévue pour qu'il n'y ait pas de graph seul sur une page
					final Image image = Image.getInstance(jrobin.graph(range, LARGE_GRAPH_WIDTH,
							LARGE_GRAPH_HEIGHT));
					jrobinTable.addCell(image);
				}
			}
		}
		newPage();
		addToDocument(jrobinTable);
		newPage();
	}

	private List<PdfCounterReport> writeCounters(List<Counter> counters) throws IOException,
			DocumentException {
		final List<PdfCounterReport> pdfCounterReports = new ArrayList<PdfCounterReport>();
		for (final Counter counter : counters) {
			pdfCounterReports.add(writeCounter(counter));
		}
		return pdfCounterReports;
	}

	private PdfCounterReport writeCounter(Counter counter) throws DocumentException, IOException {
		final String counterLabel = getString(counter.getName() + "Label");
		addParagraph(
				getFormattedString("Statistiques_compteur", counterLabel) + " - "
						+ range.getLabel(), counter.getIconName());
		final PdfCounterReport pdfCounterReport = new PdfCounterReport(collector, counter, range,
				false, getDocument());
		pdfCounterReport.toPdf();
		return pdfCounterReport;
	}

	private void writeCountersDetails(List<PdfCounterReport> pdfCounterReports)
			throws DocumentException, IOException {
		for (final PdfCounterReport pdfCounterReport : pdfCounterReports) {
			writeCounterDetails(pdfCounterReport);
		}
	}

	private void writeCounterDetails(PdfCounterReport pdfCounterReport) throws DocumentException,
			IOException {
		final String counterLabel = getString(pdfCounterReport.getCounterName() + "Label");
		addParagraph(getFormattedString("Statistiques_compteur_detaillees", counterLabel) + " - "
				+ range.getLabel(), pdfCounterReport.getCounterIconName());
		pdfCounterReport.writeRequestDetails();
		if (pdfCounterReport.isErrorCounter()) {
			addParagraph(getString(pdfCounterReport.getCounterName() + "ErrorLabel") + " - "
					+ range.getLabel(), pdfCounterReport.getCounterIconName());
			pdfCounterReport.writeErrorDetails();
		}
	}

	private List<PdfCounterRequestContextReport> writeCurrentRequests(
			JavaInformations javaInformations, List<Counter> counters,
			List<PdfCounterReport> pdfCounterReports) throws IOException, DocumentException {
		final List<PdfCounterRequestContextReport> pdfCounterRequestContextReports = new ArrayList<PdfCounterRequestContextReport>();
		final List<CounterRequestContext> rootCurrentContexts;
		if (currentRequests == null) {
			rootCurrentContexts = collector.getRootCurrentContexts(counters);
		} else {
			rootCurrentContexts = currentRequests;
		}
		if (rootCurrentContexts.isEmpty()) {
			addToDocument(new Phrase(getString("Aucune_requete_en_cours"), normalFont));
		} else {
			final PdfCounterRequestContextReport pdfCounterRequestContextReport = new PdfCounterRequestContextReport(
					rootCurrentContexts, pdfCounterReports,
					javaInformations.getThreadInformationsList(),
					javaInformations.isStackTraceEnabled(), pdfDocumentFactory, getDocument());
			pdfCounterRequestContextReport.toPdf();
			pdfCounterRequestContextReports.add(pdfCounterRequestContextReport);
		}
		return pdfCounterRequestContextReports;
	}

	private void writeCurrentRequestsDetails(
			List<PdfCounterRequestContextReport> pdfCounterRequestContextReports)
			throws IOException, DocumentException {
		for (final PdfCounterRequestContextReport pdfCounterRequestContextReport : pdfCounterRequestContextReports) {
			pdfCounterRequestContextReport.writeContextDetails();
		}
		if (pdfCounterRequestContextReports.isEmpty()) {
			addToDocument(new Phrase(getString("Aucune_requete_en_cours"), normalFont));
		}
	}

	private void writeThreads(boolean includeDetails) throws DocumentException, IOException {
		String eol = "";
		for (final JavaInformations javaInformations : javaInformationsList) {
			addToDocument(new Phrase(eol
					+ getFormattedString("Threads_sur", javaInformations.getHost()) + ": ",
					boldFont));
			addToDocument(new Phrase(getFormattedString("thread_count",
					javaInformations.getThreadCount(), javaInformations.getPeakThreadCount(),
					javaInformations.getTotalStartedThreadCount()), normalFont));

			final PdfThreadInformationsReport pdfThreadInformationsReport = new PdfThreadInformationsReport(
					javaInformations.getThreadInformationsList(),
					javaInformations.isStackTraceEnabled(), pdfDocumentFactory, getDocument());
			pdfThreadInformationsReport.writeDeadlocks();

			if (includeDetails) {
				pdfThreadInformationsReport.toPdf();
			}
			eol = "\n";
		}
	}

	private void writeCaches(boolean includeDetails) throws DocumentException {
		String eol = "";
		for (final JavaInformations javaInformations : javaInformationsList) {
			if (!javaInformations.isCacheEnabled()) {
				continue;
			}
			final List<CacheInformations> cacheInformationsList = javaInformations
					.getCacheInformationsList();
			final String msg = getFormattedString("caches_sur", cacheInformationsList.size(),
					javaInformations.getHost(), javaInformations.getCurrentlyExecutingJobCount());
			addToDocument(new Phrase(eol + msg, boldFont));

			if (includeDetails) {
				new PdfCacheInformationsReport(cacheInformationsList, getDocument()).toPdf();
			}
			eol = "\n";
		}
	}

	private boolean isCacheEnabled() {
		for (final JavaInformations javaInformations : javaInformationsList) {
			if (javaInformations.isCacheEnabled()) {
				return true;
			}
		}
		return false;
	}

	private void writeJobs(Counter rangeJobCounter, boolean includeDetails)
			throws DocumentException, IOException {
		String eol = "";
		for (final JavaInformations javaInformations : javaInformationsList) {
			if (!javaInformations.isJobEnabled()) {
				continue;
			}
			final List<JobInformations> jobInformationsList = javaInformations
					.getJobInformationsList();
			final String msg = getFormattedString("jobs_sur", jobInformationsList.size(),
					javaInformations.getHost(), javaInformations.getCurrentlyExecutingJobCount());
			addToDocument(new Phrase(eol + msg, boldFont));

			if (includeDetails) {
				new PdfJobInformationsReport(jobInformationsList, rangeJobCounter, getDocument())
						.toPdf();
			}
			eol = "\n";
		}
	}

	private boolean isJobEnabled() {
		for (final JavaInformations javaInformations : javaInformationsList) {
			if (javaInformations.isJobEnabled()) {
				return true;
			}
		}
		return false;
	}

	private void writeDurationAndOverhead() throws DocumentException {
		final long displayDuration = System.currentTimeMillis() - start;
		final String tmp = "\n\n" + getString("temps_derniere_collecte") + ": "
				+ collector.getLastCollectDuration() + ' ' + getString("ms") + '\n'
				+ getString("temps_affichage") + ": " + displayDuration + ' ' + getString("ms")
				+ '\n' + getString("Estimation_overhead_memoire") + ": < "
				+ (collector.getEstimatedMemorySize() / 1024 / 1024 + 1) + ' ' + getString("Mo")
				+ '\n' + getString("Usage_disque") + ": "
				+ (collector.getDiskUsage() / 1024 / 1024 + 1) + ' ' + getString("Mo");
		final String string;
		if (Parameters.JAVAMELODY_VERSION != null) {
			string = tmp + "\n\n" + "JavaMelody " + Parameters.JAVAMELODY_VERSION;
		} else {
			string = tmp;
		}
		addToDocument(new Phrase(string, cellFont));
	}

	private void addParagraph(String paragraphTitle, String iconName) throws DocumentException,
			IOException {
		addToDocument(pdfDocumentFactory.createParagraphElement(paragraphTitle, iconName));
	}
}
