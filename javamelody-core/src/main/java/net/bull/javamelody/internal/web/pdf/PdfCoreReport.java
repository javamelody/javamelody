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

import net.bull.javamelody.internal.common.I18N;
import net.bull.javamelody.internal.common.Parameters;
import net.bull.javamelody.internal.model.CacheInformations;
import net.bull.javamelody.internal.model.Collector;
import net.bull.javamelody.internal.model.Counter;
import net.bull.javamelody.internal.model.CounterRequestContext;
import net.bull.javamelody.internal.model.JCacheInformations;
import net.bull.javamelody.internal.model.JRobin;
import net.bull.javamelody.internal.model.JavaInformations;
import net.bull.javamelody.internal.model.JobInformations;
import net.bull.javamelody.internal.model.Period;
import net.bull.javamelody.internal.model.Range;

/**
 * Rapport pdf principal.
 * @author Emeric Vernat
 */
public class PdfCoreReport extends PdfAbstractReport {
	public static final int SMALL_GRAPH_WIDTH = 200;
	public static final int SMALL_GRAPH_HEIGHT = 50;
	public static final int LARGE_GRAPH_WIDTH = 960;
	public static final int LARGE_GRAPH_HEIGHT = 370;
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
		writeGraphs(collector.getDisplayedCounterJRobins(), smallGraphs);

		final List<Counter> counters = collector.getRangeCountersToBeDisplayed(counterRange);
		final List<PdfCounterReport> pdfCounterReports = writeCounters(counters);

		final List<PdfCounterRequestContextReport> pdfCounterRequestContextReports = new ArrayList<PdfCounterRequestContextReport>();
		if (!collectorServer) {
			addParagraph(getString("Requetes_en_cours"), "hourglass.png");
			// si on n'est pas sur le serveur de collecte il n'y a qu'un javaInformations
			pdfCounterRequestContextReports.addAll(
					writeCurrentRequests(javaInformationsList.get(0), counters, pdfCounterReports));
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

		if (isCacheEnabled() || isJCacheEnabled()) {
			addToDocument(new Phrase("\n", normalFont));
			addParagraph(getString("Caches"), "caches.png");
			writeCaches(false);
			writeJCaches(false);
		}

		newPage();
		addParagraph(getString("Statistiques_detaillees"), "systemmonitor.png");
		writeGraphs(collector.getDisplayedOtherJRobins(), smallOtherGraphs);
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

		if (isCacheEnabled() || isJCacheEnabled()) {
			addToDocument(new Phrase("\n", normalFont));
			addParagraph(getString("Caches_detailles"), "caches.png");
			writeCaches(true);
			writeJCaches(true);
		}

		writeDurationAndOverhead();
	}

	private String buildSummary() {
		final String tmp;
		if (range.getPeriod() == Period.TOUT) {
			final String startDate = I18N.createDateAndTimeFormat()
					.format(collector.getCounters().get(0).getStartDate());
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
		if (collector.isStopped()) {
			// pas de graphs, ils seraient en erreur sans timer
			// mais un message d'avertissement à la place
			final String message = getString("collect_server_misusage");
			final Paragraph jrobinParagraph = new Paragraph(message, PdfFonts.BOLD.getFont());
			jrobinParagraph.setAlignment(Element.ALIGN_CENTER);
			addToDocument(jrobinParagraph);
			return;
		}
		if (collector.isStorageUsedByMultipleInstances()) {
			final String message = getString("storage_used_by_multiple_instances") + "\n\n";
			final Paragraph jrobinParagraph = new Paragraph(message, PdfFonts.BOLD.getFont());
			jrobinParagraph.setAlignment(Element.ALIGN_CENTER);
			addToDocument(jrobinParagraph);
		}
		final Paragraph jrobinParagraph = new Paragraph("",
				FontFactory.getFont(FontFactory.HELVETICA, 9f, Font.NORMAL));
		jrobinParagraph.setAlignment(Element.ALIGN_CENTER);
		jrobinParagraph.add(new Phrase("\n\n\n\n"));
		final Collection<byte[]> graphs;
		if (mySmallGraphs != null) {
			// si les graphiques ont été préinitialisés (en Swing) alors on les utilise
			graphs = mySmallGraphs.values();
		} else {
			if (jrobins.isEmpty()) {
				return;
			}
			graphs = new ArrayList<byte[]>(jrobins.size());
			for (final JRobin jrobin : jrobins) {
				graphs.add(jrobin.graph(range, SMALL_GRAPH_WIDTH, SMALL_GRAPH_HEIGHT));
			}
		}
		int i = 0;
		for (final byte[] graph : graphs) {
			if (i % 3 == 0 && i != 0) {
				// un retour après httpSessions et avant activeThreads pour l'alignement
				jrobinParagraph.add(new Phrase("\n\n\n\n\n"));
			}
			final Image image = Image.getInstance(graph);
			image.scalePercent(50);
			jrobinParagraph.add(new Phrase(new Chunk(image, 0, 0)));
			jrobinParagraph.add(new Phrase(" "));
			i++;
		}
		jrobinParagraph.add(new Phrase("\n"));
		addToDocument(jrobinParagraph);
	}

	private void writeGraphDetails() throws IOException, DocumentException {
		if (collector.isStopped()) {
			return;
		}
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
			final Collection<JRobin> counterJRobins = collector.getDisplayedCounterJRobins();
			if (counterJRobins.isEmpty()) {
				return;
			}
			for (final JRobin jrobin : counterJRobins) {
				// la hauteur de l'image est prévue pour qu'il n'y ait pas de graph seul sur une page
				final Image image = Image
						.getInstance(jrobin.graph(range, LARGE_GRAPH_WIDTH, LARGE_GRAPH_HEIGHT));
				jrobinTable.addCell(image);
			}
		}
		newPage();
		addToDocument(jrobinTable);
		newPage();
	}

	private List<PdfCounterReport> writeCounters(List<Counter> counters)
			throws IOException, DocumentException {
		final List<PdfCounterReport> pdfCounterReports = new ArrayList<PdfCounterReport>();
		for (final Counter counter : counters) {
			pdfCounterReports.add(writeCounter(counter));
		}
		return pdfCounterReports;
	}

	private PdfCounterReport writeCounter(Counter counter) throws DocumentException, IOException {
		final String counterLabel = getString(counter.getName() + "Label");
		addParagraph(getFormattedString("Statistiques_compteur", counterLabel) + " - "
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

	private void writeCounterDetails(PdfCounterReport pdfCounterReport)
			throws DocumentException, IOException {
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
			addToDocument(new Phrase(eol, normalFont));
			final PdfThreadInformationsReport pdfThreadInformationsReport = new PdfThreadInformationsReport(
					javaInformations.getThreadInformationsList(),
					javaInformations.isStackTraceEnabled(), pdfDocumentFactory, getDocument());
			pdfThreadInformationsReport.writeIntro(javaInformations);
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

	private void writeJCaches(boolean includeDetails) throws DocumentException {
		String eol = "";
		for (final JavaInformations javaInformations : javaInformationsList) {
			if (!javaInformations.isJCacheEnabled()) {
				continue;
			}
			final List<JCacheInformations> jcacheInformationsList = javaInformations
					.getJCacheInformationsList();
			final String msg = getFormattedString("caches_sur", jcacheInformationsList.size(),
					javaInformations.getHost());
			addToDocument(new Phrase(eol + msg, boldFont));

			if (includeDetails) {
				new PdfJCacheInformationsReport(jcacheInformationsList, getDocument()).toPdf();
			}
			eol = "\n";
		}
	}

	private boolean isJCacheEnabled() {
		for (final JavaInformations javaInformations : javaInformationsList) {
			if (javaInformations.isJCacheEnabled()) {
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

	private void addParagraph(String paragraphTitle, String iconName)
			throws DocumentException, IOException {
		addToDocument(pdfDocumentFactory.createParagraphElement(paragraphTitle, iconName));
	}
}
