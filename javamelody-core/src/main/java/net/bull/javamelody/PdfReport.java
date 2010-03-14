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

import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import com.lowagie.text.ChapterAutoNumber;
import com.lowagie.text.Chunk;
import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.ElementTags;
import com.lowagie.text.Font;
import com.lowagie.text.Image;
import com.lowagie.text.Paragraph;
import com.lowagie.text.Phrase;
import com.lowagie.text.pdf.PdfPTable;

/**
 * Rapport pdf (avec iText).
 * @author Emeric Vernat
 */
class PdfReport {
	private final Collector collector;
	private final List<JavaInformations> javaInformationsList;
	private final Range range;
	private final Document document;
	private final boolean collectorServer;
	private final PdfDocumentFactory pdfDocumentFactory;
	private final Font normalFont = PdfDocumentFactory.NORMAL_FONT;
	private final Font cellFont = PdfDocumentFactory.TABLE_CELL_FONT;
	private final long start = System.currentTimeMillis();

	PdfReport(Collector collector, boolean collectorServer,
			List<JavaInformations> javaInformationsList, Range range, OutputStream output)
			throws IOException {
		super();
		assert collector != null;
		assert javaInformationsList != null && !javaInformationsList.isEmpty();
		assert range != null;
		assert output != null;

		this.collector = collector;
		this.collectorServer = collectorServer;
		this.javaInformationsList = javaInformationsList;
		this.range = range;

		try {
			pdfDocumentFactory = new PdfDocumentFactory(collector.getApplication(), output);
			this.document = pdfDocumentFactory.createDocument();
		} catch (final DocumentException e) {
			throw createIOException(e);
		}
	}

	PdfReport(Collector collector, boolean collectorServer,
			List<JavaInformations> javaInformationsList, Period period, OutputStream output)
			throws IOException {
		this(collector, collectorServer, javaInformationsList, period.getRange(), output);
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

			writeContent();
		} catch (final DocumentException e) {
			throw createIOException(e);
		}

		document.close();
	}

	private static IOException createIOException(DocumentException e) {
		// Rq: le constructeur de IOException avec message et cause n'existe qu'en jdk 1.6
		final IOException ex = new IOException(e.getMessage());
		ex.initCause(e);
		return ex;
	}

	private void writeContent() throws IOException, DocumentException {
		addParagraph(buildSummary(), "systemmonitor.png");
		writeGraphs(collector.getCounterJRobins());

		final List<PdfCounterReport> pdfCounterReports = writeCounters();

		final List<PdfCounterRequestContextReport> pdfCounterRequestContextReports = new ArrayList<PdfCounterRequestContextReport>();
		if (!collectorServer) {
			addParagraph(getI18nString("Requetes_en_cours"), "hourglass.png");
			// si on n'est pas sur le serveur de collecte il n'y a qu'un javaInformations
			final JavaInformations javaInformations = javaInformationsList.get(0);
			pdfCounterRequestContextReports.addAll(writeCurrentRequests(javaInformations,
					pdfCounterReports));
		}

		add(new Phrase("\n", normalFont));
		addParagraph(getI18nString("Informations_systemes"), "systeminfo.png");
		new PdfJavaInformationsReport(javaInformationsList, document).toPdf();

		addParagraph(getI18nString("Threads"), "threads.png");
		writeThreads(false);

		if (isCacheEnabled()) {
			add(new Phrase("\n", normalFont));
			addParagraph(getI18nString("Caches"), "caches.png");
			writeCaches(false);
		}

		if (isJobEnabled()) {
			add(new Phrase("\n", normalFont));
			addParagraph(getI18nString("Jobs"), "jobs.png");
			writeJobs(false);
		}

		document.newPage();
		addParagraph(getI18nString("Statistiques_detaillees"), "systemmonitor.png");
		writeGraphs(collector.getOtherJRobins());
		writeGraphDetails();

		writeCounterDetails(pdfCounterReports);

		if (!collectorServer) {
			addParagraph(getI18nString("Requetes_en_cours_detaillees"), "hourglass.png");
			// si on n'est pas sur le serveur de collecte il n'y a qu'un javaInformations
			writeCurrentRequestsDetails(pdfCounterRequestContextReports);
		}

		addParagraph(getI18nString("Informations_systemes_detaillees"), "systeminfo.png");
		new PdfJavaInformationsReport(javaInformationsList, document).writeInformationsDetails();

		addParagraph(getI18nString("Threads_detailles"), "threads.png");
		writeThreads(true);

		if (isCacheEnabled()) {
			add(new Phrase("\n", normalFont));
			addParagraph(getI18nString("Caches_detailles"), "caches.png");
			writeCaches(true);
		}

		if (isJobEnabled()) {
			add(new Phrase("\n", normalFont));
			addParagraph(getI18nString("Jobs_detailles"), "jobs.png");
			writeJobs(true);
		}

		writePoweredBy();

		writeDurationAndOverhead();
	}

	private String buildSummary() {
		final String startDate = I18N.createDateAndTimeFormat().format(
				collector.getCounters().get(0).getStartDate());
		final String tmp = I18N.getFormattedString("Statistiques", "JavaMelody", I18N
				.getCurrentDateAndTime(), startDate, collector.getApplication());
		if (javaInformationsList.get(0).getContextDisplayName() != null) {
			return tmp + " (" + javaInformationsList.get(0).getContextDisplayName() + ')';
		}
		return tmp;
	}

	private void writeGraphs(Collection<JRobin> jrobins) throws IOException, DocumentException {
		final Paragraph jrobinParagraph = new Paragraph("", PdfDocumentFactory.getFont(9f,
				Font.NORMAL));
		jrobinParagraph.setAlignment(Element.ALIGN_CENTER);
		jrobinParagraph.add(new Phrase("\n\n\n\n"));
		int i = 0;
		for (final JRobin jrobin : jrobins) {
			final String jrobinName = jrobin.getName();
			if (isJRobinDisplayed(jrobinName)) {
				final Image image = Image.getInstance(jrobin.graph(range, 200, 50));
				image.scalePercent(50);
				jrobinParagraph.add(new Phrase(new Chunk(image, 0, 0)));
				jrobinParagraph.add(new Phrase(" "));
			}
			i++;
			if (i % 3 == 0) {
				// un retour après httpSessions et avant activeThreads pour l'alignement
				jrobinParagraph.add(new Phrase("\n\n\n\n\n"));
			}
		}
		jrobinParagraph.add(new Phrase("\n"));
		add(jrobinParagraph);
	}

	private void writeGraphDetails() throws IOException, DocumentException {
		final PdfPTable jrobinTable = new PdfPTable(1);
		jrobinTable.setHorizontalAlignment(Element.ALIGN_CENTER);
		jrobinTable.setWidthPercentage(100);
		jrobinTable.getDefaultCell().setBorder(0);
		for (final JRobin jrobin : collector.getCounterJRobins()) {
			// les jrobin de compteurs (qui commencent par le jrobin xxxHitsRate)
			// doivent être sur une même ligne donc on met un <br/> si c'est le premier
			final String jrobinName = jrobin.getName();
			if (isJRobinDisplayed(jrobinName)) {
				// la hauteur de l'image est prévue pour qu'il n'y ait pas de graph seul sur une page
				final Image image = Image.getInstance(jrobin.graph(range, 960, 370));
				jrobinTable.addCell(image);
			}
		}
		document.add(jrobinTable);
		document.newPage();
	}

	private List<PdfCounterReport> writeCounters() throws IOException, DocumentException {
		final List<PdfCounterReport> pdfCounterReports = new ArrayList<PdfCounterReport>();
		for (final Counter counter : collector.getRangeCountersToBeDisplayed(range)) {
			final String counterLabel = I18N.getString(counter.getName() + "Label");
			addParagraph(I18N.getFormattedString("Statistiques_compteur", counterLabel) + " - "
					+ range.getLabel(), counter.getIconName());
			final PdfCounterReport pdfCounterReport = new PdfCounterReport(collector, counter,
					range, false, document);
			pdfCounterReport.toPdf();
			pdfCounterReports.add(pdfCounterReport);
		}
		return pdfCounterReports;
	}

	private void writeCounterDetails(List<PdfCounterReport> pdfCounterReports)
			throws DocumentException, IOException {
		for (final PdfCounterReport pdfCounterReport : pdfCounterReports) {
			final String counterLabel = I18N.getString(pdfCounterReport.getCounterName() + "Label");
			addParagraph(I18N.getFormattedString("Statistiques_compteur_detaillees", counterLabel)
					+ " - " + range.getLabel(), pdfCounterReport.getCounterIconName());
			pdfCounterReport.writeRequestDetails();
			if (pdfCounterReport.isErrorCounter()) {
				addParagraph(I18N.getString(pdfCounterReport.getCounterName() + "ErrorLabel")
						+ " - " + range.getLabel(), pdfCounterReport.getCounterIconName());
				pdfCounterReport.writeErrorDetails();
			}
		}
	}

	private boolean isJRobinDisplayed(String jrobinName) {
		// inutile car on ne génère pas les jrobin pour le counter de ce nom là
		//		if (jrobinName.startsWith(Counter.ERROR_COUNTER_NAME)) {
		//			return false;
		//		}
		for (final Counter counter : collector.getCounters()) {
			if (jrobinName.startsWith(counter.getName())) {
				return counter.isDisplayed();
			}
		}
		return true;
	}

	private List<PdfCounterRequestContextReport> writeCurrentRequests(
			JavaInformations javaInformations, List<PdfCounterReport> pdfCounterReports)
			throws IOException, DocumentException {
		final List<PdfCounterRequestContextReport> pdfCounterRequestContextReports = new ArrayList<PdfCounterRequestContextReport>();
		final List<CounterRequestContext> rootCurrentContexts = collector.getRootCurrentContexts();
		if (rootCurrentContexts.isEmpty()) {
			add(new Phrase(getI18nString("Aucune_requete_en_cours"), normalFont));
		} else {
			final PdfCounterRequestContextReport pdfCounterRequestContextReport = new PdfCounterRequestContextReport(
					rootCurrentContexts, pdfCounterReports, javaInformations
							.getThreadInformationsList(), javaInformations.isStackTraceEnabled(),
					pdfDocumentFactory, document);
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
			add(new Phrase(getI18nString("Aucune_requete_en_cours"), normalFont));
		}
	}

	private void writeThreads(boolean includeDetails) throws DocumentException, IOException {
		String eol = "";
		for (final JavaInformations javaInformations : javaInformationsList) {
			add(new Phrase(eol + getI18nString("Threads_sur") + ' ' + javaInformations.getHost()
					+ ": ", PdfDocumentFactory.BOLD_FONT));
			add(new Phrase(I18N.getFormattedString("thread_count", javaInformations
					.getThreadCount(), javaInformations.getPeakThreadCount(), javaInformations
					.getTotalStartedThreadCount()), normalFont));

			final PdfThreadInformationsReport pdfThreadInformationsReport = new PdfThreadInformationsReport(
					javaInformations.getThreadInformationsList(), javaInformations
							.isStackTraceEnabled(), pdfDocumentFactory, document);
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
			final String msg = I18N.getFormattedString("caches_sur", cacheInformationsList.size(),
					javaInformations.getHost(), javaInformations.getCurrentlyExecutingJobCount());
			add(new Phrase(eol + msg, PdfDocumentFactory.BOLD_FONT));

			if (includeDetails) {
				new PdfCacheInformationsReport(cacheInformationsList, document).toPdf();
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

	private void writeJobs(boolean includeDetails) throws DocumentException {
		String eol = "";
		for (final JavaInformations javaInformations : javaInformationsList) {
			if (!javaInformations.isJobEnabled()) {
				continue;
			}
			final List<JobInformations> jobInformationsList = javaInformations
					.getJobInformationsList();
			final String msg = I18N.getFormattedString("jobs_sur", jobInformationsList.size(),
					javaInformations.getHost(), javaInformations.getCurrentlyExecutingJobCount());
			add(new Phrase(eol + msg, PdfDocumentFactory.BOLD_FONT));

			if (includeDetails) {
				new PdfJobInformationsReport(jobInformationsList, document).toPdf();
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

	private void writePoweredBy() throws DocumentException, IOException {
		final Paragraph paragraph = new Paragraph();
		paragraph.setAlignment(ElementTags.ALIGN_CENTER);
		paragraph.add(new Phrase("\nPowered by   ", normalFont));
		final Image imageBull = PdfDocumentFactory.getImage("logobull.png");
		imageBull.setAnnotation(pdfDocumentFactory.createAnnotation("http://www.bull.com/fr/"));
		//		imageBull.scalePercent(50);
		//		paragraph.add(new Chunk(imageBull, 0, 0));
		//		paragraph.add(new Phrase("     "));
		//		final Image imageNovaforge = PdfDocumentFactory.getImage("Novaforge2.png");
		//		imageNovaforge.setAnnotation(pdfDocumentFactory
		//				.createAnnotation("http://www.bull.com/fr/services/novaforge.php"));
		//		imageNovaforge.scalePercent(50);
		//		paragraph.add(new Chunk(imageNovaforge, 0, 0));
		//		add(paragraph);
	}

	private void writeDurationAndOverhead() throws DocumentException {
		final long displayDuration = System.currentTimeMillis() - start;
		final String tmp = '\n' + getI18nString("temps_derniere_collecte") + ": "
				+ collector.getLastCollectDuration() + ' ' + getI18nString("ms") + '\n'
				+ getI18nString("temps_affichage") + ": " + displayDuration + ' '
				+ getI18nString("ms") + '\n' + getI18nString("Estimation_overhead_memoire")
				+ ": < " + (collector.getEstimatedMemorySize() / 1024 / 1024 + 1) + ' '
				+ getI18nString("Mo");
		final String string;
		if (Parameters.JAVAMELODY_VERSION != null) {
			string = tmp + "\n\n" + "JavaMelody " + Parameters.JAVAMELODY_VERSION;
		} else {
			string = tmp;
		}
		add(new Phrase(string, cellFont));
	}

	private void addParagraph(String paragraphTitle, String iconName) throws DocumentException,
			IOException {
		final Paragraph paragraph = new Paragraph("", PdfDocumentFactory.PARAGRAPH_TITLE_FONT);
		paragraph.setSpacingBefore(5);
		paragraph.setSpacingAfter(5);
		if (iconName != null) {
			paragraph.add(new Chunk(pdfDocumentFactory.getParagraphImage(iconName), 0, -5));
		}
		final Phrase element = new Phrase(' ' + paragraphTitle,
				PdfDocumentFactory.PARAGRAPH_TITLE_FONT);
		element.setLeading(12);
		paragraph.add(element);
		// chapter pour avoir la liste des signets
		final ChapterAutoNumber chapter = new ChapterAutoNumber(paragraph);
		// sans numéro de chapitre
		chapter.setNumberDepth(0);
		chapter.setBookmarkOpen(false);
		chapter.setTriggerNewPage(false);
		add(chapter);
	}

	private static String getI18nString(String key) {
		return I18N.getString(key);
	}

	private void add(Element element) throws DocumentException {
		document.add(element);
	}
}
