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
	private final Period period;
	private final Document document;
	private final boolean collectorServer;
	private final PdfDocumentFactory pdfDocumentFactory;
	private final Font normalFont = PdfDocumentFactory.NORMAL_FONT;
	private final Font cellFont = PdfDocumentFactory.TABLE_CELL_FONT;
	private final long start = System.currentTimeMillis();

	PdfReport(Collector collector, boolean collectorServer,
			List<JavaInformations> javaInformationsList, Period period, OutputStream output)
			throws IOException {
		super();
		assert collector != null;
		assert javaInformationsList != null && !javaInformationsList.isEmpty();
		assert period != null;
		assert output != null;

		this.collector = collector;
		this.collectorServer = collectorServer;
		this.javaInformationsList = javaInformationsList;
		this.period = period;

		try {
			pdfDocumentFactory = new PdfDocumentFactory(collector.getApplication(), output);
			this.document = pdfDocumentFactory.createDocument();
		} catch (final DocumentException e) {
			throw createIOException(e);
		}
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
		writeGraphs();

		final List<PdfCounterReport> pdfCounterReports = new ArrayList<PdfCounterReport>();
		for (final Counter counter : collector.getPeriodCountersToBeDisplayed(period)) {
			final String counterLabel = I18N.getString(counter.getName() + "Label");
			addParagraph(I18N.getFormattedString("Statistiques_compteur", counterLabel) + " - "
					+ period.getLabel(), counter.getIconName());
			final PdfCounterReport pdfCounterReport = new PdfCounterReport(collector, counter,
					period, false, document);
			pdfCounterReport.toPdf();
			pdfCounterReports.add(pdfCounterReport);
		}

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

		document.newPage();
		addParagraph(getI18nString("Statistiques_detaillees"), "systemmonitor.png");
		writeGraphDetails();

		for (final PdfCounterReport pdfCounterReport : pdfCounterReports) {
			final String counterLabel = I18N.getString(pdfCounterReport.getCounterName() + "Label");
			addParagraph(I18N.getFormattedString("Statistiques_compteur_detaillees", counterLabel)
					+ " - " + period.getLabel(), pdfCounterReport.getCounterIconName());
			pdfCounterReport.writeRequestDetails();
			if (pdfCounterReport.isErrorCounter()) {
				addParagraph(I18N.getString(pdfCounterReport.getCounterName() + "ErrorLabel")
						+ " - " + period.getLabel(), pdfCounterReport.getCounterIconName());
				pdfCounterReport.writeErrorDetails();
			}
		}

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

		writePoweredBy();

		writeDuration();
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

	private void writeGraphs() throws IOException, DocumentException {
		final Paragraph jrobinParagraph = new Paragraph("", PdfDocumentFactory.getFont(9f,
				Font.NORMAL));
		jrobinParagraph.setAlignment(Element.ALIGN_CENTER);
		jrobinParagraph.add(new Phrase("\n\n\n\n"));
		for (final JRobin jrobin : collector.getCounterJRobins()) {
			// les jrobin de compteurs (qui commencent par le jrobin xxxHitsRate)
			// doivent être sur une même ligne donc on met un <br/> si c'est le premier
			final String jrobinName = jrobin.getName();
			if (isJRobinDisplayed(jrobinName)) {
				if (jrobinName.endsWith("HitsRate")) {
					jrobinParagraph.add(new Phrase("\n\n\n\n\n"));
				}
				final Image image = Image.getInstance(jrobin.graph(period, 200, 50));
				image.scalePercent(50);
				jrobinParagraph.add(new Phrase(new Chunk(image, 0, 0)));
				jrobinParagraph.add(new Phrase(" "));
			}
			if ("httpSessions".equals(jrobinName) || "fileDescriptors".equals(jrobinName)) {
				// un <br/> après httpSessions et avant activeThreads pour l'alignement
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
				final Image image = Image.getInstance(jrobin.graph(period, 960, 370));
				jrobinTable.addCell(image);
			}
		}
		document.add(jrobinTable);
		document.newPage();
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

			if (includeDetails) {
				new PdfThreadInformationsReport(javaInformations.getThreadInformationsList(),
						javaInformations.isStackTraceEnabled(), pdfDocumentFactory, document)
						.toPdf();
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
			add(new Phrase(eol + cacheInformationsList.size() + ' ' + getI18nString("caches_sur")
					+ ' ' + javaInformations.getHost(), PdfDocumentFactory.BOLD_FONT));

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

	private void writePoweredBy() throws DocumentException, IOException {
		final Paragraph paragraph = new Paragraph();
		paragraph.setAlignment(ElementTags.ALIGN_CENTER);
		paragraph.add(new Phrase("\nPowered by   ", normalFont));
		final Image imageBull = PdfDocumentFactory.getImage("logobull.png");
		imageBull.setAnnotation(pdfDocumentFactory.createAnnotation("http://www.bull.com/fr/"));
		imageBull.scalePercent(50);
		paragraph.add(new Chunk(imageBull, 0, 0));
		paragraph.add(new Phrase("     "));
		final Image imageNovaforge = PdfDocumentFactory.getImage("Novaforge2.png");
		imageNovaforge.setAnnotation(pdfDocumentFactory
				.createAnnotation("http://www.bull.com/fr/services/novaforge.php"));
		imageNovaforge.scalePercent(50);
		paragraph.add(new Chunk(imageNovaforge, 0, 0));
		//		add(paragraph);
	}

	private void writeDuration() throws DocumentException {
		final long displayDuration = System.currentTimeMillis() - start;
		add(new Phrase('\n' + getI18nString("temps_derniere_collecte") + ": "
				+ collector.getLastCollectDuration() + ' ' + getI18nString("ms") + '\n'
				+ getI18nString("temps_affichage") + ": " + displayDuration + ' '
				+ getI18nString("ms"), cellFont));
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
