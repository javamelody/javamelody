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
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import net.bull.javamelody.HtmlCounterRequestContextReport.CounterRequestContextReportHelper;

import com.lowagie.text.Chunk;
import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.Font;
import com.lowagie.text.Image;
import com.lowagie.text.Paragraph;
import com.lowagie.text.Phrase;
import com.lowagie.text.pdf.PdfPCell;
import com.lowagie.text.pdf.PdfPTable;


/**
 * Partie du rapport pdf pour les contextes de requêtes en cours.
 * @author Emeric Vernat
 */
class PdfCounterRequestContextReport {
	private final List<CounterRequestContext> rootCurrentContexts;
	private final Map<String, PdfCounterReport> counterReportsByCounterName;
	private final Map<Long, ThreadInformations> threadInformationsByThreadId;
	private final Document document;
	private final boolean childHitsDisplayed;
	private final DecimalFormat integerFormat = I18N.createIntegerFormat();
	private final long timeOfSnapshot = System.currentTimeMillis();
	private final boolean stackTraceEnabled;
	private final Font cellFont = PdfDocumentFactory.TABLE_CELL_FONT;
	private final Font normalFont = PdfDocumentFactory.NORMAL_FONT;
	private PdfPTable currentTable;
	private final PdfDocumentFactory pdfDocumentFactory;

	PdfCounterRequestContextReport(List<CounterRequestContext> rootCurrentContexts,
			List<PdfCounterReport> pdfCounterReports,
			List<ThreadInformations> threadInformationsList, boolean stackTraceEnabled,
			PdfDocumentFactory pdfDocumentFactory, Document document) {
		super();
		assert rootCurrentContexts != null;
		assert pdfCounterReports != null;
		assert threadInformationsList != null;
		assert pdfDocumentFactory != null;
		assert document != null;

		this.rootCurrentContexts = rootCurrentContexts;
		this.counterReportsByCounterName = new HashMap<String, PdfCounterReport>(pdfCounterReports
				.size());
		for (final PdfCounterReport counterReport : pdfCounterReports) {
			counterReportsByCounterName.put(counterReport.getCounterName(), counterReport);
		}
		this.threadInformationsByThreadId = new HashMap<Long, ThreadInformations>(
				threadInformationsList.size());
		for (final ThreadInformations threadInformations : threadInformationsList) {
			this.threadInformationsByThreadId.put(threadInformations.getId(), threadInformations);
		}
		this.pdfDocumentFactory = pdfDocumentFactory;
		this.document = document;
		boolean oneRootHasChild = false;
		for (final CounterRequestContext rootCurrentContext : rootCurrentContexts) {
			if (rootCurrentContext.getParentCounter().getChildCounterName() != null) {
				oneRootHasChild = true;
				break;
			}
		}
		this.childHitsDisplayed = oneRootHasChild;
		this.stackTraceEnabled = stackTraceEnabled;
	}

	void toPdf() throws DocumentException, IOException {
		if (rootCurrentContexts.isEmpty()) {
			return;
		}
		writeContexts(Collections.singletonList(rootCurrentContexts.get(0)));
	}

	void writeContextDetails() throws DocumentException, IOException {
		// détails par contextes
		if (rootCurrentContexts.isEmpty()) {
			return;
		}
		writeContexts(rootCurrentContexts);
	}

	private void writeContexts(List<CounterRequestContext> contexts) throws DocumentException,
			IOException {
		boolean displayRemoteUser = false;
		for (final CounterRequestContext context : contexts) {
			if (context.getRemoteUser() != null) {
				displayRemoteUser = true;
				break;
			}
		}
		writeHeader(contexts, displayRemoteUser);

		final PdfPCell defaultCell = getDefaultCell();
		defaultCell.setLeading(2, 1);
		defaultCell.setPaddingTop(0);

		boolean odd = false;
		for (final CounterRequestContext context : contexts) {
			if (odd) {
				defaultCell.setGrayFill(0.97f);
			} else {
				defaultCell.setGrayFill(1);
			}
			odd = !odd; // NOPMD
			writeContext(context, displayRemoteUser);
		}
		document.add(currentTable);

		writeFooter();
	}

	private void writeHeader(List<CounterRequestContext> contexts, boolean displayRemoteUser)
			throws DocumentException {
		final List<String> headers = createHeaders(contexts, displayRemoteUser);
		final int[] relativeWidths = new int[headers.size()];
		Arrays.fill(relativeWidths, 0, headers.size(), 1);
		if (displayRemoteUser) {
			relativeWidths[2] = 6; // requête
		} else {
			relativeWidths[1] = 6; // requête
		}
		if (stackTraceEnabled) {
			relativeWidths[headers.size() - 1] = 3; // méthode exécutée
		}

		currentTable = PdfDocumentFactory.createPdfPTable(headers, relativeWidths);
	}

	private List<String> createHeaders(List<CounterRequestContext> contexts,
			boolean displayRemoteUser) {
		final List<String> headers = new ArrayList<String>();
		headers.add(getI18nString("Thread"));
		if (displayRemoteUser) {
			headers.add(getI18nString("Utilisateur"));
		}
		headers.add(getI18nString("Requete"));
		headers.add(getI18nString("Duree_ecoulee"));
		headers.add(getI18nString("Temps_moyen"));
		headers.add(getI18nString("Temps_cpu"));
		headers.add(getI18nString("Temps_cpu_moyen"));
		// rq : tous ces contextes viennent du même compteur donc peu importe lequel des parentCounter
		if (childHitsDisplayed) {
			final String childCounterName = contexts.get(0).getParentCounter()
					.getChildCounterName();
			headers.add(I18N.getFormattedString("hits_fils", childCounterName));
			headers.add(I18N.getFormattedString("hits_fils_moyens", childCounterName));
			headers.add(I18N.getFormattedString("temps_fils", childCounterName));
			headers.add(I18N.getFormattedString("temps_fils_moyen", childCounterName));
		}
		if (stackTraceEnabled) {
			headers.add(getI18nString("Methode_executee"));
		}
		return headers;
	}

	private void writeFooter() throws DocumentException {
		final Paragraph footer = new Paragraph(I18N.getFormattedString("nb_requete_en_cours",
				integerFormat.format(rootCurrentContexts.size())), normalFont);
		footer.setAlignment(Element.ALIGN_RIGHT);
		document.add(footer);
	}

	private void writeContext(CounterRequestContext rootContext, boolean displayRemoteUser)
			throws DocumentException, IOException {
		getDefaultCell().setHorizontalAlignment(Element.ALIGN_LEFT);
		// attention, cela ne marcherait pas sur le serveur de collecte, à partir du seul threadId
		// s'il y a plusieurs instances en cluster
		final ThreadInformations threadInformations = threadInformationsByThreadId.get(rootContext
				.getThreadId());
		if (threadInformations == null) {
			addCell(""); // un décalage n'a pas permis de récupérer le thread de ce context
		} else {
			addCell(threadInformations.getName());
		}
		if (displayRemoteUser) {
			if (rootContext.getRemoteUser() == null) {
				addCell("");
			} else {
				addCell(rootContext.getRemoteUser());
			}
		}

		final List<CounterRequestContext> contexts = new ArrayList<CounterRequestContext>();
		contexts.add(rootContext);
		contexts.addAll(rootContext.getChildContexts());
		writeRequests(contexts);
		writeDurations(contexts);

		final CounterRequestContextReportHelper counterRequestContextReportHelper = new CounterRequestContextReportHelper(
				contexts, childHitsDisplayed);
		for (final int[] requestValues : counterRequestContextReportHelper.getRequestValues()) {
			writeRequestValues(requestValues);
		}

		if (stackTraceEnabled) {
			getDefaultCell().setHorizontalAlignment(Element.ALIGN_LEFT);
			if (threadInformations == null) {
				addCell(""); // un décalage n'a pas permis de récupérer le thread de ce context
			} else {
				addCell(threadInformations.getExecutedMethod());
			}
		}
	}

	private void writeDurations(List<CounterRequestContext> contexts) throws DocumentException,
			IOException {
		getDefaultCell().setHorizontalAlignment(Element.ALIGN_RIGHT);

		final Paragraph paragraph = new Paragraph("", cellFont);
		boolean first = true;
		for (final CounterRequestContext context : contexts) {
			if (!first) {
				paragraph.add(new Chunk('\n', cellFont));
			}
			final int duration = context.getDuration(timeOfSnapshot);
			final Counter parentCounter = context.getParentCounter();
			final PdfCounterReport counterReport = counterReportsByCounterName.get(parentCounter
					.getName());
			if (parentCounter.getIconName() != null) {
				paragraph.add(new Chunk(getImage(parentCounter.getIconName()), 0, -1));
			}
			paragraph.add(new Phrase(integerFormat.format(duration), counterReport
					.getSlaFont(duration)));
			first = false;
		}
		currentTable.addCell(paragraph);
	}

	private void writeRequests(List<CounterRequestContext> contexts) throws DocumentException,
			IOException {
		final PdfPCell defaultCell = getDefaultCell();
		final PdfPCell requestCell = new PdfPCell();
		final Paragraph phrase = new Paragraph("", cellFont);
		int margin = 0;
		for (final CounterRequestContext context : contexts) {
			writeRequest(context, requestCell, margin);
			margin += 5;
		}
		// on utilise ici PdfPCell et addElement pour que les propriétés
		// leading et indentationLeft des paragraphes soient prises en compte
		requestCell.addElement(phrase);
		requestCell.setGrayFill(defaultCell.getGrayFill());
		requestCell.setPaddingTop(defaultCell.getPaddingTop());
		currentTable.addCell(requestCell);
	}

	private void writeRequest(CounterRequestContext context, PdfPCell cell, int margin)
			throws DocumentException, IOException {
		final Paragraph paragraph = new Paragraph(getDefaultCell().getLeading()
				+ cellFont.getSize());
		paragraph.setIndentationLeft(margin);
		if (context.getParentCounter().getIconName() != null) {
			paragraph.add(new Chunk(getImage(context.getParentCounter().getIconName()), 0, -1));
		}
		paragraph.add(new Phrase(context.getCompleteRequestName(), cellFont));
		cell.addElement(paragraph);
	}

	private void writeRequestValues(int[] requestValues) {
		final StringBuilder sb = new StringBuilder();
		boolean first = true;
		for (final int value : requestValues) {
			if (!first) {
				sb.append('\n');
			}
			if (value != -1) {
				sb.append(integerFormat.format(value));
			}
			first = false;
		}
		addCell(sb.toString());
	}

	private PdfPCell getDefaultCell() {
		return currentTable.getDefaultCell();
	}

	private void addCell(String string) {
		currentTable.addCell(new Phrase(string, cellFont));
	}

	private Image getImage(String resourceFileName) throws DocumentException, IOException {
		return pdfDocumentFactory.getSmallImage(resourceFileName);
	}

	private static String getI18nString(String key) {
		return I18N.getString(key);
	}
}
