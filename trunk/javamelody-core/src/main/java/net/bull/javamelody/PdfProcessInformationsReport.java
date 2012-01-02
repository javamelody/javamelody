/*
 * Copyright 2008-2010 by Emeric Vernat
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

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.lowagie.text.Anchor;
import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.Font;
import com.lowagie.text.Paragraph;
import com.lowagie.text.Phrase;
import com.lowagie.text.pdf.PdfPCell;
import com.lowagie.text.pdf.PdfPTable;

/**
 * Rapport pdf pour les processus du système d'exploitation.
 * @author Emeric Vernat
 */
class PdfProcessInformationsReport {
	private final List<ProcessInformations> processInformationsList;
	private final Document document;
	private final boolean windows;
	private final DecimalFormat percentFormat = I18N.createPercentFormat();
	private final DecimalFormat integerFormat = I18N.createIntegerFormat();
	private final Font cellFont = PdfFonts.TABLE_CELL.getFont();
	private PdfPTable currentTable;

	PdfProcessInformationsReport(List<ProcessInformations> processInformationsList,
			Document document) {
		super();
		assert processInformationsList != null;
		assert document != null;

		this.processInformationsList = processInformationsList;
		this.document = document;
		this.windows = HtmlProcessInformationsReport.isWindowsProcessList(processInformationsList);
	}

	void toPdf() throws DocumentException {
		writeHeader();

		writeProcessInformations();

		if (!windows) {
			addPsCommandReference();
		}
	}

	private void addPsCommandReference() throws DocumentException {
		final Anchor psAnchor = new Anchor("ps command reference", PdfFonts.BLUE.getFont());
		psAnchor.setName("ps command reference");
		psAnchor.setReference("http://en.wikipedia.org/wiki/Ps_(Unix)");
		psAnchor.setFont(PdfFonts.BLUE.getFont());
		final Paragraph psParagraph = new Paragraph();
		psParagraph.add(psAnchor);
		psParagraph.setAlignment(Element.ALIGN_RIGHT);
		document.add(psParagraph);
	}

	private void writeProcessInformations() throws DocumentException {
		final PdfPCell defaultCell = getDefaultCell();
		boolean odd = false;
		for (final ProcessInformations processInformations : processInformationsList) {
			if (odd) {
				defaultCell.setGrayFill(0.97f);
			} else {
				defaultCell.setGrayFill(1);
			}
			odd = !odd; // NOPMD
			writeProcessInformations(processInformations);
		}
		document.add(currentTable);
	}

	private void writeHeader() throws DocumentException {
		final List<String> headers = createHeaders();
		final int[] relativeWidths = new int[headers.size()];
		Arrays.fill(relativeWidths, 0, headers.size(), 1);
		if (windows) {
			relativeWidths[0] = 2; // user
		}
		relativeWidths[headers.size() - 1] = 6; // command

		currentTable = PdfDocumentFactory.createPdfPTable(headers, relativeWidths);
	}

	private List<String> createHeaders() {
		final List<String> headers = new ArrayList<String>();
		headers.add(getI18nString("Utilisateur"));
		headers.add(getI18nString("PID"));
		if (!windows) {
			headers.add(getI18nString("cpu"));
			headers.add(getI18nString("mem"));
		}
		headers.add(getI18nString("vsz"));
		if (!windows) {
			headers.add(getI18nString("rss"));
			headers.add(getI18nString("tty"));
			headers.add(getI18nString("stat"));
			headers.add(getI18nString("start"));
		}
		headers.add(getI18nString("cpuTime"));
		headers.add(getI18nString("command"));
		return headers;
	}

	private void writeProcessInformations(ProcessInformations processInformations) {
		final PdfPCell defaultCell = getDefaultCell();
		defaultCell.setHorizontalAlignment(Element.ALIGN_LEFT);
		addCell(processInformations.getUser());
		defaultCell.setHorizontalAlignment(Element.ALIGN_RIGHT);
		addCell(integerFormat.format(processInformations.getPid()));
		if (!windows) {
			addCell(percentFormat.format(processInformations.getCpuPercentage()));
			addCell(percentFormat.format(processInformations.getMemPercentage()));
		}
		addCell(integerFormat.format(processInformations.getVsz()));
		if (!windows) {
			addCell(integerFormat.format(processInformations.getRss()));
			defaultCell.setHorizontalAlignment(Element.ALIGN_LEFT);
			addCell(processInformations.getTty());
			addCell(processInformations.getStat());
			defaultCell.setHorizontalAlignment(Element.ALIGN_RIGHT);
			addCell(processInformations.getStart());
		}
		addCell(processInformations.getCpuTime());
		defaultCell.setHorizontalAlignment(Element.ALIGN_LEFT);
		addCell(processInformations.getCommand());
	}

	private static String getI18nString(String key) {
		return I18N.getString(key);
	}

	private PdfPCell getDefaultCell() {
		return currentTable.getDefaultCell();
	}

	private void addCell(String string) {
		currentTable.addCell(new Phrase(string, cellFont));
	}
}
