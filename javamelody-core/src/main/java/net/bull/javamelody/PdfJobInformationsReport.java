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

import java.text.DateFormat;
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
 * Partie du rapport pdf pour les jobs.
 * @author Emeric Vernat
 */
class PdfJobInformationsReport {
	private final List<JobInformations> jobInformationsList;
	private final Document document;
	private final DateFormat fireTimeFormat = I18N.createDateAndTimeFormat();
	private final DateFormat elapsedTimeFormat = I18N.createDurationFormat();
	private final Font cellFont = PdfDocumentFactory.TABLE_CELL_FONT;
	private PdfPTable currentTable;

	PdfJobInformationsReport(List<JobInformations> jobInformationsList, Document document) {
		super();
		assert jobInformationsList != null;
		assert document != null;

		this.jobInformationsList = jobInformationsList;
		this.document = document;
	}

	void toPdf() throws DocumentException {
		writeHeader();

		final PdfPCell defaultCell = getDefaultCell();
		boolean odd = false;
		for (final JobInformations jobInformations : jobInformationsList) {
			if (odd) {
				defaultCell.setGrayFill(0.97f);
			} else {
				defaultCell.setGrayFill(1);
			}
			odd = !odd; // NOPMD
			writeJobInformations(jobInformations);
		}
		document.add(currentTable);
		addConfigurationReference();
	}

	private void addConfigurationReference() throws DocumentException {
		final Anchor quartzAnchor = new Anchor("Configuration reference",
				PdfDocumentFactory.BLUE_FONT);
		quartzAnchor.setName("Quartz configuration reference");
		quartzAnchor.setReference("http://www.quartz-scheduler.org/docs/index.html");
		quartzAnchor.setFont(PdfDocumentFactory.BLUE_FONT);
		final Paragraph quartzParagraph = new Paragraph();
		quartzParagraph.add(quartzAnchor);
		quartzParagraph.setAlignment(Element.ALIGN_RIGHT);
		document.add(quartzParagraph);
	}

	private void writeHeader() throws DocumentException {
		final List<String> headers = createHeaders();
		final int[] relativeWidths = new int[headers.size()];
		Arrays.fill(relativeWidths, 0, headers.size(), 1);
		relativeWidths[1] = 2;
		relativeWidths[2] = 3;
		relativeWidths[3] = 3;

		currentTable = PdfDocumentFactory.createPdfPTable(headers, relativeWidths);
	}

	private List<String> createHeaders() {
		final List<String> headers = new ArrayList<String>();
		headers.add(getI18nString("JobGroup"));
		headers.add(getI18nString("JobName"));
		headers.add(getI18nString("JobDescription"));
		headers.add(getI18nString("JobClassName"));
		headers.add(getI18nString("JobPreviousFireTime"));
		headers.add(getI18nString("JobNextFireTime"));
		headers.add(getI18nString("JobElapsedTime"));
		headers.add(getI18nString("JobPaused"));
		return headers;
	}

	private void writeJobInformations(JobInformations jobInformations) {
		final PdfPCell defaultCell = getDefaultCell();
		defaultCell.setHorizontalAlignment(Element.ALIGN_LEFT);
		addCell(jobInformations.getGroup());
		addCell(jobInformations.getName());
		if (jobInformations.getDescription() != null) {
			addCell(jobInformations.getDescription());
		} else {
			addCell("");
		}
		addCell(jobInformations.getJobClassName());
		defaultCell.setHorizontalAlignment(Element.ALIGN_RIGHT);
		if (jobInformations.getPreviousFireTime() != null) {
			addCell(fireTimeFormat.format(jobInformations.getPreviousFireTime()));
		} else {
			addCell("");
		}
		if (jobInformations.getNextFireTime() != null) {
			addCell(fireTimeFormat.format(jobInformations.getNextFireTime()));
		} else {
			addCell("");
		}
		if (jobInformations.getElapsedTime() >= 0) {
			addCell(elapsedTimeFormat.format(jobInformations.getElapsedTime()));
		} else {
			addCell("");
		}
		defaultCell.setHorizontalAlignment(Element.ALIGN_CENTER);
		if (jobInformations.isPaused()) {
			addCell(getI18nString("oui"));
		} else {
			addCell(getI18nString("non"));
		}
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
