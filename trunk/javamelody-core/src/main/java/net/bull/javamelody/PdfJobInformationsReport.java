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
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

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
	private static final long ONE_DAY_MILLIS = 24L * 60 * 60 * 1000;
	private final List<JobInformations> jobInformationsList;
	private final Map<String, CounterRequest> counterRequestsByRequestName;
	private final Document document;
	private final DateFormat fireTimeFormat = I18N.createDateAndTimeFormat();
	private final DateFormat durationFormat = I18N.createDurationFormat();
	private final Font cellFont = PdfDocumentFactory.TABLE_CELL_FONT;
	private PdfPTable currentTable;

	PdfJobInformationsReport(List<JobInformations> jobInformationsList, Counter rangeJobCounter,
			Document document) {
		super();
		assert jobInformationsList != null;
		assert rangeJobCounter != null;
		assert document != null;

		this.jobInformationsList = jobInformationsList;
		this.document = document;
		final List<CounterRequest> counterRequests = rangeJobCounter.getRequests();
		this.counterRequestsByRequestName = new HashMap<String, CounterRequest>(counterRequests
				.size());
		for (final CounterRequest counterRequest : counterRequests) {
			counterRequestsByRequestName.put(counterRequest.getName(), counterRequest);
		}
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
		Arrays.fill(relativeWidths, 0, headers.size(), 2);
		relativeWidths[1] = 3; // nom
		relativeWidths[2] = 5; // nom de la classe
		relativeWidths[headers.size() - 1] = 1; // paused

		currentTable = PdfDocumentFactory.createPdfPTable(headers, relativeWidths);
	}

	private List<String> createHeaders() {
		final List<String> headers = new ArrayList<String>();
		headers.add(getI18nString("JobGroup"));
		headers.add(getI18nString("JobName"));
		headers.add(getI18nString("JobClassName"));
		headers.add(getI18nString("JobMeanTime"));
		headers.add(getI18nString("JobElapsedTime"));
		headers.add(getI18nString("JobPreviousFireTime"));
		headers.add(getI18nString("JobNextFireTime"));
		headers.add(getI18nString("JobPeriodOrCronExpression"));
		headers.add(getI18nString("JobPaused"));
		return headers;
	}

	private void writeJobInformations(JobInformations jobInformations) {
		final PdfPCell defaultCell = getDefaultCell();
		defaultCell.setHorizontalAlignment(Element.ALIGN_LEFT);
		addCell(jobInformations.getGroup());
		addCell(jobInformations.getName());
		addCell(jobInformations.getJobClassName());
		defaultCell.setHorizontalAlignment(Element.ALIGN_RIGHT);
		final CounterRequest counterRequest = getCounterRequest(jobInformations);
		if (counterRequest != null) {
			addCell(durationFormat.format(new Date(counterRequest.getMean())));
			// rq: on n'affiche pas ici le nb d'exécutions, le maximum, l'écart-type
			// ou le pourcentage d'erreurs, uniquement car cela ferait trop de colonnes dans la page
		} else {
			addCell("");
		}
		if (jobInformations.getElapsedTime() >= 0) {
			addCell(durationFormat.format(jobInformations.getElapsedTime()));
		} else {
			addCell("");
		}
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
		// on n'affiche pas la période si >= 1 jour car ce formateur ne saurait pas l'afficher
		if (jobInformations.getRepeatInterval() > 0
				&& jobInformations.getRepeatInterval() < ONE_DAY_MILLIS) {
			addCell(durationFormat.format(new Date(jobInformations.getRepeatInterval())));
		} else if (jobInformations.getCronExpression() != null) {
			addCell(jobInformations.getCronExpression());
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

	private CounterRequest getCounterRequest(JobInformations jobInformations) {
		final String jobFullName = jobInformations.getGroup() + '.' + jobInformations.getName();
		return counterRequestsByRequestName.get(jobFullName);
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
