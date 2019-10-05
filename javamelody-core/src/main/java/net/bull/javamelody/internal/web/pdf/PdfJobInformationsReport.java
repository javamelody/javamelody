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
import java.text.DateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import com.lowagie.text.Anchor;
import com.lowagie.text.BadElementException;
import com.lowagie.text.Chunk;
import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.Font;
import com.lowagie.text.Image;
import com.lowagie.text.Paragraph;
import com.lowagie.text.Phrase;
import com.lowagie.text.pdf.PdfPCell;

import net.bull.javamelody.internal.common.I18N;
import net.bull.javamelody.internal.model.Counter;
import net.bull.javamelody.internal.model.CounterRequest;
import net.bull.javamelody.internal.model.JobInformations;

/**
 * Partie du rapport pdf pour les jobs.
 * @author Emeric Vernat
 */
class PdfJobInformationsReport extends PdfAbstractTableReport {
	private static final long ONE_DAY_MILLIS = 24L * 60 * 60 * 1000;
	private final List<JobInformations> jobInformationsList;
	private final Counter jobCounter;
	private final DateFormat fireTimeFormat = I18N.createDateAndTimeFormat();
	private final DateFormat durationFormat = I18N.createDurationFormat();
	private final Font cellFont = PdfFonts.TABLE_CELL.getFont();

	PdfJobInformationsReport(List<JobInformations> jobInformationsList, Counter rangeJobCounter,
			Document document) {
		super(document);
		assert jobInformationsList != null;
		assert rangeJobCounter != null;

		this.jobInformationsList = jobInformationsList;
		this.jobCounter = rangeJobCounter;
	}

	@Override
	void toPdf() throws DocumentException, IOException {
		writeHeader();

		for (final JobInformations jobInformations : jobInformationsList) {
			nextRow();
			writeJobInformations(jobInformations);
		}
		addTableToDocument();
		addConfigurationReference();
	}

	private void addConfigurationReference() throws DocumentException {
		final Anchor quartzAnchor = new Anchor("Configuration reference", PdfFonts.BLUE.getFont());
		quartzAnchor.setName("Quartz configuration reference");
		quartzAnchor.setReference("http://www.quartz-scheduler.org/docs/index.html");
		quartzAnchor.setFont(PdfFonts.BLUE.getFont());
		final Paragraph quartzParagraph = new Paragraph();
		quartzParagraph.add(quartzAnchor);
		quartzParagraph.setAlignment(Element.ALIGN_RIGHT);
		addToDocument(quartzParagraph);
	}

	private void writeHeader() throws DocumentException {
		final List<String> headers = createHeaders();
		final int[] relativeWidths = new int[headers.size()];
		Arrays.fill(relativeWidths, 0, headers.size(), 2);
		relativeWidths[1] = 3; // nom
		relativeWidths[2] = 5; // nom de la classe
		relativeWidths[headers.size() - 1] = 1; // paused

		initTable(headers, relativeWidths);
	}

	private List<String> createHeaders() {
		final List<String> headers = new ArrayList<String>();
		headers.add(getString("JobGroup"));
		headers.add(getString("JobName"));
		headers.add(getString("JobClassName"));
		headers.add(getString("JobMeanTime"));
		headers.add(getString("JobElapsedTime"));
		headers.add(getString("JobPreviousFireTime"));
		headers.add(getString("JobNextFireTime"));
		headers.add(getString("JobPeriodOrCronExpression"));
		headers.add(getString("JobPaused"));
		return headers;
	}

	private void writeJobInformations(JobInformations jobInformations)
			throws BadElementException, IOException {
		final PdfPCell defaultCell = getDefaultCell();
		defaultCell.setHorizontalAlignment(Element.ALIGN_LEFT);
		addCell(jobInformations.getGroup());
		addCell(jobInformations.getName());
		addCell(jobInformations.getJobClassName());
		defaultCell.setHorizontalAlignment(Element.ALIGN_RIGHT);
		final CounterRequest counterRequest = getCounterRequest(jobInformations);
		// counterRequest ne peut pas être null ici
		if (counterRequest.getMean() >= 0) {
			addCell(formatDuration(counterRequest.getMean()));
		} else {
			addCell("");
		}
		// rq: on n'affiche pas ici le nb d'exécutions, le maximum, l'écart-type
		// ou le pourcentage d'erreurs, uniquement car cela ferait trop de colonnes dans la page
		writeJobTimes(jobInformations, counterRequest);
		defaultCell.setHorizontalAlignment(Element.ALIGN_CENTER);
		if (jobInformations.isPaused()) {
			addCell(getString("oui"));
		} else {
			addCell(getString("non"));
		}
	}

	private String formatDuration(int durationAsMillis) {
		// int to long sans cast pour findbugs
		final long duration = 1L * durationAsMillis;
		return durationFormat.format(new Date(duration));
	}

	private void writeJobTimes(JobInformations jobInformations, CounterRequest counterRequest)
			throws BadElementException, IOException {
		final long elapsedTime = jobInformations.getElapsedTime();
		if (elapsedTime >= 0) {
			final Phrase elapsedTimePhrase = new Phrase(durationFormat.format(elapsedTime),
					cellFont);
			final Image memoryImage = Image
					.getInstance(Bar.toBar(100d * elapsedTime / counterRequest.getMean()), null);
			memoryImage.scalePercent(47);
			elapsedTimePhrase.add(new Chunk("\n"));
			elapsedTimePhrase.add(new Chunk(memoryImage, 0, 0));
			addCell(elapsedTimePhrase);
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
	}

	private CounterRequest getCounterRequest(JobInformations jobInformations) {
		final String jobFullName = jobInformations.getGroup() + '.' + jobInformations.getName();
		// rq: la méthode getCounterRequestByName prend en compte l'éventuelle utilisation du paramètre
		// job-transform-pattern qui peut faire que jobFullName != counterRequest.getName()
		final CounterRequest result = jobCounter.getCounterRequestByName(jobFullName, true);
		// getCounterRequestByName ne peut pas retourner null actuellement
		assert result != null;
		return result;
	}
}
