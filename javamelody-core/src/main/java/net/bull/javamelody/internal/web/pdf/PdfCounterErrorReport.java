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

import java.text.DateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.Font;
import com.lowagie.text.Phrase;

import net.bull.javamelody.internal.common.I18N;
import net.bull.javamelody.internal.model.Counter;
import net.bull.javamelody.internal.model.CounterError;
import net.bull.javamelody.internal.web.html.HtmlCounterErrorReport;

/**
 * Partie du rapport pdf pour les erreurs http et dans les logs.
 * @author Emeric Vernat
 */
class PdfCounterErrorReport extends PdfAbstractTableReport {
	private final Counter counter;
	private final DateFormat dateTimeFormat = DateFormat.getDateTimeInstance(DateFormat.SHORT,
			DateFormat.MEDIUM, I18N.getCurrentLocale());
	private final Font severeFont = PdfFonts.SEVERE_CELL.getFont();
	private final Font normalFont = PdfFonts.NORMAL.getFont();

	PdfCounterErrorReport(Counter counter, Document document) {
		super(document);
		assert counter != null;
		assert counter.isErrorCounter();
		this.counter = counter;
	}

	@Override
	void toPdf() throws DocumentException {
		final List<CounterError> errors = counter.getErrors();
		if (errors.isEmpty()) {
			addToDocument(new Phrase(getString("Aucune_erreur"), normalFont));
		} else {
			writeErrors(errors);
		}
	}

	private void writeErrors(List<CounterError> errors) throws DocumentException {
		assert errors != null;
		final boolean displayUser = HtmlCounterErrorReport.shouldDisplayUser(errors);
		final boolean displayHttpRequest = HtmlCounterErrorReport.shouldDisplayHttpRequest(errors);
		if (errors.size() >= Counter.MAX_ERRORS_COUNT) {
			addToDocument(new Phrase(
					getFormattedString("Dernieres_erreurs_seulement", Counter.MAX_ERRORS_COUNT)
							+ '\n',
					severeFont));
		}
		writeHeader(displayUser, displayHttpRequest);

		for (final CounterError error : errors) {
			nextRow();
			writeError(error, displayUser, displayHttpRequest);
		}
		addTableToDocument();
	}

	private void writeHeader(boolean displayUser, boolean displayHttpRequest)
			throws DocumentException {
		final List<String> headers = createHeaders(displayUser, displayHttpRequest);
		final int[] relativeWidths = new int[headers.size()];
		Arrays.fill(relativeWidths, 0, headers.size(), 1);
		if (displayHttpRequest) {
			relativeWidths[1] = 4; // requête http
		}
		relativeWidths[headers.size() - 1] = 4; // message d'erreur

		initTable(headers, relativeWidths);
	}

	private List<String> createHeaders(boolean displayUser, boolean displayHttpRequest) {
		final List<String> headers = new ArrayList<String>();
		headers.add(getString("Date"));
		if (displayHttpRequest) {
			headers.add(getString("Requete"));
		}
		if (displayUser) {
			headers.add(getString("Utilisateur"));
		}
		headers.add(getString("Erreur"));
		return headers;
	}

	private void writeError(CounterError error, boolean displayUser, boolean displayHttpRequest) {
		getDefaultCell().setHorizontalAlignment(Element.ALIGN_RIGHT);
		addCell(dateTimeFormat.format(error.getDate()));
		getDefaultCell().setHorizontalAlignment(Element.ALIGN_LEFT);
		if (displayHttpRequest) {
			if (error.getHttpRequest() == null) {
				addCell("");
			} else {
				addCell(error.getHttpRequest());
			}
		}
		if (displayUser) {
			if (error.getRemoteUser() == null) {
				addCell("");
			} else {
				addCell(error.getRemoteUser());
			}
		}
		addCell(error.getMessage());
	}
}
