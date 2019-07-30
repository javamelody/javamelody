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

import java.util.Arrays;
import java.util.List;

import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.pdf.PdfPCell;

import net.bull.javamelody.internal.model.DatabaseInformations;

/**
 * Rapport pdf pour les informations sur la base de données.
 * @author Emeric Vernat
 */
class PdfDatabaseInformationsReport extends PdfAbstractTableReport {
	private final DatabaseInformations databaseInformations;

	PdfDatabaseInformationsReport(DatabaseInformations databaseInformations, Document document) {
		super(document);
		assert databaseInformations != null;
		this.databaseInformations = databaseInformations;
	}

	@Override
	void toPdf() throws DocumentException {
		final String[][] values = databaseInformations.getResult();
		// final int nbColumns = databaseInformations.getNbColumns();
		// contrairement au rapport html, le rapport pdf ne s'affiche pas avec des tableaux
		// sur plusieurs colonnes, en particulier car en pdf la largeur des cellules ne s'adapte
		// pas au contenu et donc certaines cellules seraient un peu trop petites pour leur contenu
		final String[] headerValues = values[0];
		writeTableHeaders(headerValues);
		int index = 0;
		for (final String[] row : values) {
			if (index == 0) {
				index++;
				continue;
			}
			nextRow();
			writeRow(row);
			index++;
		}
		addTableToDocument();
	}

	private void writeTableHeaders(String[] headerValues) throws DocumentException {
		final List<String> headers = Arrays.asList(headerValues);
		final int[] relativeWidths = new int[headers.size()];
		Arrays.fill(relativeWidths, 0, headers.size(), 1);
		initTable(headers, relativeWidths);
	}

	private void writeRow(String[] row) {
		final PdfPCell defaultCell = getDefaultCell();
		defaultCell.setVerticalAlignment(Element.ALIGN_TOP);
		for (final String value : row) {
			if (value == null || value.isEmpty()) {
				addCell("");
			} else {
				if (isNumber(value)) {
					defaultCell.setHorizontalAlignment(Element.ALIGN_RIGHT);
				} else {
					defaultCell.setHorizontalAlignment(Element.ALIGN_LEFT);
				}
				addCell(value);
			}
		}
	}

	private static boolean isNumber(String text) {
		final int length = text.length();
		for (int i = 0; i < length; i++) {
			final char c = text.charAt(i);
			if (!Character.isDigit(c) && c != '.') {
				return false;
			}
		}
		return true;
	}
}
