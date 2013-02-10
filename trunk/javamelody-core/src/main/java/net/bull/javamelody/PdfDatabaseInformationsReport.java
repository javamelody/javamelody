/*
 * Copyright 2008-2012 by Emeric Vernat
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

import java.util.Arrays;
import java.util.List;

import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.pdf.PdfPCell;

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
			if (value == null || value.length() == 0) {
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
