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
import com.lowagie.text.Font;
import com.lowagie.text.Phrase;
import com.lowagie.text.pdf.PdfPCell;
import com.lowagie.text.pdf.PdfPTable;

/**
 * Rapport pdf pour les informations sur la base de données.
 * @author Emeric Vernat
 */
class PdfDatabaseInformationsReport {
	private final DatabaseInformations databaseInformations;
	private final Document document;
	private final Font cellFont = PdfFonts.TABLE_CELL.getFont();
	private PdfPTable currentTable;

	PdfDatabaseInformationsReport(DatabaseInformations databaseInformations, Document document) {
		super();
		assert databaseInformations != null;
		assert document != null;
		this.databaseInformations = databaseInformations;
		this.document = document;
	}

	void toPdf() throws DocumentException {
		final String[][] values = databaseInformations.getResult();
		// final int nbColumns = databaseInformations.getNbColumns();
		// contrairement au rapport html, le rapport pdf ne s'affiche pas avec des tableaux
		// sur plusieurs colonnes, en particulier car en pdf la largeur des cellules ne s'adapte
		// pas au contenu et donc certaines cellules seraient un peu trop petites pour leur contenu
		final String[] headerValues = values[0];
		writeTableHeaders(headerValues);
		int index = 0;
		final PdfPCell defaultCell = getDefaultCell();
		boolean odd = false;
		for (final String[] row : values) {
			if (index == 0) {
				index++;
				continue;
			}
			if (odd) {
				defaultCell.setGrayFill(0.97f);
			} else {
				defaultCell.setGrayFill(1);
			}
			odd = !odd; // NOPMD
			writeRow(row);
			index++;
		}
		document.add(currentTable);
	}

	private void writeTableHeaders(String[] headerValues) throws DocumentException {
		final List<String> headers = Arrays.asList(headerValues);
		final int[] relativeWidths = new int[headers.size()];
		Arrays.fill(relativeWidths, 0, headers.size(), 1);
		currentTable = PdfDocumentFactory.createPdfPTable(headers, relativeWidths);
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

	private PdfPCell getDefaultCell() {
		return currentTable.getDefaultCell();
	}

	private void addCell(String string) {
		currentTable.addCell(new Phrase(string, cellFont));
	}
}
