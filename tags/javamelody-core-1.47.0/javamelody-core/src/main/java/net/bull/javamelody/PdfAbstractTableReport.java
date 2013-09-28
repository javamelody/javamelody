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

import java.util.List;

import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.Font;
import com.lowagie.text.Image;
import com.lowagie.text.Phrase;
import com.lowagie.text.pdf.PdfPCell;
import com.lowagie.text.pdf.PdfPTable;

/**
 * Parent abstrait des classes de rapport pdf avec un tableau.
 * @author Emeric Vernat
 */
abstract class PdfAbstractTableReport extends PdfAbstractReport {
	private final Font cellFont = PdfFonts.TABLE_CELL.getFont();
	private PdfPTable table;
	private boolean oddRow;

	PdfAbstractTableReport(Document document) {
		super(document);
	}

	void initTable(List<String> headers, int[] relativeWidths) throws DocumentException {
		assert headers.size() == relativeWidths.length;
		final PdfPTable mytable = new PdfPTable(headers.size());
		mytable.setWidthPercentage(100);
		mytable.setWidths(relativeWidths);
		mytable.setHeaderRows(1);
		final PdfPCell defaultCell = mytable.getDefaultCell();
		defaultCell.setGrayFill(0.9f);
		defaultCell.setHorizontalAlignment(Element.ALIGN_CENTER);
		defaultCell.setPaddingLeft(0);
		defaultCell.setPaddingRight(0);
		final Font tableHeaderFont = PdfFonts.TABLE_HEADER.getFont();
		for (final String header : headers) {
			mytable.addCell(new Phrase(header, tableHeaderFont));
		}
		defaultCell.setPaddingLeft(2);
		defaultCell.setPaddingRight(2);
		this.table = mytable;
	}

	void nextRow() {
		if (oddRow) {
			getDefaultCell().setGrayFill(0.97f);
		} else {
			getDefaultCell().setGrayFill(1);
		}
		oddRow = !oddRow; // NOPMD
	}

	PdfPCell getDefaultCell() {
		return table.getDefaultCell();
	}

	void addCell(String string) {
		table.addCell(new Phrase(string, cellFont));
	}

	void addCell(Phrase phrase) {
		table.addCell(phrase);
	}

	void addCell(Image image) {
		table.addCell(image);
	}

	void addCell(PdfPCell cell) {
		table.addCell(cell);
	}

	/**
	 * Adds the <CODE>table</CODE> to the <CODE>Document</CODE>.
	 *
	 * @return <CODE>true</CODE> if the element was added, <CODE>false
	 *         </CODE> if not
	 * @throws DocumentException
	 *             when a document isn't open yet, or has been closed
	 */
	boolean addTableToDocument() throws DocumentException {
		return addToDocument(table);
	}
}
