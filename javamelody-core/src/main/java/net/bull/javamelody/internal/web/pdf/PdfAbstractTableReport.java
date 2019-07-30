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
