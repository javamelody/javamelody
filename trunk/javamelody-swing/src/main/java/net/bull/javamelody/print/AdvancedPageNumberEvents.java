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
package net.bull.javamelody.print;

import java.io.IOException;

import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Paragraph;
import com.lowagie.text.Rectangle;
import com.lowagie.text.pdf.BaseFont;
import com.lowagie.text.pdf.PdfContentByte;
import com.lowagie.text.pdf.PdfPageEventHelper;
import com.lowagie.text.pdf.PdfTemplate;
import com.lowagie.text.pdf.PdfWriter;

/**
 * Advanced page number x/y events.
 * 
 * @author Emeric Vernat
 */
class AdvancedPageNumberEvents extends PdfPageEventHelper {
	// This is the contentbyte object of the writer
	private PdfContentByte cb;

	// we will put the final number of pages in a template
	private PdfTemplate template;

	// this is the BaseFont we are going to use for the header / footer
	private BaseFont bf;

	/**
	 * we override the onGenericTag method.
	 * 
	 * @param writer
	 *           PdfWriter
	 * @param document
	 *           Document
	 * @param rect
	 *           Rectangle
	 * @param text
	 *           String
	 */
	@Override
	public void onGenericTag(final PdfWriter writer, final Document document, final Rectangle rect,
			final String text) {
		// rien ici
	}

	/**
	 * we override the onOpenDocument method.
	 * 
	 * @param writer
	 *           PdfWriter
	 * @param document
	 *           Document
	 */
	@Override
	public void onOpenDocument(final PdfWriter writer, final Document document) {
		try {
			bf = BaseFont.createFont(BaseFont.HELVETICA, BaseFont.CP1252, BaseFont.NOT_EMBEDDED);
			cb = writer.getDirectContent();
			template = cb.createTemplate(50, 50);
		} catch (final DocumentException de) {
			throw new IllegalStateException(de);
		} catch (final IOException ioe) {
			throw new IllegalStateException(ioe);
		}
	}

	/**
	 * we override the onChapter method.
	 * 
	 * @param writer
	 *           PdfWriter
	 * @param document
	 *           Document
	 * @param paragraphPosition
	 *           float
	 * @param title
	 *           Paragraph
	 */
	@Override
	public void onChapter(final PdfWriter writer, final Document document,
			final float paragraphPosition, final Paragraph title) {
		// rien ici
	}

	/**
	 * we override the onEndPage method.
	 * 
	 * @param writer
	 *           PdfWriter
	 * @param document
	 *           Document
	 */
	@Override
	public void onEndPage(final PdfWriter writer, final Document document) {
		final int pageN = writer.getPageNumber();
		final String text = pageN + " / ";
		final float len = bf.getWidthPoint(text, 8);
		cb.beginText();
		cb.setFontAndSize(bf, 8);
		final float width = document.getPageSize().getWidth();
		cb.setTextMatrix(width / 2, 30);
		cb.showText(text);
		cb.endText();
		cb.addTemplate(template, width / 2 + len, 30);
	}

	/**
	 * we override the onCloseDocument method.
	 * 
	 * @param writer
	 *           PdfWriter
	 * @param document
	 *           Document
	 */
	@Override
	public void onCloseDocument(final PdfWriter writer, final Document document) {
		template.beginText();
		template.setFontAndSize(bf, 8);
		template.showText(String.valueOf(writer.getPageNumber() - 1));
		template.endText();
	}
}
