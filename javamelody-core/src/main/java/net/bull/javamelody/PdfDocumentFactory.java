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

import java.io.IOException;
import java.io.OutputStream;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.lowagie.text.BadElementException;
import com.lowagie.text.ChapterAutoNumber;
import com.lowagie.text.Chunk;
import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.Font;
import com.lowagie.text.HeaderFooter;
import com.lowagie.text.Image;
import com.lowagie.text.PageSize;
import com.lowagie.text.Paragraph;
import com.lowagie.text.Phrase;
import com.lowagie.text.Rectangle;
import com.lowagie.text.pdf.BaseFont;
import com.lowagie.text.pdf.PdfContentByte;
import com.lowagie.text.pdf.PdfPCell;
import com.lowagie.text.pdf.PdfPTable;
import com.lowagie.text.pdf.PdfPageEventHelper;
import com.lowagie.text.pdf.PdfTemplate;
import com.lowagie.text.pdf.PdfWriter;

/**
 * Factory pour les documents pdf.
 * @author Emeric Vernat
 */
class PdfDocumentFactory {
	private final String application;
	private final Range range;
	private final OutputStream output;
	private final Map<String, Image> paragraphImagesByResourceName = new HashMap<String, Image>();
	private final Map<String, Image> smallImagesByResourceName = new HashMap<String, Image>();
	private final Font paragraphTitleFont = PdfFonts.PARAGRAPH_TITLE.getFont();

	private static class PdfAdvancedPageNumberEvents extends PdfPageEventHelper {
		// This is the contentbyte object of the writer
		private PdfContentByte cb;

		// we will put the final number of pages in a template
		private PdfTemplate template;

		// this is the BaseFont we are going to use for the header / footer
		private final BaseFont bf;

		PdfAdvancedPageNumberEvents() throws DocumentException, IOException {
			super();
			bf = BaseFont.createFont(BaseFont.HELVETICA, BaseFont.CP1252, BaseFont.NOT_EMBEDDED);
		}

		// we override the onGenericTag method
		/** {@inheritDoc} */
		@Override
		public void onGenericTag(PdfWriter writer, Document document, Rectangle rect, String text) {
			// rien ici
		}

		// we override the onOpenDocument method
		/** {@inheritDoc} */
		@Override
		public void onOpenDocument(PdfWriter writer, Document document) {
			cb = writer.getDirectContent();
			template = cb.createTemplate(50, 50);
		}

		// we override the onChapter method
		/** {@inheritDoc} */
		@Override
		public void onChapter(PdfWriter writer, Document document, float paragraphPosition,
				Paragraph title) {
			// rien ici
		}

		// we override the onEndPage method
		/** {@inheritDoc} */
		@Override
		public void onEndPage(PdfWriter writer, Document document) {
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

		// we override the onCloseDocument method
		/** {@inheritDoc} */
		@Override
		public void onCloseDocument(PdfWriter writer, Document document) {
			template.beginText();
			template.setFontAndSize(bf, 8);
			template.showText(String.valueOf(writer.getPageNumber() - 1));
			template.endText();
		}
	}

	PdfDocumentFactory(String application, Range range, OutputStream output) {
		super();
		assert application != null;
		assert output != null;
		// range peut être null
		this.application = application;
		this.range = range;
		this.output = output;
	}

	Document createDocument() throws DocumentException, IOException {
		return createDocument(false);
	}

	Document createDocument(boolean landscape) throws DocumentException, IOException {
		// creation of a document-object
		final Rectangle pageSize;
		if (landscape) {
			pageSize = PageSize.A4.rotate();
		} else {
			pageSize = PageSize.A4;
		}
		// marges de 20 à gauche, à droite et en haut pour bien utiliser la largeur
		// et avoir une meilleur lisibilité sur les tableaux larges,
		// mais marge de 40 en bas pour ne pas empiéter sur les numéros de pages
		final Document document = new Document(pageSize, 20, 20, 20, 40);

		final String title;
		if (range == null) {
			title = I18N.getFormattedString("Monitoring_sur", application);
		} else {
			title = I18N.getFormattedString("Monitoring_sur", application) + " - "
					+ range.getLabel();
		}
		createWriter(document, title);

		// we add some meta information to the document (after writer)
		document.addAuthor(application);
		document.addCreator("JavaMelody par E. Vernat, http://javamelody.googlecode.com");
		document.addTitle(title);
		return document;
	}

	// We create a writer that listens to the document and directs a PDF-stream to output
	private void createWriter(Document document, String title) throws DocumentException,
			IOException {
		final PdfWriter writer = PdfWriter.getInstance(document, output);
		//writer.setViewerPreferences(PdfWriter.PageLayoutTwoColumnLeft);

		// title
		final HeaderFooter header = new HeaderFooter(new Phrase(title), false);
		header.setAlignment(Element.ALIGN_LEFT);
		header.setBorder(Rectangle.NO_BORDER);
		document.setHeader(header);

		// simple page numbers : x
		//HeaderFooter footer = new HeaderFooter(new Phrase(), true);
		//footer.setAlignment(Element.ALIGN_RIGHT);
		//footer.setBorder(Rectangle.TOP);
		//document.setFooter(footer);

		// add the event handler for advanced page numbers : x/y
		writer.setPageEvent(new PdfAdvancedPageNumberEvents());
	}

	Element createParagraphElement(String paragraphTitle, String iconName)
			throws DocumentException, IOException {
		final Paragraph paragraph = new Paragraph("", paragraphTitleFont);
		paragraph.setSpacingBefore(5);
		paragraph.setSpacingAfter(5);
		if (iconName != null) {
			paragraph.add(new Chunk(getParagraphImage(iconName), 0, -5));
		}
		final Phrase element = new Phrase(' ' + paragraphTitle, paragraphTitleFont);
		element.setLeading(12);
		paragraph.add(element);
		// chapter pour avoir la liste des signets
		final ChapterAutoNumber chapter = new ChapterAutoNumber(paragraph);
		// sans numéro de chapitre
		chapter.setNumberDepth(0);
		chapter.setBookmarkOpen(false);
		chapter.setTriggerNewPage(false);
		return chapter;
	}

	private Image getParagraphImage(String resourceFileName) throws DocumentException, IOException {
		Image image = paragraphImagesByResourceName.get(resourceFileName);
		if (image == null) {
			image = getImage(resourceFileName);
			image.scaleAbsolute(16, 16);
			paragraphImagesByResourceName.put(resourceFileName, image);
		}
		return image;
	}

	Image getSmallImage(String resourceFileName) throws DocumentException, IOException {
		Image image = smallImagesByResourceName.get(resourceFileName);
		if (image == null) {
			image = getImage(resourceFileName);
			image.scaleAbsolute(8, 8);
			smallImagesByResourceName.put(resourceFileName, image);
		}
		return image;
	}

	static Image getImage(String resourceFileName) throws BadElementException, IOException {
		return Image.getInstance(PdfDocumentFactory.class.getResource(Parameters
				.getResourcePath(resourceFileName)));
	}

	static PdfPTable createPdfPTable(List<String> headers, int[] relativeWidths)
			throws DocumentException {
		assert headers.size() == relativeWidths.length;
		final PdfPTable table = new PdfPTable(headers.size());
		table.setWidthPercentage(100);
		table.setWidths(relativeWidths);
		table.setHeaderRows(1);
		final PdfPCell defaultCell = table.getDefaultCell();
		defaultCell.setGrayFill(0.9f);
		defaultCell.setHorizontalAlignment(Element.ALIGN_CENTER);
		defaultCell.setPaddingLeft(0);
		defaultCell.setPaddingRight(0);
		final Font tableHeaderFont = PdfFonts.TABLE_HEADER.getFont();
		for (final String header : headers) {
			table.addCell(new Phrase(header, tableHeaderFont));
		}
		defaultCell.setPaddingLeft(2);
		defaultCell.setPaddingRight(2);
		return table;
	}
}
