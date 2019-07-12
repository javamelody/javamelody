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
import java.io.OutputStream;
import java.util.HashMap;
import java.util.Locale;
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
import com.lowagie.text.pdf.PdfPageEventHelper;
import com.lowagie.text.pdf.PdfTemplate;
import com.lowagie.text.pdf.PdfWriter;

import net.bull.javamelody.internal.common.I18N;
import net.bull.javamelody.internal.common.Parameters;
import net.bull.javamelody.internal.model.Range;

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
		final Rectangle pageSize = getPageSize(landscape);
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
		document.addCreator(
				"JavaMelody par E. Vernat, https://github.com/javamelody/javamelody/wiki");
		document.addTitle(title);
		return document;
	}

	private Rectangle getPageSize(boolean landscape) {
		Rectangle pageSize;
		if (Locale.US.getCountry().equals(I18N.getCurrentLocale().getCountry())) {
			// Letter size paper is used in the US instead of the ISO standard A4
			pageSize = PageSize.LETTER;
		} else {
			pageSize = PageSize.A4;
		}
		if (landscape) {
			pageSize = pageSize.rotate();
		}
		return pageSize;
	}

	// We create a writer that listens to the document and directs a PDF-stream to output
	private void createWriter(Document document, String title)
			throws DocumentException, IOException {
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
		return Image.getInstance(
				PdfDocumentFactory.class.getResource(Parameters.getResourcePath(resourceFileName)));
	}
}
