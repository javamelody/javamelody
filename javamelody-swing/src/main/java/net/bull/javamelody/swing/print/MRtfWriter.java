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
package net.bull.javamelody.swing.print;

import java.io.OutputStream;

import javax.swing.Icon;
import javax.swing.ImageIcon;

import com.lowagie.text.DocWriter;
import com.lowagie.text.Document;
import com.lowagie.text.Element;
import com.lowagie.text.Font;
import com.lowagie.text.FontFactory;
import com.lowagie.text.HeaderFooter;
import com.lowagie.text.Paragraph;
import com.lowagie.text.Phrase;
import com.lowagie.text.Rectangle;
import com.lowagie.text.rtf.RtfWriter2;
import com.lowagie.text.rtf.field.RtfPageNumber;
import com.lowagie.text.rtf.field.RtfTotalPageNumber;
import com.lowagie.text.rtf.headerfooter.RtfHeaderFooter;

import net.bull.javamelody.I18NAdapter;
import net.bull.javamelody.swing.table.MBasicTable;

/**
 * Objet d'impression/export pour Rtf (portrait ou paysage) en utilisant iText-RTF.
 *
 * @author Emeric Vernat
 */
public class MRtfWriter extends MPdfWriter {
	/**
	 * Icône Word.
	 */
	public static final ImageIcon MSWORD_ICON = new ImageIcon(
			MRtfWriter.class.getResource("/icons/ms word.png"));

	/**
	 * Objet d'impression/export pour Rtf paysage.
	 *
	 * @author Emeric Vernat
	 */
	public static class LandscapeRtfWriter extends MRtfWriter {
		/** Constructeur. */
		public LandscapeRtfWriter() {
			super(true);
		}
	}

	/**
	 * Constructeur.
	 */
	public MRtfWriter() {
		this(false);
	}

	/**
	 * Constructeur avec booléen selon que format Portrait (false) ou Paysage (true).
	 *
	 * @param landscape
	 *           boolean
	 */
	public MRtfWriter(final boolean landscape) {
		super(landscape);
	}

	/**
	 * Méthode abstraite : les instances doivent renvoyer l'extension du fichier exporté.
	 *
	 * @return String
	 */
	@Override
	public String getFileExtension() {
		return "rtf";
	}

	/**
	 * Implémentation de méthode abstraite : renvoye l'icône représentant le type.
	 *
	 * @return Icon
	 */
	@Override
	public Icon getIcon() {
		return MSWORD_ICON;
	}

	/**
	 * Implémentation de méthode abstraite : renvoie le nom.
	 *
	 * @return String
	 */
	@Override
	public String getName() {
		return I18NAdapter.getString(isLandscape() ? "export_rtf_landscape" : "export_rtf");
	}

	/**
	 * We create a writer that listens to the document and directs a RTF-stream to out
	 *
	 * @param table
	 *           MBasicTable
	 * @param document
	 *           Document
	 * @param out
	 *           OutputStream
	 * @return DocWriter
	 */
	@Override
	protected DocWriter createWriter(final MBasicTable table, final Document document,
			final OutputStream out) {
		final RtfWriter2 writer = RtfWriter2.getInstance(document, out);

		// title
		final String title = buildTitle(table);
		if (title != null) {
			final HeaderFooter header = new RtfHeaderFooter(new Paragraph(title));
			header.setAlignment(Element.ALIGN_LEFT);
			header.setBorder(Rectangle.NO_BORDER);
			document.setHeader(header);
			document.addTitle(title);
		}

		// advanced page numbers : x/y
		final Paragraph footerParagraph = new Paragraph();
		final Font font = FontFactory.getFont(FontFactory.TIMES_ROMAN, 12, Font.NORMAL);
		footerParagraph.add(new RtfPageNumber(font));
		footerParagraph.add(new Phrase(" / ", font));
		footerParagraph.add(new RtfTotalPageNumber(font));
		footerParagraph.setAlignment(Element.ALIGN_CENTER);
		final HeaderFooter footer = new RtfHeaderFooter(footerParagraph);
		footer.setBorder(Rectangle.TOP);
		document.setFooter(footer);

		return writer;
	}

	// width rtf plus nécessaire
	// /**
	// * Effectue le rendu des headers.
	// * @param table JTable
	// * @param datatable Table
	// * @throws BadElementException
	// */
	// @Override protected void renderHeaders(JTable table, Table datatable) throws BadElementException {
	// super.renderHeaders(table, datatable);
	// datatable.setWidth(90f); // 90 au lieu de 100 pour avoir une marge dans le RTF
	// }
}
