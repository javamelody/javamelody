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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.lowagie.text.BadElementException;
import com.lowagie.text.Chunk;
import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.Font;
import com.lowagie.text.Image;
import com.lowagie.text.Phrase;
import com.lowagie.text.pdf.PdfPCell;

import net.bull.javamelody.internal.model.JndiBinding;

/**
 * Partie du rapport pdf pour l'arbre JNDI.
 * @author Emeric Vernat
 */
class PdfJndiReport extends PdfAbstractTableReport {
	private final List<JndiBinding> jndiBindings;
	private final Font cellFont = PdfFonts.TABLE_CELL.getFont();
	private Image folderImage;

	PdfJndiReport(List<JndiBinding> jndiBindings, Document document) {
		super(document);
		assert jndiBindings != null;
		this.jndiBindings = jndiBindings;
	}

	@Override
	void toPdf() throws DocumentException, IOException {
		writeHeader();

		for (final JndiBinding jndiBinding : jndiBindings) {
			nextRow();
			writeJndiBinding(jndiBinding);
		}
		addTableToDocument();
	}

	private void writeHeader() throws DocumentException {
		final List<String> headers = createHeaders();
		final int[] relativeWidths = new int[headers.size()];
		Arrays.fill(relativeWidths, 0, headers.size(), 1);

		initTable(headers, relativeWidths);
	}

	private List<String> createHeaders() {
		final List<String> headers = new ArrayList<String>();
		headers.add(getString("Nom"));
		headers.add(getString("Type"));
		headers.add(getString("Value"));
		return headers;
	}

	private void writeJndiBinding(JndiBinding jndiBinding) throws BadElementException, IOException {
		final PdfPCell defaultCell = getDefaultCell();
		defaultCell.setHorizontalAlignment(Element.ALIGN_LEFT);
		final String name = jndiBinding.getName();
		final String className = jndiBinding.getClassName();
		final String contextPath = jndiBinding.getContextPath();
		final String value = jndiBinding.getValue();
		if (contextPath != null) {
			final Image image = getFolderImage();
			final Phrase phrase = new Phrase("", cellFont);
			phrase.add(new Chunk(image, 0, 0));
			phrase.add(new Chunk(" " + name));
			addCell(phrase);
		} else {
			addCell(name);
		}
		addCell(className != null ? className : "");
		addCell(value != null ? value : "");
	}

	private Image getFolderImage() throws BadElementException, IOException {
		if (folderImage == null) {
			folderImage = PdfDocumentFactory.getImage("folder.png");
			folderImage.scalePercent(40);
		}
		return folderImage;
	}
}
