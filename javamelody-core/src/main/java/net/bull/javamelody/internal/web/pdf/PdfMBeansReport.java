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

import com.lowagie.text.Chunk;
import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.Font;
import com.lowagie.text.Paragraph;
import com.lowagie.text.Phrase;
import com.lowagie.text.pdf.PdfPCell;
import com.lowagie.text.pdf.PdfPTable;

import net.bull.javamelody.internal.model.MBeanNode;
import net.bull.javamelody.internal.model.MBeanNode.MBeanAttribute;

/**
 * Partie du rapport pdf pour les MBeans.
 * @author Emeric Vernat
 */
class PdfMBeansReport extends PdfAbstractReport {
	private final List<MBeanNode> mbeans;
	private final Font boldFont = PdfFonts.BOLD.getFont();
	private final Font normalFont = PdfFonts.NORMAL.getFont();
	private final Font cellFont = PdfFonts.TABLE_CELL.getFont();
	private int margin;
	private PdfPTable currentTable;

	PdfMBeansReport(List<MBeanNode> mbeans, Document document) {
		super(document);
		assert mbeans != null;
		this.mbeans = mbeans;
	}

	@Override
	void toPdf() throws DocumentException {
		writeTree();
	}

	/**
	 * Affiche l'arbre des MBeans.
	 * @throws DocumentException e
	 */
	void writeTree() throws DocumentException {
		// MBeans pour la plateforme
		margin = 0;
		final MBeanNode platformNode = mbeans.get(0);
		writeTree(platformNode.getChildren());

		for (final MBeanNode node : mbeans) {
			if (node != platformNode) {
				newPage();
				addToDocument(new Chunk(node.getName(), boldFont));
				margin = 0;
				writeTree(node.getChildren());
			}
		}
	}

	private void writeTree(List<MBeanNode> nodes) throws DocumentException {
		final int marginBackup = margin;
		for (final MBeanNode node : nodes) {
			margin = marginBackup;

			final List<MBeanNode> children = node.getChildren();
			if (children != null) {
				addText(node.getName());
				margin += 12;
				writeTree(children);
			} else {
				writeMBeanNode(node);
			}
		}
	}

	private void writeMBeanNode(MBeanNode mbean) throws DocumentException {
		String mbeanName = mbean.getName();
		final int indexOfComma = mbeanName.indexOf(',');
		if (indexOfComma != -1) {
			mbeanName = mbeanName.substring(indexOfComma + 1);
			addText(mbeanName);
			margin += 13;
		}
		writeAttributes(mbean);
	}

	private void writeAttributes(MBeanNode mbean) throws DocumentException {
		final String description = mbean.getDescription();
		final List<MBeanAttribute> attributes = mbean.getAttributes();
		if (description != null || !attributes.isEmpty()) {
			currentTable = createAttributesTable();
			if (description != null) {
				currentTable.getDefaultCell().setColspan(3);
				addCell('(' + description + ')');
				currentTable.getDefaultCell().setColspan(1);
			}
			for (final MBeanAttribute attribute : attributes) {
				writeAttribute(attribute);
			}
			final Paragraph paragraph = new Paragraph();
			paragraph.setIndentationLeft(margin);
			paragraph.add(currentTable);
			addToDocument(paragraph);
			addText("\n");
		}
	}

	private void writeAttribute(MBeanAttribute attribute) {
		addCell(attribute.getName());
		addCell(attribute.getFormattedValue());
		final String description = attribute.getDescription();
		if (description != null) {
			addCell('(' + description + ')');
		} else {
			addCell("");
		}
	}

	private static PdfPTable createAttributesTable() {
		final PdfPTable table = new PdfPTable(3);
		table.setWidthPercentage(100);
		final PdfPCell defaultCell = table.getDefaultCell();
		defaultCell.setPaddingLeft(2);
		defaultCell.setPaddingRight(2);
		defaultCell.setVerticalAlignment(Element.ALIGN_TOP);
		defaultCell.setBorder(0);
		return table;
	}

	private void addText(String text) throws DocumentException {
		final Paragraph paragraph = new Paragraph(text, normalFont);
		paragraph.setIndentationLeft(margin);
		addToDocument(paragraph);
	}

	private void addCell(String string) {
		currentTable.addCell(new Phrase(string, cellFont));
	}
}
