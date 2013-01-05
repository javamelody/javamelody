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

import net.bull.javamelody.MBeanNode.MBeanAttribute;

import com.lowagie.text.Chunk;
import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.Font;
import com.lowagie.text.Paragraph;
import com.lowagie.text.Phrase;
import com.lowagie.text.pdf.PdfPCell;
import com.lowagie.text.pdf.PdfPTable;

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
			writeAttributes(mbean);
		} else {
			writeAttributes(mbean);
		}
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
