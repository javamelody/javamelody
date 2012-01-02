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
package net.bull.javamelody;

import java.util.List;
import java.util.Map;

import javax.management.JMException;
import javax.management.MBeanAttributeInfo;
import javax.management.MBeanInfo;
import javax.management.MBeanServer;
import javax.management.ObjectName;

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
class PdfMBeansReport {
	private MBeans mbeans;
	private final Document document;
	private final Font boldFont = PdfFonts.BOLD.getFont();
	private final Font normalFont = PdfFonts.NORMAL.getFont();
	private final Font cellFont = PdfFonts.TABLE_CELL.getFont();
	private int margin;
	private PdfPTable currentTable;

	PdfMBeansReport(Document document) {
		super();
		assert document != null;
		this.document = document;
		// MBeans pour la plateforme
		setMBeans(new MBeans());
	}

	void toPdf() throws JMException, DocumentException {
		writeTree();
	}

	/**
	 * Affiche l'arbre des MBeans.
	 * @throws JMException e
	 * @throws DocumentException e
	 */
	void writeTree() throws JMException, DocumentException {
		// MBeans pour la plateforme
		writeTreeForCurrentMBeans();

		// pour JBoss 5.0.x, les MBeans de JBoss sont dans un autre MBeanServer
		final MBeanServer plateformMBeanServer = MBeans.getPlatformMBeanServer();
		for (final MBeanServer mbeanServer : MBeans.getMBeanServers()) {
			if (mbeanServer != plateformMBeanServer) {
				setMBeans(new MBeans(mbeanServer));
				document.newPage();
				document.add(new Chunk(mbeanServer.getDefaultDomain(), boldFont));
				writeTreeForCurrentMBeans();
			}
		}
	}

	private void writeTreeForCurrentMBeans() throws JMException, DocumentException {
		final Map<String, Map<String, List<ObjectName>>> mapObjectNamesByDomainAndFirstProperty = mbeans
				.getMapObjectNamesByDomainAndFirstProperty();
		for (final Map.Entry<String, Map<String, List<ObjectName>>> entryObjectNamesByDomainAndFirstProperty : mapObjectNamesByDomainAndFirstProperty
				.entrySet()) {
			final String domain = entryObjectNamesByDomainAndFirstProperty.getKey();
			margin = 0;
			addText(domain);
			final Map<String, List<ObjectName>> mapObjectNamesByFirstProperty = entryObjectNamesByDomainAndFirstProperty
					.getValue();
			for (final Map.Entry<String, List<ObjectName>> entryObjectNamesByFirstProperty : mapObjectNamesByFirstProperty
					.entrySet()) {
				final String firstProperty = entryObjectNamesByFirstProperty.getKey();
				margin = 12;
				addText(firstProperty);
				final List<ObjectName> objectNames = entryObjectNamesByFirstProperty.getValue();
				for (final ObjectName name : objectNames) {
					margin = 25;
					String mbean = name.toString();
					final int indexOfComma = mbean.indexOf(',');
					if (indexOfComma != -1) {
						mbean = mbean.substring(indexOfComma + 1);
						addText(mbean);
						margin = 37;
						writeAttributes(name);
					} else {
						writeAttributes(name);
					}
				}
			}
		}
	}

	private void writeAttributes(ObjectName name) throws JMException, DocumentException {
		final MBeanInfo mbeanInfo = mbeans.getMBeanInfo(name);
		final MBeanAttributeInfo[] attributeInfos = mbeanInfo.getAttributes();
		final Map<String, Object> attributes = mbeans.getAttributes(name, attributeInfos);
		final String description = mbeans.formatMBeansDescription(mbeanInfo.getDescription());
		if (description != null || !attributes.isEmpty()) {
			currentTable = createAttributesTable();
			if (description != null) {
				currentTable.getDefaultCell().setColspan(3);
				addCell('(' + description + ')');
				currentTable.getDefaultCell().setColspan(1);
			}
			for (final Map.Entry<String, Object> entryAttributes : attributes.entrySet()) {
				final String attributeName = entryAttributes.getKey();
				final Object attributeValue = entryAttributes.getValue();
				writeAttribute(attributeName, attributeValue, attributeInfos);
			}
			final Paragraph paragraph = new Paragraph();
			paragraph.setIndentationLeft(margin);
			paragraph.add(currentTable);
			document.add(paragraph);
			addText("\n");
		}
	}

	private void writeAttribute(String attributeName, Object attributeValue,
			MBeanAttributeInfo[] attributeInfos) {

		addCell(attributeName);
		addCell(mbeans.formatAttributeValue(attributeValue));
		final String attributeDescription = mbeans.getAttributeDescription(attributeName,
				attributeInfos);
		// les attributs des MBeans de java.lang ont des descriptions égales aux noms,
		// ce sont des descriptions inutiles
		if (attributeDescription != null && !attributeDescription.equals(attributeName)) {
			addCell('(' + attributeDescription + ')');
		} else {
			addCell("");
		}
	}

	private void setMBeans(MBeans mBeans) {
		this.mbeans = mBeans;
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
		document.add(paragraph);
	}

	private void addCell(String string) {
		currentTable.addCell(new Phrase(string, cellFont));
	}
}
