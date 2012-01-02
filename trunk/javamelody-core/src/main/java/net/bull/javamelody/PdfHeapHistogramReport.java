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

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import net.bull.javamelody.HeapHistogram.ClassInfo;

import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.Font;
import com.lowagie.text.Phrase;
import com.lowagie.text.pdf.PdfPCell;
import com.lowagie.text.pdf.PdfPTable;

/**
 * Rapport pdf pour l'histogramme mémoire.
 * @author Emeric Vernat
 */
class PdfHeapHistogramReport {
	private final HeapHistogram heapHistogram;
	private final Document document;
	private final DecimalFormat integerFormat = I18N.createIntegerFormat();
	private final Font cellFont = PdfFonts.TABLE_CELL.getFont();
	private final Font boldFont = PdfFonts.BOLD_CELL.getFont();
	private PdfPTable currentTable;

	PdfHeapHistogramReport(HeapHistogram heapHistogram, Document document) {
		super();
		assert heapHistogram != null;
		assert document != null;
		this.heapHistogram = heapHistogram;
		this.document = document;
	}

	void toPdf() throws DocumentException {
		document.add(new Phrase(getI18nString("Heap"), boldFont));
		final List<ClassInfo> heap = heapHistogram.getHeapHistogram();
		final long totalHeapInstances = heapHistogram.getTotalHeapInstances();
		final long totalHeapBytes = heapHistogram.getTotalHeapBytes();
		final String separator = ",   ";
		document.add(new Phrase(
				"     " + getI18nString("Classes") + ": " + integerFormat.format(heap.size())
						+ separator + getI18nString("Instances") + ": "
						+ integerFormat.format(totalHeapInstances) + separator
						+ getI18nString("Kilo-Octets") + ": "
						+ integerFormat.format(totalHeapBytes / 1024), cellFont));
		writeHeader(heapHistogram.isSourceDisplayed(), heapHistogram.isDeltaDisplayed());
		writeClassInfo(heap, totalHeapInstances, totalHeapBytes, heapHistogram.isSourceDisplayed(),
				heapHistogram.isDeltaDisplayed());
		final List<ClassInfo> permGen = heapHistogram.getPermGenHistogram();
		if (!permGen.isEmpty()) {
			// avec jrockit, permGen est vide
			document.add(new Phrase("\n\n" + getI18nString("PermGen"), boldFont));
			final long totalPermGenInstances = heapHistogram.getTotalPermGenInstances();
			final long totalPermGenBytes = heapHistogram.getTotalPermGenBytes();
			document.add(new Phrase("     " + getI18nString("Classes") + ": "
					+ integerFormat.format(permGen.size()) + separator + getI18nString("Instances")
					+ ": " + integerFormat.format(totalPermGenInstances) + separator
					+ getI18nString("Kilo-Octets") + ": "
					+ integerFormat.format(totalPermGenBytes / 1024), cellFont));
			writeHeader(false, false);
			writeClassInfo(permGen, totalPermGenInstances, totalPermGenBytes, false, false);
		}
	}

	private void writeHeader(boolean sourceDisplayed, boolean deltaDisplayed)
			throws DocumentException {
		final List<String> headers = createHeaders(sourceDisplayed, deltaDisplayed);
		final int[] relativeWidths = new int[headers.size()];
		Arrays.fill(relativeWidths, 0, headers.size(), 1);
		relativeWidths[0] = 6; // Class
		if (sourceDisplayed) {
			relativeWidths[headers.size() - 1] = 6; // Source
		}

		currentTable = PdfDocumentFactory.createPdfPTable(headers, relativeWidths);
	}

	private List<String> createHeaders(boolean sourceDisplayed, boolean deltaDisplayed) {
		final List<String> headers = new ArrayList<String>();
		headers.add(getI18nString("Classe"));
		headers.add(getI18nString("Taille"));
		headers.add(getI18nString("pct_taille"));
		if (deltaDisplayed) {
			headers.add(getI18nString("Delta"));
		}
		headers.add(getI18nString("Instances"));
		headers.add(getI18nString("pct_instances"));
		if (sourceDisplayed) {
			headers.add(getI18nString("Source"));
		}
		return headers;
	}

	private void writeClassInfo(List<ClassInfo> classHistogram, long totalInstances,
			long totalBytes, boolean sourceDisplayed, boolean deltaDisplayed)
			throws DocumentException {
		final PdfPCell defaultCell = getDefaultCell();
		boolean odd = false;
		for (final ClassInfo classInfo : classHistogram) {
			if (odd) {
				defaultCell.setGrayFill(0.97f);
			} else {
				defaultCell.setGrayFill(1);
			}
			odd = !odd; // NOPMD
			writeClassInfoRow(classInfo, totalInstances, totalBytes, sourceDisplayed,
					deltaDisplayed);
		}
		document.add(currentTable);
	}

	private void writeClassInfoRow(ClassInfo classInfo, long totalInstances, long totalBytes,
			boolean sourceDisplayed, boolean deltaDisplayed) {
		final PdfPCell defaultCell = getDefaultCell();
		defaultCell.setHorizontalAlignment(Element.ALIGN_LEFT);
		addCell(classInfo.getName());
		defaultCell.setHorizontalAlignment(Element.ALIGN_RIGHT);
		final long bytes = classInfo.getBytes();
		final long instancesCount = classInfo.getInstancesCount();
		addCell(integerFormat.format(bytes / 1024));
		addCell(integerFormat.format(bytes * 100 / totalBytes));
		if (deltaDisplayed) {
			if (classInfo.getBytesDelta() > 0) {
				addCell('+' + integerFormat.format(classInfo.getBytesDelta() / 1024));
			} else {
				addCell(integerFormat.format(classInfo.getBytesDelta() / 1024));
			}
		}
		addCell(integerFormat.format(instancesCount));
		addCell(integerFormat.format(instancesCount * 100 / totalInstances));
		defaultCell.setHorizontalAlignment(Element.ALIGN_LEFT);
		if (sourceDisplayed) {
			final String source = classInfo.getSource();
			if (source == null) {
				addCell("");
			} else {
				addCell(source);
			}
		}
	}

	private static String getI18nString(String key) {
		return I18N.getString(key);
	}

	private PdfPCell getDefaultCell() {
		return currentTable.getDefaultCell();
	}

	private void addCell(String string) {
		currentTable.addCell(new Phrase(string, cellFont));
	}
}
