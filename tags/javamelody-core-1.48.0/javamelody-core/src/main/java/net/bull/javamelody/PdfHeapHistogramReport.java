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

/**
 * Rapport pdf pour l'histogramme mémoire.
 * @author Emeric Vernat
 */
class PdfHeapHistogramReport extends PdfAbstractTableReport {
	private final HeapHistogram heapHistogram;
	private final DecimalFormat integerFormat = I18N.createIntegerFormat();
	private final Font cellFont = PdfFonts.TABLE_CELL.getFont();
	private final Font boldFont = PdfFonts.BOLD_CELL.getFont();

	PdfHeapHistogramReport(HeapHistogram heapHistogram, Document document) {
		super(document);
		assert heapHistogram != null;
		this.heapHistogram = heapHistogram;
	}

	@Override
	void toPdf() throws DocumentException {
		addToDocument(new Phrase(getString("Heap"), boldFont));
		final List<ClassInfo> heap = heapHistogram.getHeapHistogram();
		final long totalHeapInstances = heapHistogram.getTotalHeapInstances();
		final long totalHeapBytes = heapHistogram.getTotalHeapBytes();
		final String separator = ",   ";
		addToDocument(new Phrase("     " + getString("Classes") + ": "
				+ integerFormat.format(heap.size()) + separator + getString("Instances") + ": "
				+ integerFormat.format(totalHeapInstances) + separator + getString("Kilo-Octets")
				+ ": " + integerFormat.format(totalHeapBytes / 1024), cellFont));
		writeHeader(heapHistogram.isSourceDisplayed(), heapHistogram.isDeltaDisplayed());
		writeClassInfo(heap, totalHeapInstances, totalHeapBytes, heapHistogram.isSourceDisplayed(),
				heapHistogram.isDeltaDisplayed());
		final List<ClassInfo> permGen = heapHistogram.getPermGenHistogram();
		if (!permGen.isEmpty()) {
			// avec jrockit, permGen est vide
			addToDocument(new Phrase("\n\n" + getString("PermGen"), boldFont));
			final long totalPermGenInstances = heapHistogram.getTotalPermGenInstances();
			final long totalPermGenBytes = heapHistogram.getTotalPermGenBytes();
			addToDocument(new Phrase("     " + getString("Classes") + ": "
					+ integerFormat.format(permGen.size()) + separator + getString("Instances")
					+ ": " + integerFormat.format(totalPermGenInstances) + separator
					+ getString("Kilo-Octets") + ": "
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

		initTable(headers, relativeWidths);
	}

	private List<String> createHeaders(boolean sourceDisplayed, boolean deltaDisplayed) {
		final List<String> headers = new ArrayList<String>();
		headers.add(getString("Classe"));
		headers.add(getString("Taille"));
		headers.add(getString("pct_taille"));
		if (deltaDisplayed) {
			headers.add(getString("Delta"));
		}
		headers.add(getString("Instances"));
		headers.add(getString("pct_instances"));
		if (sourceDisplayed) {
			headers.add(getString("Source"));
		}
		return headers;
	}

	private void writeClassInfo(List<ClassInfo> classHistogram, long totalInstances,
			long totalBytes, boolean sourceDisplayed, boolean deltaDisplayed)
			throws DocumentException {
		for (final ClassInfo classInfo : classHistogram) {
			nextRow();
			writeClassInfoRow(classInfo, totalInstances, totalBytes, sourceDisplayed,
					deltaDisplayed);
		}
		addTableToDocument();
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
}
