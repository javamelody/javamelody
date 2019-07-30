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

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.Font;
import com.lowagie.text.Phrase;
import com.lowagie.text.pdf.PdfPCell;

import net.bull.javamelody.internal.common.I18N;
import net.bull.javamelody.internal.model.HeapHistogram;
import net.bull.javamelody.internal.model.HeapHistogram.ClassInfo;

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
		writeHeader(heapHistogram.isSourceDisplayed());
		writeClassInfo(heap, totalHeapInstances, totalHeapBytes, heapHistogram.isSourceDisplayed());
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
			writeHeader(false);
			writeClassInfo(permGen, totalPermGenInstances, totalPermGenBytes, false);
		}
	}

	private void writeHeader(boolean sourceDisplayed) throws DocumentException {
		final List<String> headers = createHeaders(sourceDisplayed);
		final int[] relativeWidths = new int[headers.size()];
		Arrays.fill(relativeWidths, 0, headers.size(), 1);
		relativeWidths[0] = 6; // Class
		if (sourceDisplayed) {
			relativeWidths[headers.size() - 1] = 6; // Source
		}

		initTable(headers, relativeWidths);
	}

	private List<String> createHeaders(boolean sourceDisplayed) {
		final List<String> headers = new ArrayList<String>();
		headers.add(getString("Classe"));
		headers.add(getString("Taille"));
		headers.add(getString("pct_taille"));
		headers.add(getString("Instances"));
		headers.add(getString("pct_instances"));
		if (sourceDisplayed) {
			headers.add(getString("Source"));
		}
		return headers;
	}

	private void writeClassInfo(List<ClassInfo> classHistogram, long totalInstances,
			long totalBytes, boolean sourceDisplayed) throws DocumentException {
		for (final ClassInfo classInfo : classHistogram) {
			nextRow();
			writeClassInfoRow(classInfo, totalInstances, totalBytes, sourceDisplayed);
		}
		addTableToDocument();
	}

	private void writeClassInfoRow(ClassInfo classInfo, long totalInstances, long totalBytes,
			boolean sourceDisplayed) {
		final PdfPCell defaultCell = getDefaultCell();
		defaultCell.setHorizontalAlignment(Element.ALIGN_LEFT);
		addCell(classInfo.getName());
		defaultCell.setHorizontalAlignment(Element.ALIGN_RIGHT);
		final long bytes = classInfo.getBytes();
		final long instancesCount = classInfo.getInstancesCount();
		addCell(integerFormat.format(bytes / 1024));
		addCell(integerFormat.format(bytes * 100 / totalBytes));
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
