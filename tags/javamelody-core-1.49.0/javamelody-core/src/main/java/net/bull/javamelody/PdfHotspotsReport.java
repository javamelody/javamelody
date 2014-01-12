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

import net.bull.javamelody.SamplingProfiler.SampledMethod;

import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.pdf.PdfPCell;

/**
 * Rapport pdf pour les hotspots du sampling.
 * @author Emeric Vernat
 */
class PdfHotspotsReport extends PdfAbstractTableReport {
	private final List<SampledMethod> hotspots;
	private final long totalCount;
	private final DecimalFormat percentFormat = I18N.createPercentFormat();

	PdfHotspotsReport(List<SampledMethod> hotspots, Document document) {
		super(document);
		assert hotspots != null;

		this.hotspots = hotspots;

		long total = 0;
		for (final SampledMethod hotspot : hotspots) {
			total += hotspot.getCount();
		}
		this.totalCount = total;
	}

	@Override
	void toPdf() throws DocumentException {
		writeHeader();

		writeHotspots();
	}

	private void writeHotspots() throws DocumentException {
		for (final SampledMethod hotspot : hotspots) {
			nextRow();
			writeHotspot(hotspot);
		}
		addTableToDocument();
	}

	private void writeHeader() throws DocumentException {
		final List<String> headers = createHeaders();
		final int[] relativeWidths = new int[headers.size()];
		Arrays.fill(relativeWidths, 0, headers.size(), 1);
		relativeWidths[0] = 12; // méthode

		initTable(headers, relativeWidths);
	}

	private List<String> createHeaders() {
		final List<String> headers = new ArrayList<String>();
		headers.add(getString("Methode_executee"));
		headers.add(getString("percent_time"));
		return headers;
	}

	private void writeHotspot(SampledMethod hotspot) {
		final PdfPCell defaultCell = getDefaultCell();
		defaultCell.setHorizontalAlignment(Element.ALIGN_LEFT);
		addCell(hotspot.getClassName() + '.' + hotspot.getMethodName());
		defaultCell.setHorizontalAlignment(Element.ALIGN_RIGHT);
		final double percent = 100d * hotspot.getCount() / totalCount;
		addCell(percentFormat.format(percent));
	}
}
