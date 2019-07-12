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
import com.lowagie.text.pdf.PdfPCell;

import net.bull.javamelody.internal.common.I18N;
import net.bull.javamelody.internal.model.SamplingProfiler.SampledMethod;

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
