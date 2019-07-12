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
import com.lowagie.text.Paragraph;
import com.lowagie.text.pdf.PdfPCell;

import net.bull.javamelody.internal.common.I18N;
import net.bull.javamelody.internal.model.JCacheInformations;
import net.bull.javamelody.internal.web.html.HtmlJCacheInformationsReport;

/**
 * Partie du rapport pdf pour les caches de données (JCache).
 * @author Emeric Vernat
 */
class PdfJCacheInformationsReport extends PdfAbstractTableReport {
	private final List<JCacheInformations> jcacheInformationsList;
	private final DecimalFormat integerFormat = I18N.createIntegerFormat();
	private final Font cellFont = PdfFonts.TABLE_CELL.getFont();
	private final boolean hitsRatioEnabled;

	PdfJCacheInformationsReport(List<JCacheInformations> jcacheInformationsList,
			Document document) {
		super(document);
		assert jcacheInformationsList != null;
		this.jcacheInformationsList = jcacheInformationsList;
		this.hitsRatioEnabled = HtmlJCacheInformationsReport
				.isHitsRatioEnabled(jcacheInformationsList);
	}

	@Override
	void toPdf() throws DocumentException {
		writeHeader();
		for (final JCacheInformations jcacheInformations : jcacheInformationsList) {
			nextRow();
			writeCacheInformations(jcacheInformations);
		}
		addTableToDocument();
		if (!hitsRatioEnabled) {
			final Paragraph statisticsEnabledParagraph = new Paragraph(
					getString("jcaches_statistics_enable"), cellFont);
			statisticsEnabledParagraph.setAlignment(Element.ALIGN_RIGHT);
			addToDocument(statisticsEnabledParagraph);
		}
	}

	private void writeHeader() throws DocumentException {
		final List<String> headers = createHeaders();
		final int[] relativeWidths = new int[headers.size()];
		Arrays.fill(relativeWidths, 0, headers.size(), 4);
		relativeWidths[headers.size() - 1] = 1;
		initTable(headers, relativeWidths);
	}

	private List<String> createHeaders() {
		final List<String> headers = new ArrayList<String>();
		headers.add(getString("Cache"));
		if (hitsRatioEnabled) {
			headers.add(getString("Efficacite_cache"));
		}
		return headers;
	}

	private void writeCacheInformations(JCacheInformations jcacheInformations) {
		final PdfPCell defaultCell = getDefaultCell();
		defaultCell.setHorizontalAlignment(Element.ALIGN_LEFT);
		addCell(jcacheInformations.getName());
		if (hitsRatioEnabled) {
			defaultCell.setHorizontalAlignment(Element.ALIGN_RIGHT);
			addCell(integerFormat.format(jcacheInformations.getHitsRatio()));
		}
	}
}
