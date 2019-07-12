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

import com.lowagie.text.Anchor;
import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.Font;
import com.lowagie.text.Paragraph;
import com.lowagie.text.pdf.PdfPCell;

import net.bull.javamelody.internal.common.I18N;
import net.bull.javamelody.internal.model.CacheInformations;
import net.bull.javamelody.internal.web.html.HtmlCacheInformationsReport;

/**
 * Partie du rapport pdf pour les caches de données.
 * @author Emeric Vernat
 */
class PdfCacheInformationsReport extends PdfAbstractTableReport {
	private final List<CacheInformations> cacheInformationsList;
	private final DecimalFormat integerFormat = I18N.createIntegerFormat();
	private final Font cellFont = PdfFonts.TABLE_CELL.getFont();
	private final boolean hitsRatioEnabled;
	private final boolean configurationEnabled;

	PdfCacheInformationsReport(List<CacheInformations> cacheInformationsList, Document document) {
		super(document);
		assert cacheInformationsList != null;

		this.cacheInformationsList = cacheInformationsList;
		this.hitsRatioEnabled = HtmlCacheInformationsReport
				.isHitsRatioEnabled(cacheInformationsList);
		this.configurationEnabled = HtmlCacheInformationsReport
				.isConfigurationEnabled(cacheInformationsList);
	}

	@Override
	void toPdf() throws DocumentException {
		writeHeader();

		for (final CacheInformations cacheInformations : cacheInformationsList) {
			nextRow();
			writeCacheInformations(cacheInformations);
		}
		addTableToDocument();
		if (!hitsRatioEnabled) {
			final Paragraph statisticsEnabledParagraph = new Paragraph(
					getString("caches_statistics_enable"), cellFont);
			statisticsEnabledParagraph.setAlignment(Element.ALIGN_RIGHT);
			addToDocument(statisticsEnabledParagraph);
		}
		addConfigurationReference();
	}

	private void addConfigurationReference() throws DocumentException {
		final Anchor ehcacheAnchor = new Anchor("Configuration reference", PdfFonts.BLUE.getFont());
		ehcacheAnchor.setName("Ehcache configuration reference");
		ehcacheAnchor.setReference(
				"http://ehcache.sourceforge.net/apidocs/net/sf/ehcache/config/CacheConfiguration.html#field_summary");
		ehcacheAnchor.setFont(PdfFonts.BLUE.getFont());
		final Paragraph ehcacheParagraph = new Paragraph();
		ehcacheParagraph.add(ehcacheAnchor);
		ehcacheParagraph.setAlignment(Element.ALIGN_RIGHT);
		addToDocument(ehcacheParagraph);
	}

	private void writeHeader() throws DocumentException {
		final List<String> headers = createHeaders();
		final int[] relativeWidths = new int[headers.size()];
		Arrays.fill(relativeWidths, 0, headers.size(), 1);
		if (configurationEnabled) {
			relativeWidths[headers.size() - 1] = 4;
		}

		initTable(headers, relativeWidths);
	}

	private List<String> createHeaders() {
		final List<String> headers = new ArrayList<String>();
		headers.add(getString("Cache"));
		if (configurationEnabled) {
			headers.add(getString("Pourcentage_memoire_utilise"));
		}
		headers.add(getString("Nb_objets_en_memoire"));
		headers.add(getString("Nb_objets_sur_disque"));
		if (hitsRatioEnabled) {
			headers.add(getString("Efficacite_cache_memoire"));
			headers.add(getString("Efficacite_cache"));
		}
		if (configurationEnabled) {
			headers.add(getString("Configuration"));
		}
		return headers;
	}

	private void writeCacheInformations(CacheInformations cacheInformations) {
		final PdfPCell defaultCell = getDefaultCell();
		defaultCell.setHorizontalAlignment(Element.ALIGN_LEFT);
		addCell(cacheInformations.getName());
		defaultCell.setHorizontalAlignment(Element.ALIGN_RIGHT);
		if (configurationEnabled) {
			addCell(integerFormat.format(cacheInformations.getInMemoryPercentUsed()));
		}
		addCell(integerFormat.format(cacheInformations.getInMemoryObjectCount()));
		addCell(integerFormat.format(cacheInformations.getOnDiskObjectCount()));
		if (hitsRatioEnabled) {
			addCell(integerFormat.format(cacheInformations.getInMemoryHitsRatio()));
			addCell(integerFormat.format(cacheInformations.getHitsRatio()));
		}
		if (configurationEnabled) {
			defaultCell.setHorizontalAlignment(Element.ALIGN_LEFT);
			addCell(cacheInformations.getConfiguration());
		}
	}
}
