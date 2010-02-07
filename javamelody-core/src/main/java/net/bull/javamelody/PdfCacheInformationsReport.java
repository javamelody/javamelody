/*
 * Copyright 2008-2009 by Emeric Vernat, Bull
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

import com.lowagie.text.Anchor;
import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.Font;
import com.lowagie.text.Paragraph;
import com.lowagie.text.Phrase;
import com.lowagie.text.pdf.PdfPCell;
import com.lowagie.text.pdf.PdfPTable;

/**
 * Partie du rapport pdf pour les caches de données.
 * @author Emeric Vernat
 */
class PdfCacheInformationsReport {
	private final List<CacheInformations> cacheInformationsList;
	private final Document document;
	private final DecimalFormat integerFormat = I18N.createIntegerFormat();
	private final Font cellFont = PdfDocumentFactory.TABLE_CELL_FONT;
	private PdfPTable currentTable;
	private final boolean hitsRatioEnabled;
	private final boolean configurationEnabled;

	PdfCacheInformationsReport(List<CacheInformations> cacheInformationsList, Document document) {
		super();
		assert cacheInformationsList != null;
		assert document != null;

		this.cacheInformationsList = cacheInformationsList;
		this.document = document;
		this.hitsRatioEnabled = HtmlCacheInformationsReport
				.isHitsRatioEnabled(cacheInformationsList);
		this.configurationEnabled = HtmlCacheInformationsReport
				.isConfigurationEnabled(cacheInformationsList);
	}

	void toPdf() throws DocumentException {
		writeHeader();

		final PdfPCell defaultCell = getDefaultCell();
		boolean odd = false;
		for (final CacheInformations cacheInformations : cacheInformationsList) {
			if (odd) {
				defaultCell.setGrayFill(0.97f);
			} else {
				defaultCell.setGrayFill(1);
			}
			odd = !odd; // NOPMD
			writeCacheInformations(cacheInformations);
		}
		document.add(currentTable);
		addConfigurationReference();
	}

	private void addConfigurationReference() throws DocumentException {
		final Anchor ehcacheAnchor = new Anchor("Configuration reference",
				PdfDocumentFactory.BLUE_FONT);
		ehcacheAnchor.setName("Ehcache configuration reference");
		ehcacheAnchor
				.setReference("http://ehcache.sourceforge.net/apidocs/net/sf/ehcache/config/CacheConfiguration.html#field_summary");
		ehcacheAnchor.setFont(PdfDocumentFactory.BLUE_FONT);
		final Paragraph ehcacheParagraph = new Paragraph();
		ehcacheParagraph.add(ehcacheAnchor);
		ehcacheParagraph.setAlignment(Element.ALIGN_RIGHT);
		document.add(ehcacheParagraph);
	}

	private void writeHeader() throws DocumentException {
		final List<String> headers = createHeaders();
		final int[] relativeWidths = new int[headers.size()];
		Arrays.fill(relativeWidths, 0, headers.size(), 1);
		if (configurationEnabled) {
			relativeWidths[headers.size() - 1] = 4;
		}

		currentTable = PdfDocumentFactory.createPdfPTable(headers, relativeWidths);
	}

	private List<String> createHeaders() {
		final List<String> headers = new ArrayList<String>();
		headers.add(getI18nString("Cache"));
		headers.add(getI18nString("Nb_objets_en_memoire"));
		headers.add(getI18nString("Nb_objets_sur_disque"));
		if (hitsRatioEnabled) {
			headers.add(getI18nString("Efficacite_cache_memoire"));
			headers.add(getI18nString("Efficacite_cache"));
		}
		if (configurationEnabled) {
			headers.add(getI18nString("Configuration"));
		}
		return headers;
	}

	private void writeCacheInformations(CacheInformations cacheInformations) {
		final PdfPCell defaultCell = getDefaultCell();
		defaultCell.setHorizontalAlignment(Element.ALIGN_LEFT);
		addCell(cacheInformations.getName());
		defaultCell.setHorizontalAlignment(Element.ALIGN_RIGHT);
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
