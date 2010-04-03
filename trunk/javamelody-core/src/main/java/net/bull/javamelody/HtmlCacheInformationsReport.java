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

import java.io.IOException;
import java.io.Writer;
import java.text.DecimalFormat;
import java.util.List;

/**
 * Partie du rapport html pour les caches de données.
 * @author Emeric Vernat
 */
class HtmlCacheInformationsReport {
	private final List<CacheInformations> cacheInformationsList;
	private final Writer writer;
	private final DecimalFormat integerFormat = I18N.createIntegerFormat();
	private final boolean hitsRatioEnabled;
	private final boolean configurationEnabled;

	HtmlCacheInformationsReport(List<CacheInformations> cacheInformationsList, Writer writer) {
		super();
		assert cacheInformationsList != null;
		assert writer != null;

		this.cacheInformationsList = cacheInformationsList;
		this.writer = writer;
		this.hitsRatioEnabled = isHitsRatioEnabled(cacheInformationsList);
		this.configurationEnabled = isConfigurationEnabled(cacheInformationsList);
	}

	void toHtml() throws IOException {
		writeln("<table class='sortable' width='100%' border='1' cellspacing='0' cellpadding='2' summary='#Caches#'>");
		write("<thead><tr><th>#Cache#</th>");
		write("<th class='sorttable_numeric'>#Nb_objets_en_memoire#</th>");
		write("<th class='sorttable_numeric'>#Nb_objets_sur_disque#</th>");
		if (hitsRatioEnabled) {
			write("<th class='sorttable_numeric'>");
			write(I18N.getString("Efficacite_cache_memoire").replaceAll("\n", "<br/>"));
			write("</th><th class='sorttable_numeric'>");
			write(I18N.getString("Efficacite_cache").replaceAll("\n", "<br/>"));
			write("</th>");
		}
		if (configurationEnabled) {
			write("<th>#Configuration#</th>");
		}
		writeln("</tr></thead><tbody>");
		boolean odd = false;
		for (final CacheInformations cacheInformations : cacheInformationsList) {
			if (odd) {
				write("<tr class='odd' onmouseover=\"this.className='highlight'\" onmouseout=\"this.className='odd'\">");
			} else {
				write("<tr onmouseover=\"this.className='highlight'\" onmouseout=\"this.className=''\">");
			}
			odd = !odd; // NOPMD
			writeCacheInformations(cacheInformations);
			writeln("</tr>");
		}
		writeln("</tbody></table>");
		write("<div align='right' class='noPrint'>");
		if (Parameters.isSystemActionsEnabled()) {
			writeln("<a href='?action=clear_caches' onclick=\"javascript:return confirm('"
					+ I18N.getStringForJavascript("confirm_purge_caches") + "');\">");
			writeln("<img src='?resource=user-trash.png' width='18' height='18' alt=\"#Purge_caches#\" /> #Purge_caches#</a>");
			writeln("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
		}
		// writer.write pour éviter traduction car # dans l'url
		writer
				.write("<a href='http://ehcache.sourceforge.net/apidocs/net/sf/ehcache/config/CacheConfiguration.html#field_summary'");
		writeln("target='_blank'>Configuration reference</a></div>");
	}

	private void writeCacheInformations(CacheInformations cacheInformations) throws IOException {
		write("<td>");
		write(cacheInformations.getName());
		final String nextColumnAlignRight = "</td> <td align='right'>";
		write(nextColumnAlignRight);
		write(integerFormat.format(cacheInformations.getInMemoryObjectCount()));
		write(nextColumnAlignRight);
		write(integerFormat.format(cacheInformations.getOnDiskObjectCount()));
		if (hitsRatioEnabled) {
			write(nextColumnAlignRight);
			write(integerFormat.format(cacheInformations.getInMemoryHitsRatio()));
			write(nextColumnAlignRight);
			write(integerFormat.format(cacheInformations.getHitsRatio()));
		}
		if (configurationEnabled) {
			write("</td> <td>");
			write(cacheInformations.getConfiguration());
		}
		write("</td>");
	}

	static boolean isHitsRatioEnabled(List<CacheInformations> cacheInformationsList) {
		for (final CacheInformations cacheInformations : cacheInformationsList) {
			if (cacheInformations.getHitsRatio() >= 0
					|| cacheInformations.getInMemoryHitsRatio() >= 0) {
				return true;
			}
		}
		return false;
	}

	static boolean isConfigurationEnabled(List<CacheInformations> cacheInformationsList) {
		for (final CacheInformations cacheInformations : cacheInformationsList) {
			if (cacheInformations.getConfiguration() != null) {
				return true;
			}
		}
		return false;
	}

	private void write(String html) throws IOException {
		I18N.writeTo(html, writer);
	}

	private void writeln(String html) throws IOException {
		I18N.writelnTo(html, writer);
	}
}
