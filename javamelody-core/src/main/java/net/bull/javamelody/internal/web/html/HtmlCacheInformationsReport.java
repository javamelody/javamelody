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
package net.bull.javamelody.internal.web.html;

import java.io.IOException;
import java.io.Writer;
import java.text.DecimalFormat;
import java.util.List;

import net.bull.javamelody.internal.common.I18N;
import net.bull.javamelody.internal.common.Parameters;
import net.bull.javamelody.internal.model.CacheInformations;

/**
 * Partie du rapport html pour les caches de données.
 * @author Emeric Vernat
 */
public class HtmlCacheInformationsReport extends HtmlAbstractReport {
	private final List<CacheInformations> cacheInformationsList;
	private final DecimalFormat integerFormat = I18N.createIntegerFormat();
	private final boolean hitsRatioEnabled;
	private final boolean configurationEnabled;
	private final boolean systemActionsEnabled = Parameters.isSystemActionsEnabled();

	HtmlCacheInformationsReport(List<CacheInformations> cacheInformationsList, Writer writer) {
		super(writer);
		assert cacheInformationsList != null;

		this.cacheInformationsList = cacheInformationsList;
		this.hitsRatioEnabled = isHitsRatioEnabled(cacheInformationsList);
		this.configurationEnabled = isConfigurationEnabled(cacheInformationsList);
	}

	@Override
	void toHtml() throws IOException {
		writeCaches(cacheInformationsList);
		write("<div align='right' class='noPrint'>");
		if (!hitsRatioEnabled) {
			writeln("#caches_statistics_enable#<br/>");
		}
		if (systemActionsEnabled) {
			writeln("<a href='?action=clear_caches" + getCsrfTokenUrlPart()
					+ "' onclick=\"javascript:return confirm('"
					+ getStringForJavascript("confirm_purge_caches") + "');\">");
			writeln("<img src='?resource=user-trash.png' width='18' height='18' alt=\"#Purge_caches#\" /> #Purge_caches#</a>");
			writeln("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
		}
		// writeDirectly pour éviter traduction car # dans l'url
		writeDirectly(
				"<a href='http://ehcache.org/apidocs/2.9/net/sf/ehcache/config/CacheConfiguration.html#field_summary'");
		writeln("target='_blank'>Configuration reference</a></div>");
	}

	private void writeCaches(List<CacheInformations> cacheInformations) throws IOException {
		final HtmlTable table = new HtmlTable();
		table.beginTable(getString("Caches"));
		write("<th>#Cache#</th>");
		if (configurationEnabled) {
			write("<th class='sorttable_numeric'>#Pourcentage_memoire_utilise#</th>");
		}
		write("<th class='sorttable_numeric'>#Nb_objets_en_memoire#</th>");
		write("<th class='sorttable_numeric'>#Nb_objets_sur_disque#</th>");
		if (hitsRatioEnabled) {
			write("<th class='sorttable_numeric'>");
			write(getString("Efficacite_cache_memoire").replaceAll("\n", "<br/>"));
			write("</th><th class='sorttable_numeric'>");
			write(getString("Efficacite_cache").replaceAll("\n", "<br/>"));
			write("</th>");
		}
		if (configurationEnabled) {
			write("<th>#Configuration#</th>");
		}
		if (systemActionsEnabled) {
			write("<th class='noPrint'>#Purger#</th>");
		}
		for (final CacheInformations cacheInfos : cacheInformations) {
			table.nextRow();
			writeCacheInformations(cacheInfos);
		}
		table.endTable();
	}

	private void writeCacheInformations(CacheInformations cacheInformations) throws IOException {
		write("<td>");
		writeDirectly("<a href='?part=cacheKeys&amp;cacheId="
				+ urlEncode(cacheInformations.getName()) + "'>");
		if (cacheInformations.getName().isEmpty()) {
			// cache name may be empty
			write("--");
		} else {
			writeDirectly(htmlEncodeButNotSpace(cacheInformations.getName()));
		}
		writeln("</a>");
		final String nextColumnAlignRight = "</td> <td align='right'>";
		if (configurationEnabled) {
			write(nextColumnAlignRight);
			write(integerFormat.format(cacheInformations.getInMemoryPercentUsed()));
		}
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

		if (systemActionsEnabled) {
			write("<td align='center' class='noPrint'>");
			final String confirmClearCache = javascriptEncode(
					getFormattedString("confirm_purge_cache", cacheInformations.getName()));
			// writeDirectly pour ne pas gérer de traductions si le nom contient '#'
			writeDirectly("<a href='?action=clear_cache&amp;cacheId="
					+ urlEncode(cacheInformations.getName()) + getCsrfTokenUrlPart()
					+ "' onclick=\"javascript:return confirm('" + confirmClearCache + "');\">");
			final String title = htmlEncode(
					getFormattedString("Purge_cache", cacheInformations.getName()));
			writeDirectly("<img src='?resource=user-trash.png' width='16' height='16' alt='" + title
					+ "' title='" + title + "' /></a>");
			write("</td>");
		}
	}

	public static boolean isHitsRatioEnabled(List<CacheInformations> cacheInformationsList) {
		for (final CacheInformations cacheInformations : cacheInformationsList) {
			if (cacheInformations.getHitsRatio() >= 0
					|| cacheInformations.getInMemoryHitsRatio() >= 0) {
				return true;
			}
		}
		return false;
	}

	public static boolean isConfigurationEnabled(List<CacheInformations> cacheInformationsList) {
		for (final CacheInformations cacheInformations : cacheInformationsList) {
			if (cacheInformations.getConfiguration() != null) {
				return true;
			}
		}
		return false;
	}

	void writeCacheWithKeys(String cacheId, boolean withoutHeaders) throws IOException {
		assert cacheInformationsList.size() == 1;
		if (!withoutHeaders) {
			writeBackAndRefreshLinksForCache(cacheId);
			writeln("<br/>");

			writeTitle("caches.png",
					getFormattedString("Keys_cache", htmlEncodeButNotSpace(cacheId)));
		}
		writeCaches(cacheInformationsList);

		writeln("<br/><b>#Keys#</b>");
		writeCacheKeys(cacheInformationsList.get(0));
	}

	private void writeBackAndRefreshLinksForCache(String cacheId) throws IOException {
		writeln("<div class='noPrint'>");
		writeln("<a href='javascript:history.back()'><img src='?resource=action_back.png' alt='#Retour#'/> #Retour#</a>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
		writeDirectly("<a href='?part=cacheKeys&amp;cacheId=" + urlEncode(cacheId) + "'>");
		writeln("<img src='?resource=action_refresh.png' alt='#Actualiser#'/> #Actualiser#</a>");
		writeln("</div>");
	}

	private void writeCacheKeys(CacheInformations cacheInformations) throws IOException {
		final List<?> cacheKeys = cacheInformations.getCacheKeys();
		assert cacheKeys != null;
		if (cacheKeys.isEmpty()) {
			write("<br/>#No_keys#");
			return;
		}
		if (cacheKeys.size() > 20) {
			writeln("<div align='right'>" + cacheKeys.size() + " #Keys#</div>");
			writeln("<br/>");
		}
		final HtmlTable table = new HtmlTable();
		table.beginTable(getString("Keys"));
		write("<th>#Keys#</th>");
		if (systemActionsEnabled) {
			write("<th class='noPrint'>#Purger#</th>");
		}
		final String cacheNameEncoded = urlEncode(cacheInformations.getName());
		final String csrfTokenUrlPart = getCsrfTokenUrlPart();
		final String confirmClearCache = javascriptEncode(
				getFormattedString("confirm_purge_cache", cacheInformations.getName()));
		final String title = htmlEncode(
				getFormattedString("Purge_cache", cacheInformations.getName()));
		for (final Object key : cacheKeys) {
			if (key != null) {
				final String myKey = key.toString();
				table.nextRow();
				writeDirectly("<td>");
				writeDirectly(htmlEncodeButNotSpace(myKey));
				writeDirectly("</td>");
				if (systemActionsEnabled) {
					writeDirectly("<td class='noPrint' style='text-align: center;'>");
					writeDirectly(
							"<a href='?part=cacheKeys&amp;action=clear_cache_key&amp;cacheId=");
					writeDirectly(cacheNameEncoded);
					writeDirectly("&amp;cacheKey=");
					writeDirectly(urlEncode(myKey));
					writeDirectly(csrfTokenUrlPart);
					writeDirectly("' onclick=\"javascript:return confirm('");
					writeDirectly(confirmClearCache);
					writeDirectly("');\">");
					writeDirectly(
							"<img src='?resource=user-trash.png' width='16' height='16' alt='");
					writeDirectly(title);
					writeDirectly("' title='");
					writeDirectly(title);
					writeDirectly("' /></a>");
					writeDirectly("</td>");
				}
			}
		}
		table.endTable();
		writeln("<br/>");
		writeln("<div align='right'>" + cacheKeys.size() + " #Keys#</div>");
	}
}
