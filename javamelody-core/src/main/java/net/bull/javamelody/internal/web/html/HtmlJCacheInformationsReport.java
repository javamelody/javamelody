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
import net.bull.javamelody.internal.model.JCacheInformations;

/**
 * Partie du rapport html pour les caches de données (JCache).
 * @author Emeric Vernat
 */
public class HtmlJCacheInformationsReport extends HtmlAbstractReport {
	private final List<JCacheInformations> jcacheInformationsList;
	private final DecimalFormat integerFormat = I18N.createIntegerFormat();
	private final boolean hitsRatioEnabled;
	private final boolean clearEnabled;
	private final boolean systemActionsEnabled = Parameters.isSystemActionsEnabled();

	HtmlJCacheInformationsReport(List<JCacheInformations> jcacheInformationsList, Writer writer) {
		super(writer);
		assert jcacheInformationsList != null;

		this.jcacheInformationsList = jcacheInformationsList;
		this.hitsRatioEnabled = isHitsRatioEnabled(jcacheInformationsList);
		this.clearEnabled = isClearEnabled(jcacheInformationsList);
	}

	@Override
	void toHtml() throws IOException {
		writeJCaches(jcacheInformationsList);
		write("<div align='right' class='noPrint'>");
		if (!hitsRatioEnabled) {
			writeln("#jcaches_statistics_enable#<br/>");
		}
		if (clearEnabled && systemActionsEnabled) {
			writeln("<a href='?action=clear_jcaches" + getCsrfTokenUrlPart()
					+ "' onclick=\"javascript:return confirm('"
					+ getStringForJavascript("confirm_purge_caches") + "');\">");
			writeln("<img src='?resource=user-trash.png' width='18' height='18' alt=\"#Purge_caches#\" /> #Purge_caches#</a>");
			writeln("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
		}
		writeln("</div>");
	}

	private void writeJCaches(List<JCacheInformations> jcacheInformations) throws IOException {
		final HtmlTable table = new HtmlTable();
		table.beginTable(getString("Caches"));
		write("<th>#Cache#</th>");
		if (hitsRatioEnabled) {
			write("<th class='sorttable_numeric'>");
			write(getString("Efficacite_cache").replaceAll("\n", "<br/>"));
			write("</th>");
		}
		if (clearEnabled && systemActionsEnabled) {
			write("<th class='noPrint'>#Purger#</th>");
		}
		for (final JCacheInformations jcacheInfos : jcacheInformations) {
			table.nextRow();
			writeJCacheInformations(jcacheInfos);
		}
		table.endTable();
	}

	private void writeJCacheInformations(JCacheInformations jcacheInformations) throws IOException {
		write("<td>");
		if (jcacheInformations.isAvailableByApi()) {
			writeDirectly("<a href='?part=jcacheKeys&amp;cacheId="
					+ urlEncode(jcacheInformations.getName()) + "'>");
		}
		if (jcacheInformations.getName().isEmpty()) {
			// cache name may be empty
			write("--");
		} else {
			writeDirectly(htmlEncodeButNotSpace(jcacheInformations.getName()));
		}
		if (jcacheInformations.isAvailableByApi()) {
			writeln("</a>");
		}
		final String nextColumnAlignRight = "</td> <td align='right'>";
		if (hitsRatioEnabled) {
			write(nextColumnAlignRight);
			write(integerFormat.format(jcacheInformations.getHitsRatio()));
		}

		write("</td>");

		if (clearEnabled && systemActionsEnabled) {
			write("<td align='center' class='noPrint'>");
			if (jcacheInformations.isAvailableByApi()) {
				final String confirmClearCache = javascriptEncode(
						getFormattedString("confirm_purge_cache", jcacheInformations.getName()));
				// writeDirectly pour ne pas gérer de traductions si le nom contient '#'
				writeDirectly("<a href='?action=clear_jcache&amp;cacheId="
						+ urlEncode(jcacheInformations.getName()) + getCsrfTokenUrlPart()
						+ "' onclick=\"javascript:return confirm('" + confirmClearCache + "');\">");
				final String title = htmlEncode(
						getFormattedString("Purge_cache", jcacheInformations.getName()));
				writeDirectly("<img src='?resource=user-trash.png' width='16' height='16' alt='"
						+ title + "' title='" + title + "' /></a>");
			} else {
				writeln("&nbsp;");
			}
			write("</td>");
		}
	}

	public static boolean isHitsRatioEnabled(List<JCacheInformations> jcacheInformationsList) {
		for (final JCacheInformations jcacheInformations : jcacheInformationsList) {
			if (jcacheInformations.getHitsRatio() >= 0) {
				return true;
			}
		}
		return false;
	}

	public static boolean isClearEnabled(List<JCacheInformations> jcacheInformationsList) {
		for (final JCacheInformations jcacheInformations : jcacheInformationsList) {
			if (jcacheInformations.isAvailableByApi()) {
				return true;
			}
		}
		return false;
	}

	void writeJCacheWithKeys(String cacheId, boolean withoutHeaders) throws IOException {
		assert jcacheInformationsList.size() == 1;
		if (!withoutHeaders) {
			writeBackAndRefreshLinksForCache(cacheId);
			writeln("<br/>");

			writeTitle("caches.png",
					getFormattedString("Keys_cache", htmlEncodeButNotSpace(cacheId)));
		}
		writeJCaches(jcacheInformationsList);

		writeln("<br/><b>#Keys#</b>");
		writeJCacheKeys(jcacheInformationsList.get(0));
	}

	private void writeBackAndRefreshLinksForCache(String cacheId) throws IOException {
		writeln("<div class='noPrint'>");
		writeln("<a href='javascript:history.back()'><img src='?resource=action_back.png' alt='#Retour#'/> #Retour#</a>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
		writeDirectly("<a href='?part=jcacheKeys&amp;cacheId=" + urlEncode(cacheId) + "'>");
		writeln("<img src='?resource=action_refresh.png' alt='#Actualiser#'/> #Actualiser#</a>");
		writeln("</div>");
	}

	private void writeJCacheKeys(JCacheInformations jcacheInformations) throws IOException {
		final List<?> cacheKeys = jcacheInformations.getCacheKeys();
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
		final String cacheNameEncoded = urlEncode(jcacheInformations.getName());
		final String csrfTokenUrlPart = getCsrfTokenUrlPart();
		final String confirmClearCache = javascriptEncode(
				getFormattedString("confirm_purge_cache", jcacheInformations.getName()));
		final String title = htmlEncode(
				getFormattedString("Purge_cache", jcacheInformations.getName()));
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
							"<a href='?part=jcacheKeys&amp;action=clear_jcache_key&amp;cacheId=");
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
