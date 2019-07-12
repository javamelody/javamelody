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
import net.bull.javamelody.internal.model.SamplingProfiler.SampledMethod;

/**
 * Partie du rapport html pour les hotspots du sampling.
 * @author Emeric Vernat
 */
class HtmlHotspotsReport extends HtmlAbstractReport {
	private final List<SampledMethod> hotspots;
	private final long totalCount;
	private final DecimalFormat percentFormat = I18N.createPercentFormat();

	HtmlHotspotsReport(List<SampledMethod> hotspots, Writer writer) {
		super(writer);
		assert hotspots != null;

		this.hotspots = hotspots;

		long total = 0;
		for (final SampledMethod hotspot : hotspots) {
			total += hotspot.getCount();
		}
		this.totalCount = total;
	}

	@Override
	void toHtml() throws IOException {
		writeLinks();
		writeln("<br/>");

		writeTitle("clock.png", getString("hotspots"));
		writeTable();
	}

	private void writeTable() throws IOException {
		final HtmlTable table = new HtmlTable();
		table.beginTable(getString("hotspots"));
		write("<th>#Methode_executee#</th>");
		write("<th class='sorttable_numeric'>#percent_time#</th>");
		for (final SampledMethod hotspot : hotspots) {
			table.nextRow();
			writeHotspot(hotspot);
		}
		table.endTable();
	}

	private void writeHotspot(SampledMethod hotspot) throws IOException {
		write("<td>");
		writeDirectly(HtmlSourceReport.addLinkToClassName(hotspot.getClassName()));
		write(".<b>");
		writeDirectly(htmlEncode(hotspot.getMethodName()));
		write("</b>");
		write("</td><td align='right'>");
		final double percent = 100d * hotspot.getCount() / totalCount;
		write(percentFormat.format(percent));
		write("</td>");
	}

	void writeLinks() throws IOException {
		writeln("<div class='noPrint'>");
		writeln("<a href='javascript:history.back()'><img src='?resource=action_back.png' alt='#Retour#'/> #Retour#</a>");
		writeln("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
		writeln("<a href='?part=hotspots'><img src='?resource=action_refresh.png' alt='#Actualiser#'/> #Actualiser#</a>");
		if (isPdfEnabled()) {
			writeln("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
			write("<a href='?part=hotspots&amp;format=pdf' title='#afficher_PDF#'>");
			write("<img src='?resource=pdf.png' alt='#PDF#'/> #PDF#</a>");
		}
		writeln("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
		writeln("<a href='?part=hotspots&amp;action=clear_hotspots" + getCsrfTokenUrlPart()
				+ "' onclick=\"javascript:return confirm('"
				+ getStringForJavascript("confirm_clear_hotspots") + "');\">");
		writeln("<img width='16' height='16' src='?resource=user-trash.png' alt='#clear_hotspots#' title='#clear_hotspots#' /> #clear_hotspots#</a>");
		writeln("</div>");
	}
}
