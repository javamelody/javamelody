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

import java.io.IOException;
import java.io.Writer;
import java.text.DecimalFormat;
import java.util.List;

import net.bull.javamelody.SamplingProfiler.SampledMethod;

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
		writeDirectly(htmlEncode(hotspot.getClassName()));
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
		writeln("<a href='?part=hotspots&amp;action=clear_hotspots' onclick=\"javascript:return confirm('"
				+ getStringForJavascript("confirm_clear_hotspots") + "');\">");
		writeln("<img width='16' height='16' src='?resource=user-trash.png' alt='#clear_hotspots#' title='#clear_hotspots#' /> #clear_hotspots#</a>");
		writeln("</div>");
	}
}
