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
 * Partie du rapport html pour les processus du système d'exploitation.
 * @author Emeric Vernat
 */
class HtmlProcessInformationsReport {
	private static final boolean PDF_ENABLED = HtmlCoreReport.isPdfEnabled();
	private final List<ProcessInformations> processInformationsList;
	private final Writer writer;
	private final boolean windows;
	private final DecimalFormat percentFormat = I18N.createPercentFormat();
	private final DecimalFormat integerFormat = I18N.createIntegerFormat();

	HtmlProcessInformationsReport(List<ProcessInformations> processInformationsList, Writer writer) {
		super();
		assert processInformationsList != null;
		assert writer != null;

		this.processInformationsList = processInformationsList;
		this.writer = writer;
		this.windows = isWindowsProcessList(processInformationsList);
	}

	void toHtml() throws IOException {
		writeLinks();
		writeln("<br/>");

		writeln("<img src='?resource=processes.png' width='24' height='24' alt='#Processus#' />&nbsp;");
		writeln("<b>#Processus#</b>");
		writeTable();
	}

	void writeTable() throws IOException {
		writeln("<table class='sortable' width='100%' border='1' cellspacing='0' cellpadding='2' summary='#Processus#'>");
		write("<thead><tr><th>#Utilisateur#</th>");
		write("<th class='sorttable_numeric'>#PID#</th>");
		if (!windows) {
			write("<th class='sorttable_numeric'>#cpu#</th><th class='sorttable_numeric'>#mem#</th>");
		}
		write("<th class='sorttable_numeric'>#vsz#</th>");
		if (!windows) {
			write("<th class='sorttable_numeric'>#rss#</th><th>#tty#</th>");
			write("<th>#stat#</th><th>#start#</th>");
		}
		write("<th>#cpuTime#</th><th>#command#</th>");
		writeln("</tr></thead><tbody>");
		boolean odd = false;
		for (final ProcessInformations processInformations : processInformationsList) {
			if (odd) {
				write("<tr class='odd' onmouseover=\"this.className='highlight'\" onmouseout=\"this.className='odd'\">");
			} else {
				write("<tr onmouseover=\"this.className='highlight'\" onmouseout=\"this.className=''\">");
			}
			odd = !odd; // NOPMD
			writeProcessInformations(processInformations);
			writeln("</tr>");
		}
		writeln("</tbody></table>");
		if (!windows) {
			write("<div align='right'>");
			write("<a href='http://en.wikipedia.org/wiki/Ps_(Unix)' target='_blank'>ps command reference</a></div>");
		}
	}

	private void writeProcessInformations(ProcessInformations processInformations)
			throws IOException {
		write("<td>");
		write(htmlEncode(processInformations.getUser()));
		final String newColumnRight = "</td><td align='right'>";
		final String newColumn = "</td><td>";
		write(newColumnRight);
		write(integerFormat.format(processInformations.getPid()));
		if (!windows) {
			write(newColumnRight);
			write(percentFormat.format(processInformations.getCpuPercentage()));
			write(newColumnRight);
			write(percentFormat.format(processInformations.getMemPercentage()));
		}
		write(newColumnRight);
		write(integerFormat.format(processInformations.getVsz()));
		if (!windows) {
			write(newColumnRight);
			write(integerFormat.format(processInformations.getRss()));
			write(newColumn);
			write(htmlEncode(processInformations.getTty()));
			write(newColumn);
			write(htmlEncode(processInformations.getStat()));
			write(newColumnRight);
			write(htmlEncode(processInformations.getStart()));
		}
		write(newColumnRight);
		write(htmlEncode(processInformations.getCpuTime()));
		write(newColumn);
		writer.write(htmlEncode(processInformations.getCommand()));
		write("</td>");
	}

	private void writeLinks() throws IOException {
		writeln("<div class='noPrint'>");
		writeln("<a href='javascript:history.back()'><img src='?resource=action_back.png' alt='#Retour#'/> #Retour#</a>");
		writeln("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
		writeln("<a href='?part=processes'><img src='?resource=action_refresh.png' alt='#Actualiser#'/> #Actualiser#</a>");
		if (PDF_ENABLED) {
			writeln("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
			write("<a href='?part=processes&amp;format=pdf' title='#afficher_PDF#'>");
			write("<img src='?resource=pdf.png' alt='#PDF#'/> #PDF#</a>");
		}
		writeln("</div>");
	}

	static boolean isWindowsProcessList(List<ProcessInformations> processInformationsList) {
		// une liste de process est issue de windows et non linux si toutes les valeurs de tty sont nulles
		for (final ProcessInformations processInformations : processInformationsList) {
			if (processInformations.getTty() != null) {
				return false;
			}
		}
		return true;
	}

	private static String htmlEncode(String text) {
		return I18N.htmlEncode(text, true);
	}

	private void write(String html) throws IOException {
		I18N.writeTo(html, writer);
	}

	private void writeln(String html) throws IOException {
		I18N.writelnTo(html, writer);
	}
}
