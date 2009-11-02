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

import java.io.IOException;
import java.io.Writer;

/**
 * Partie du rapport html pour les informations sur la base de données.
 * @author Emeric Vernat
 */
class HtmlDatabaseInformationsReport {
	private final DatabaseInformations databaseInformations;
	private final Writer writer;

	HtmlDatabaseInformationsReport(DatabaseInformations databaseInformations, Writer writer) {
		super();
		assert databaseInformations != null;
		assert writer != null;
		this.databaseInformations = databaseInformations;
		this.writer = writer;
	}

	void toHtml() throws IOException {
		writeLinks();
		writeln("<br/>");

		writeln("<img src='?resource=db.png' width='24' height='24' alt='#database#' />&nbsp;");
		final String selectedRequestName = databaseInformations.getRequestNames().get(
				databaseInformations.getRequestIndex());
		writeln("<b>#database# : #" + selectedRequestName + "#</b>");

		final String[][] values = databaseInformations.getResult();
		writeln("<table class='sortable' width='100%' border='1' cellspacing='0' cellpadding='2' summary='#database#'>");
		write("<thead><tr>");
		for (final String value : values[0]) {
			write("<th>");
			writer.write(value.replace("\n", "<br/>"));
			write("</th>");
		}
		writeln("</tr></thead><tbody>");
		boolean first = true;
		boolean odd = false;
		for (final String[] row : values) {
			if (first) {
				first = false;
				continue;
			}
			if (odd) {
				write("<tr class='odd' onmouseover=\"this.className='highlight'\" onmouseout=\"this.className='odd'\">");
			} else {
				write("<tr onmouseover=\"this.className='highlight'\" onmouseout=\"this.className=''\">");
			}
			odd = !odd; // NOPMD
			for (final String value : row) {
				if (value == null || value.length() == 0) {
					write("<td>&nbsp;</td>");
				} else {
					if (isNumber(value)) {
						write("<td align='right' valign='top'>");
						writer.write(value);
					} else {
						write("<td valign='top'>");
						writer.write(value.replace("\n", "<br/>"));
					}
					write("</td>");
				}
			}
			writeln("</tr>");
		}
		writeln("</tbody></table>");
	}

	private static boolean isNumber(String text) {
		final int length = text.length();
		for (int i = 0; i < length; i++) {
			final char c = text.charAt(i);
			if (!Character.isDigit(c) && c != '.') {
				return false;
			}
		}
		return true;
	}

	private void writeLinks() throws IOException {
		writeln("<div class='noPrint'>");
		writeln("<a href='javascript:history.back()'><img src='?resource=action_back.png' alt='#Retour#'/> #Retour#</a>");
		final String separator = "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;";
		writeln(separator);
		writeln("<a href='?part=database&amp;request=" + databaseInformations.getRequestIndex()
				+ "'><img src='?resource=action_refresh.png' alt='#Actualiser#'/> #Actualiser#</a>");
		writeln(separator);
		writeln("<select name='request' onchange=\"location.href='?part=database&amp;request='+options.selectedIndex;\">");
		int index = 0;
		for (final String request : databaseInformations.getRequestNames()) {
			write("<option value='");
			write(String.valueOf(index));
			write("'");
			if (index == databaseInformations.getRequestIndex()) {
				write(" selected='selected'");
			}
			write(">");
			write(I18N.getString(request));
			write("</option>");
			index++;
		}
		writeln("</select>");
		writeln("</div>");
	}

	private void write(String html) throws IOException {
		I18N.writeTo(html, writer);
	}

	private void writeln(String html) throws IOException {
		I18N.writelnTo(html, writer);
	}
}
