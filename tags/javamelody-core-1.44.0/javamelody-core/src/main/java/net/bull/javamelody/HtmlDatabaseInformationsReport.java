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

/**
 * Partie du rapport html pour les informations sur la base de données.
 * @author Emeric Vernat
 */
class HtmlDatabaseInformationsReport extends HtmlAbstractReport {
	private final DatabaseInformations databaseInformations;

	static class TableReport extends HtmlAbstractReport {
		private final String[][] values;
		private final int nbColumns;

		TableReport(String[][] values, int nbColumns, Writer writer) {
			super(writer);
			this.values = values.clone();
			this.nbColumns = nbColumns;
		}

		@Override
		void toHtml() throws IOException {
			final int rowsByColumn;
			if (nbColumns > 1) {
				rowsByColumn = (values.length - 1) / nbColumns + 1;
				writeln("<table width='100%' cellspacing='0' cellpadding='2' summary=''><tr><td valign='top'>");
			} else {
				rowsByColumn = -1;
			}
			if (values.length <= 1) {
				return;
			}
			final String[] headerValues = values[0];
			HtmlTable table = new HtmlTable();
			table.beginTable(getString("database"));
			writeTableHeaders(headerValues);
			int index = 0;
			for (final String[] row : values) {
				if (index == 0) {
					index++;
					continue;
				}
				table.nextRow();
				writeRow(row);
				index++;
				if (rowsByColumn > 0 && (index - 1) % rowsByColumn == 0 && index != values.length) {
					table.endTable();
					writeln("</td><td valign='top'>");
					table = new HtmlTable();
					table.beginTable(getString("database"));
					writeTableHeaders(headerValues);
				}
			}
			table.endTable();
			if (nbColumns > 1) {
				writeln("</td></tr></table>");
			}
		}

		private void writeTableHeaders(String[] headerValues) throws IOException {
			for (final String value : headerValues) {
				write("<th>");
				writeDirectly(value.replace("\n", "<br/>"));
				write("</th>");
			}
		}

		private void writeRow(String[] row) throws IOException {
			for (final String value : row) {
				if (value == null || value.length() == 0) {
					write("<td>&nbsp;</td>");
				} else {
					if (isNumber(value)) {
						write("<td align='right' valign='top'>");
						writeDirectly(value);
					} else {
						write("<td valign='top'>");
						writeDirectly(value.replace("\n", "<br/>"));
					}
					write("</td>");
				}
			}
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
	}

	HtmlDatabaseInformationsReport(DatabaseInformations databaseInformations, Writer writer) {
		super(writer);
		assert databaseInformations != null;
		this.databaseInformations = databaseInformations;
	}

	@Override
	void toHtml() throws IOException {
		writeLinks();
		writeln("<br/>");

		final String title = getString("database") + " : "
				+ getString(databaseInformations.getSelectedRequestName());
		writeTitle("db.png", title);

		final String[][] values = databaseInformations.getResult();
		final int nbColumns = databaseInformations.getNbColumns();
		new TableReport(values, nbColumns, getWriter()).toHtml();
	}

	private void writeLinks() throws IOException {
		writeln("<div class='noPrint'>");
		writeln("<a href='javascript:history.back()'><img src='?resource=action_back.png' alt='#Retour#'/> #Retour#</a>");
		final String separator = "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;";
		writeln(separator);
		writeln("<a href='?part=database&amp;request="
				+ databaseInformations.getSelectedRequestIndex()
				+ "'><img src='?resource=action_refresh.png' alt='#Actualiser#'/> #Actualiser#</a>");
		if (isPdfEnabled()) {
			writeln(separator);
			write("<a href='?part=database&amp;request="
					+ databaseInformations.getSelectedRequestIndex()
					+ "&amp;format=pdf' title='#afficher_PDF#'>");
			write("<img src='?resource=pdf.png' alt='#PDF#'/> #PDF#</a>");
		}
		writeln(separator);
		writeln("<select name='request' onchange=\"location.href='?part=database&amp;request='+options.selectedIndex;\">");
		int index = 0;
		for (final String request : databaseInformations.getRequestNames()) {
			write("<option value='");
			write(String.valueOf(index));
			write("'");
			if (index == databaseInformations.getSelectedRequestIndex()) {
				write(" selected='selected'");
			}
			write(">");
			write(getString(request));
			write("</option>");
			index++;
		}
		writeln("</select>");
		writeln("</div>");
	}
}
