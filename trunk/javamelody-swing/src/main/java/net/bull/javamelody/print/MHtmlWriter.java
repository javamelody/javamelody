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
package net.bull.javamelody.print;

import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.text.DateFormat;
import java.util.Date;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.ListSelectionModel;

import net.bull.javamelody.table.MBasicTable;

/**
 * Writer HTML.
 *
 * @author Emeric Vernat
 */
public class MHtmlWriter extends MPrinter {
	/**
	 * Icône MS IE.
	 */
	public static final ImageIcon MSIE_ICON = new ImageIcon(
			MHtmlWriter.class.getResource("/icons/ms ie.png"));

	/** {@inheritDoc} */
	@Override
	public void print(final MBasicTable table, final OutputStream out) throws IOException {
		writeHtml(table, out, false);
	}

	/** {@inheritDoc} */
	@Override
	public String getName() {
		return "Exporter en HTML";
	}

	/** {@inheritDoc} */
	@Override
	public String getFileExtension() {
		return "html";
	}

	/** {@inheritDoc} */
	@Override
	public Icon getIcon() {
		return MSIE_ICON;
	}

	/**
	 * Encode un texte au format html.
	 *
	 * @return String
	 * @param text
	 *           String
	 */
	protected String formatHtml(final String text) {
		return MHtmlEncoder.encodeString(text);
	}

	/**
	 * Exporte une JTable dans un fichier au format html.
	 *
	 * @param table
	 *           MBasicTable
	 * @param outputStream
	 *           OutputStream
	 * @param isSelection
	 *           boolean
	 * @throws IOException
	 *            Erreur disque
	 */
	protected void writeHtml(final MBasicTable table, final OutputStream outputStream,
			final boolean isSelection) throws IOException {
		final int rowCount = table.getRowCount();
		final int columnCount = table.getColumnCount();
		final Writer out = new OutputStreamWriter(outputStream);

		final String eol = isSelection ? "\n" : System.getProperty("line.separator");
		// eol = "\n" si sélection, "\r\n" sinon pour un fichier windows et "\n" pour un fichier unix

		out.write("<!-- Fichier genere par ");
		out.write(System.getProperty("user.name"));
		out.write(" le ");
		out.write(DateFormat.getDateTimeInstance(DateFormat.LONG, DateFormat.LONG).format(
				new Date()));
		out.write(" -->");
		out.write(eol);

		out.write("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">");
		out.write("<html>");
		out.write(eol);
		out.write("<head>");
		out.write(eol);
		out.write("<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">");
		out.write(eol);
		out.write("<title>");
		final String title = buildTitle(table);
		if (title != null) {
			out.write(title);
		}
		out.write("</title>");
		out.write(eol);
		out.write("<style type=\"text/css\"><!--.smallFont {  font-size: smaller}--></style>");
		out.write(eol);
		out.write("</head>");
		out.write(eol);
		out.write("<body>");
		out.write(eol);
		if (title != null) {
			out.write("<h3>");
			out.write(title);
			out.write("</h3>");
		}
		out.write(eol);

		out.write("<table width=\"100%\" border=\"1\" cellspacing=\"0\" bordercolor=\"#000000\" cellpadding=\"2\">");
		out.write(eol);
		out.write(eol);
		out.write("  <tr align=\"center\" class=\"smallFont\">");
		out.write(eol);

		String text;
		Object value;
		// titres des colonnes
		for (int i = 0; i < columnCount; i++) {
			out.write("    <th id=");
			out.write(String.valueOf(i));
			out.write("> ");
			value = table.getColumnModel().getColumn(i).getHeaderValue();
			text = value != null ? value.toString() : "";
			text = formatHtml(text);
			out.write(text);
			out.write(" </th>");
			out.write(eol);
		}
		out.write("  </tr>");
		out.write(eol);

		// les données proprement dites (ligne par ligne puis colonne par colonne)
		final ListSelectionModel selectionModel = table.getSelectionModel();
		for (int k = 0; k < rowCount; k++) {
			if (isSelection && !selectionModel.isSelectedIndex(k)) {
				continue;
			}

			out.write(eol);
			out.write("  <tr id=");
			out.write(String.valueOf(k));
			out.write(" class=\"smallFont\">");
			out.write(eol);
			for (int i = 0; i < columnCount; i++) {
				value = getValueAt(table, k, i);
				out.write("    <td");
				if (value instanceof Number || value instanceof Date) {
					out.write(" align=\"right\"");
				} else if (value instanceof Boolean) {
					out.write(" align=\"center\"");
				}
				out.write("> ");

				text = getTextAt(table, k, i);
				text = formatHtml(text);
				out.write(text != null && text.trim().length() != 0 ? text : "&nbsp;"); // NOPMD
				out.write(" </td>");
				out.write(eol);
			}
			out.write("  </tr>");
			out.write(eol);
		}

		out.write(eol);
		out.write("</table>");
		out.write(eol);
		out.write("</body>");
		out.write(eol);
		out.write("</html>");
		out.flush();
	}
}
