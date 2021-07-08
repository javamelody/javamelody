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
package net.bull.javamelody.swing.print;

import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.text.DateFormat;
import java.util.Date;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.ListSelectionModel;

import net.bull.javamelody.I18NAdapter;
import net.bull.javamelody.swing.table.MBasicTable;

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
		return I18NAdapter.getString("export_html");
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
		final Writer out = new OutputStreamWriter(outputStream, StandardCharsets.UTF_8);

		final String eol = isSelection ? "\n" : System.lineSeparator();
		// eol = "\n" si sélection, "\r\n" sinon pour un fichier windows et "\n" pour un fichier unix

		out.write("<!-- Fichier genere par ");
		out.write(System.getProperty("user.name"));
		out.write(" le ");
		out.write(DateFormat.getDateTimeInstance(DateFormat.LONG, DateFormat.LONG)
				.format(new Date()));
		out.write(" -->");
		out.write(eol);

		out.write("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">");
		out.write("<html>");
		out.write(eol);
		final String title = buildTitle(table);
		writeHtmlHead(title, out, eol);
		out.write("<body>");
		out.write(eol);
		if (title != null) {
			out.write("<h3>");
			out.write(title);
			out.write("</h3>");
		}
		out.write(eol);

		writeHtmlTable(table, isSelection, out, eol);
		out.write("</body>");
		out.write(eol);
		out.write("</html>");
		out.flush();
	}

	private void writeHtmlTable(final MBasicTable table, final boolean isSelection,
			final Writer out, final String eol) throws IOException {
		out.write(
				"<table width=\"100%\" border=\"1\" cellspacing=\"0\" bordercolor=\"#000000\" cellpadding=\"2\">");
		out.write(eol);
		out.write(eol);
		out.write("  <tr align=\"center\" class=\"smallFont\">");
		out.write(eol);

		final int rowCount = table.getRowCount();
		final int columnCount = table.getColumnCount();
		// titres des colonnes
		for (int i = 0; i < columnCount; i++) {
			out.write("    <th id=");
			out.write(String.valueOf(i));
			out.write("> ");
			final Object value = table.getColumnModel().getColumn(i).getHeaderValue();
			String text = value != null ? value.toString() : "";
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
				writeHtmlTd(table, out, eol, k, i);
			}
			out.write("  </tr>");
			out.write(eol);
		}

		out.write(eol);
		out.write("</table>");
		out.write(eol);
	}

	private void writeHtmlTd(final MBasicTable table, final Writer out, final String eol, int k,
			int i) throws IOException {
		final Object value = getValueAt(table, k, i);
		out.write("    <td");
		if (value instanceof Number || value instanceof Date) {
			out.write(" align=\"right\"");
		} else if (value instanceof Boolean) {
			out.write(" align=\"center\"");
		}
		out.write("> ");

		String text = getTextAt(table, k, i);
		text = formatHtml(text);
		out.write(text != null && text.trim().length() != 0 ? text : "&nbsp;"); // NOPMD
		out.write(" </td>");
		out.write(eol);
	}

	private void writeHtmlHead(final String title, final Writer out, final String eol)
			throws IOException {
		out.write("<head>");
		out.write(eol);
		out.write("<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">");
		out.write(eol);
		out.write("<title>");
		if (title != null) {
			out.write(title);
		}
		out.write("</title>");
		out.write(eol);
		out.write("<style type=\"text/css\"><!--.smallFont {  font-size: smaller}--></style>");
		out.write(eol);
		out.write("</head>");
		out.write(eol);
	}
}
