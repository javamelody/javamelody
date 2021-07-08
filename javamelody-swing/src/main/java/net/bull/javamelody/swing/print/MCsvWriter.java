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
import java.text.DateFormat;
import java.text.NumberFormat;
import java.util.Date;
import java.util.Locale;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JTable;

import net.bull.javamelody.I18NAdapter;
import net.bull.javamelody.swing.table.MBasicTable;

/**
 * Writer CSV non localisé.
 *
 * @author Emeric Vernat
 */
public class MCsvWriter extends MPrinter {
	/** Caractère de séparation dans les fichiers csv (non local). */
	public static final char CSV_SEPARATOR = ',';

	/**
	 * Icône Excel.
	 */
	public static final ImageIcon MSEXCEL_ICON = new ImageIcon(
			MCsvWriter.class.getResource("/icons/ms excel.png"));

	/** {@inheritDoc} */
	@Override
	public void print(final MBasicTable table, final OutputStream out) throws IOException {
		writeCsv(table, out);
	}

	/** {@inheritDoc} */
	@Override
	public String getName() {
		return I18NAdapter.getString("export_csv");
	}

	/** {@inheritDoc} */
	@Override
	public String getFileExtension() {
		return "csv";
	}

	/** {@inheritDoc} */
	@Override
	public Icon getIcon() {
		return MSEXCEL_ICON;
	}

	/**
	 * Encode un texte pour l'export au format csv ou csv local.
	 *
	 * @return String
	 * @param text
	 *           String
	 * @param csvSeparator
	 *           char
	 */
	protected String formatCsv(final String text, final char csvSeparator) {
		String result = text;
		if (result.indexOf('\n') != -1) {
			// le retour chariot fonctionne dans les dernières versions d'excel entre des doubles quotes
			// mais il ne fonctionne pas dans OpenOffice 1.1.2
			result = result.replace('\n', ' ');
		}
		int index = result.indexOf('"');
		while (index != -1) {
			// on double les double-quote pour csv (non performant mais rare)
			result = new StringBuilder(result).insert(index, '"').toString();
			index = result.indexOf('"', index + 2);
		}
		if (text.indexOf(csvSeparator) != -1 || text.indexOf('"') != -1) {
			final String tmp = '"' + result + '"';
			result = tmp;
		}
		return result;
	}

	/**
	 * Exporte une JTable dans un fichier au format csv pour double-clique. (séparateur ',', formats nombres et dates US).
	 *
	 * @param table
	 *           MBasicTable
	 * @param outputStream
	 *           OutputStream
	 * @throws IOException
	 *            Erreur disque
	 */
	protected void writeCsv(final MBasicTable table, final OutputStream outputStream)
			throws IOException {
		writeCsv(table, outputStream, DateFormat.getDateInstance(DateFormat.SHORT, Locale.US));
	}

	/**
	 * Exporte une JTable dans un fichier au format csv pour double-clique. (séparateur ',', formats nombres et dates US).
	 *
	 * @param table
	 *           MBasicTable
	 * @param outputStream
	 *           OutputStream
	 * @param dateFormat
	 *           DateFormat
	 * @throws IOException
	 *            Erreur disque
	 */
	protected void writeCsv(final MBasicTable table, final OutputStream outputStream,
			final DateFormat dateFormat) throws IOException {
		// récupération des informations utiles
		final int columnCount = table.getColumnModel().getColumnCount();
		final int rowCount = table.getRowCount();
		final String charset = System.getProperty("file.encoding");
		final Writer out = new OutputStreamWriter(outputStream, charset);

		final char csvSeparator = CSV_SEPARATOR;
		// le séparateur des .csv (',' à l'américaine)
		final NumberFormat decimalFormat = NumberFormat.getInstance(Locale.US);
		decimalFormat.setGroupingUsed(false);
		final String eol = System.lineSeparator();

		// titres des colonnes
		writeCsvHeader(table, out, csvSeparator);

		// les données proprement dites (ligne par ligne puis colonne par colonne)
		Object value;
		String text;
		for (int k = 0; k < rowCount; k++) {
			for (int i = 0; i < columnCount; i++) {
				value = getValueAt(table, k, i);

				if (value instanceof Number) {
					text = decimalFormat.format(value);
				} else if (value instanceof Date) {
					text = dateFormat.format(value);
				} else if (value instanceof Boolean) {
					text = value.toString();
				} else {
					text = getTextAt(table, k, i);
					text = formatCsv(text, csvSeparator);
				}

				out.write(text);
				if (i < columnCount - 1) {
					out.write(csvSeparator);
				}
			}
			if (k < rowCount - 1) {
				out.write(eol);
			}
		}
		out.flush();
	}

	/**
	 * Exporte les headers csv d'une JTable..
	 *
	 * @param table
	 *           JTable
	 * @param out
	 *           Writer
	 * @param csvSeparator
	 *           char
	 * @throws IOException
	 *            Erreur disque
	 */
	protected void writeCsvHeader(final JTable table, final Writer out, final char csvSeparator)
			throws IOException {
		// récupération des informations utiles
		final int columnCount = table.getColumnModel().getColumnCount();

		final String eol = System.lineSeparator();
		if (table.getName() != null) {
			String title = formatCsv(table.getName(), csvSeparator);
			if (title.startsWith("ID")) {
				final String tmp = '"' + title + '"';
				title = tmp;
			}
			out.write(title);
			out.write(eol);
		}
		// titres des colonnes
		String text;
		for (int i = 0; i < columnCount; i++) {
			text = String.valueOf(table.getColumnModel().getColumn(i).getHeaderValue());
			text = formatCsv(text, csvSeparator);
			if (i == 0 && text.startsWith("ID")) {
				out.write('"' + text + '"'); // si commence par ID, Excel pense que c'est un sylk !
			} else {
				out.write(text);
			}
			if (i < columnCount - 1) {
				out.write(csvSeparator);
			}
		}
		out.write(eol);
	}
}
