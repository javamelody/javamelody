/*
 * Copyright 2008-2017 by Emeric Vernat
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
import java.util.Date;

import net.bull.javamelody.I18NAdapter;
import net.bull.javamelody.swing.table.MBasicTable;

/**
 * Writer CSV localisé.
 *
 * @author Emeric Vernat
 */
public class MCsvLocalWriter extends MCsvWriter {
	/** Caractère de séparation dans les fichiers csv (local). */
	public static final char CSV_LOCAL_SEPARATOR = I18NAdapter.getString("CSV_LOCAL_SEPARATOR")
			.charAt(0);

	/** {@inheritDoc} */
	@Override
	public void print(final MBasicTable table, final OutputStream out) throws IOException {
		writeCsvLocal(table, out, CSV_LOCAL_SEPARATOR);
		// le séparateur des .csv (';' à la française));
	}

	/**
	 * Exporte une JTable dans un fichier au format csv local pour Menu,Ouvrir. (séparateur ';', formats nombres et dates locaux).
	 *
	 * @param table
	 *           MBasicTable
	 * @param outputStream
	 *           OutputStream
	 * @param csvSeparator
	 *           char
	 * @throws IOException
	 *            Erreur disque
	 */
	protected void writeCsvLocal(final MBasicTable table, final OutputStream outputStream,
			final char csvSeparator) throws IOException {
		writeCsvLocal(table, outputStream, csvSeparator, null);
	}

	/**
	 * Exporte une JTable dans un fichier au format csv local pour Menu,Ouvrir. (séparateur ';', formats nombres et dates locaux).
	 *
	 * @param table
	 *           MBasicTable
	 * @param outputStream
	 *           OutputStream
	 * @param csvSeparator
	 *           char
	 * @param dateFormat
	 *           DateFormat
	 * @throws IOException
	 *            Erreur disque
	 */
	protected void writeCsvLocal(final MBasicTable table, final OutputStream outputStream,
			final char csvSeparator, final DateFormat dateFormat) throws IOException {
		// récupération des informations utiles
		final int columnCount = table.getColumnModel().getColumnCount();
		final int rowCount = table.getRowCount();
		// charset local
		final String charset = System.getProperty("file.encoding");
		final Writer out = new OutputStreamWriter(outputStream, charset);

		final String eol = System.getProperty("line.separator");

		// titres des colonnes
		writeCsvHeader(table, out, csvSeparator);

		// les données proprement dites (ligne par ligne puis colonne par colonne)
		Object value;
		String text;
		for (int k = 0; k < rowCount; k++) {
			for (int i = 0; i < columnCount; i++) {
				value = getValueAt(table, k, i);
				if (dateFormat != null && value instanceof Date) {
					text = dateFormat.format(value);
				} else {
					text = getTextAt(table, k, i);
					text = formatCsv(text, csvSeparator);
					if (value instanceof Number) {
						// en France, le caractère de séparation des milliers est pour Java un espace insécable (0xA0)
						// et non un espace standard (0x20), mais Excel XP reconnaît l'espace standard dans les nombres
						// mais ne reconnaît pas l'espace insécable et interprète alors les nombres comme du texte
						text = text.replace('\u00A0', '\u0020');
					}
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
}
