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

import net.bull.javamelody.table.MBasicTable;

/**
 * Writer CSV localisé.
 *
 * @author Emeric Vernat
 */
public class MCsvLocalWriter extends MCsvWriter {
	/** Caractère de séparation dans les fichiers csv (local). */
	public static final char CSV_LOCAL_SEPARATOR = ';';

	/** {@inheritDoc} */
	@Override
	public void print(final MBasicTable table, final OutputStream out) throws IOException {
		writeCsvLocal(table, out, CSV_LOCAL_SEPARATOR);
		// le séparateur des .csv (';' à la française));
	}

	/** {@inheritDoc} */
	@Override
	public String getName() {
		return "Exporter en CSV";
	}

	/** {@inheritDoc} */
	@Override
	public Icon getIcon() {
		return MSEXCEL_ICON;
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
		final Writer out = new OutputStreamWriter(outputStream);

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
					if (value instanceof Number && text != null) {
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
