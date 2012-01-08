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
package net.bull.javamelody.swing.print;

import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.StringSelection;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;

import javax.swing.JTable;

import net.bull.javamelody.swing.table.MBasicTable;
import net.bull.javamelody.swing.util.MSwingUtilities;

/**
 * Printer de table copiant les données dans le presse-papiers.
 *
 * @author Emeric Vernat
 */
public class MClipboardPrinter extends MHtmlWriter {
	/** {@inheritDoc} */
	@Override
	public void print(final MBasicTable table, final OutputStream out) {
		copySelectionToClipboard(table);
	}

	/** {@inheritDoc} */
	@Override
	public File getFile(final JTable table) {
		return null; // pas de fichier
	}

	/** {@inheritDoc} */
	@Override
	public String getName() {
		return "Clipboard";
	}

	/**
	 * Copie la sélection d'une table dans le presse-papiers (au format html pour Excel par exemple).
	 *
	 * @param table
	 *           MBasicTable
	 */
	protected void copySelectionToClipboard(final MBasicTable table) {
		if (table.getSelectionModel().isSelectionEmpty()) {
			return;
		}

		final Toolkit toolkit = table.getToolkit();
		final Clipboard clipboard = toolkit.getSystemClipboard();
		final ByteArrayOutputStream byteArrayOut = new ByteArrayOutputStream(2048);
		try {
			writeHtml(table, byteArrayOut, true);
			final StringSelection contents = new StringSelection(byteArrayOut.toString());
			clipboard.setContents(contents, contents);
		} catch (final IOException e) {
			MSwingUtilities.showException(e);
		}
	}
}
