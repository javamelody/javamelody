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
			final String charset = System.getProperty("file.encoding");
			final StringSelection contents = new StringSelection(byteArrayOut.toString(charset));
			clipboard.setContents(contents, contents);
		} catch (final IOException e) {
			MSwingUtilities.showException(e);
		}
	}
}
