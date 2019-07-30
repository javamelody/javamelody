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

import java.awt.print.PrinterException;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JTable;

import net.bull.javamelody.I18NAdapter;
import net.bull.javamelody.swing.table.MBasicTable;

/**
 * Printer Java.
 *
 * @author Emeric Vernat
 */
public class MJavaPrinter extends MPrinter {
	/**
	 * Icône Imprimante.
	 */
	public static final ImageIcon PRINT_ICON = new ImageIcon(
			MJavaPrinter.class.getResource("/icons/Print16.gif"));

	/** {@inheritDoc} */
	@Override
	public void print(final MBasicTable table, final OutputStream out) {
		// rien ici
	}

	/** {@inheritDoc} */
	@Override
	public File getFile(final JTable table) throws IOException {
		// impression directe, pas de fichier
		// (Remarque : le code pour exporter et / ou imprimer en pdf est disponible, mais cela nécessiterait de déployer la dépendance iText sur le poste client)
		try {
			table.print();
		} catch (final PrinterException e) {
			throw new IOException(e);
		}
		return null;
	}

	/** {@inheritDoc} */
	@Override
	public String getName() {
		return I18NAdapter.getString("imprimer");
	}

	/** {@inheritDoc} */
	@Override
	public String getFileExtension() {
		return null;
	}

	/** {@inheritDoc} */
	@Override
	public Icon getIcon() {
		return PRINT_ICON;
	}
}
