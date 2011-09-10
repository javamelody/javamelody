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
package net.bull.javamelody.swing.print;

import java.awt.print.PrinterException;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JTable;

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
		return "Imprimer";
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
