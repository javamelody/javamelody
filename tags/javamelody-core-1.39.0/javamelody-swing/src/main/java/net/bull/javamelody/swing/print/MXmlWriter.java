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

import java.io.IOException;
import java.io.OutputStream;
import java.io.Serializable;
import java.util.List;

import javax.swing.Icon;

import net.bull.javamelody.ImageIconCache;
import net.bull.javamelody.TransportFormatAdapter;
import net.bull.javamelody.swing.table.MBasicTable;
import net.bull.javamelody.swing.table.MListTable;

/**
 * Writer xml en utilisant XStream.
 * @author Emeric Vernat
 */
public class MXmlWriter extends MPrinter {
	/** {@inheritDoc} */
	@Override
	public void print(MBasicTable table, OutputStream out) throws IOException {
		// xml possible qu'avec MTable
		if (table instanceof MListTable) {
			writeXml((MListTable<?>) table, out);
		} else {
			throw new IllegalArgumentException();
		}
	}

	/** {@inheritDoc} */
	@Override
	public String getName() {
		return "Exporter en XML";
	}

	/** {@inheritDoc} */
	@Override
	public String getFileExtension() {
		return "xml";
	}

	/** {@inheritDoc} */
	@Override
	public Icon getIcon() {
		return ImageIconCache.getImageIcon("xml.png");
	}

	/**
	 * Exporte une MListTable dans un fichier au format xml.
	 * @param table MListTable
	 * @param outputStream OutputStream
	 * @throws IOException   Erreur disque
	 */
	protected void writeXml(MListTable<?> table, OutputStream outputStream) throws IOException {
		final List<?> list = table.getList();
		TransportFormatAdapter.writeXml((Serializable) list, outputStream);
	}
}
