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
 * Writer json en utilisant XStream.
 * @author Emeric Vernat
 */
public class MJsonWriter extends MPrinter {
	/** {@inheritDoc} */
	@Override
	public void print(MBasicTable table, OutputStream out) throws IOException {
		// json possible qu'avec MListTable
		if (table instanceof MListTable) {
			writeJson((MListTable<?>) table, out);
		} else {
			throw new IllegalArgumentException();
		}
	}

	/** {@inheritDoc} */
	@Override
	public String getName() {
		return "Exporter en JSON";
	}

	/** {@inheritDoc} */
	@Override
	public String getFileExtension() {
		return "json";
	}

	/** {@inheritDoc} */
	@Override
	public Icon getIcon() {
		return ImageIconCache.getImageIcon("xml.png");
	}

	/**
	 * Exporte une MListTable dans un fichier au format json.
	 * @param table MListTable
	 * @param outputStream OutputStream
	 * @throws IOException   Erreur disque
	 */
	protected void writeJson(MListTable<?> table, OutputStream outputStream) throws IOException {
		final List<?> list = table.getList();
		TransportFormatAdapter.writeJson((Serializable) list, outputStream);
	}
}
