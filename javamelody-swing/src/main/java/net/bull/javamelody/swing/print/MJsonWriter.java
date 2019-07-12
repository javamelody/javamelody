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
import java.io.Serializable;
import java.util.List;

import javax.swing.Icon;

import net.bull.javamelody.I18NAdapter;
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
		return I18NAdapter.getString("export_json");
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
