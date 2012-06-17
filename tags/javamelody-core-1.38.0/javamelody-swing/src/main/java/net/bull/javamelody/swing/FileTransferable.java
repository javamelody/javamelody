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
package net.bull.javamelody.swing;

import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;

/**
 * Implémentation de Transferable pour le drag and drop de fichiers.
 * @author Emeric Vernat
 */
class FileTransferable extends ArrayList<File> implements Transferable {
	private static final long serialVersionUID = 1L;
	private static final int FILE = 0;
	private static final int STRING = 1;
	private static final DataFlavor[] FLAVORS = { DataFlavor.javaFileListFlavor,
			DataFlavor.stringFlavor, };
	private final byte[] data;

	/**
	 * Constructeur.
	 * @param fileName String
	 * @param data byte[]
	 */
	FileTransferable(String fileName, byte[] data) {
		super();
		// instancie le fichier temporaire dans le répertoire temporaire,
		// il doit avoir le bon nom et il sera vraiment écrit au besoin dans getTransferData
		final File tmpFile = new File(System.getProperty("java.io.tmpdir"), fileName);
		// supprime un fichier de même nom éventuellement présent avant
		delete(tmpFile);
		add(tmpFile);
		this.data = data.clone();
	}

	private boolean delete(File tmpFile) {
		return tmpFile.delete();
	}

	/** {@inheritDoc} */
	@Override
	public DataFlavor[] getTransferDataFlavors() {
		return FLAVORS.clone();
	}

	/** {@inheritDoc} */
	@Override
	public boolean isDataFlavorSupported(DataFlavor flavor) {
		return flavor.equals(FLAVORS[FILE]) || flavor.equals(FLAVORS[STRING]);
	}

	/** {@inheritDoc} */
	@Override
	public Object getTransferData(DataFlavor flavor) throws UnsupportedFlavorException, IOException {
		if (flavor.equals(FLAVORS[FILE])) {
			final File tmpFile = get(0);
			if (!tmpFile.exists()) {
				final FileOutputStream output = new FileOutputStream(tmpFile);
				try {
					output.write(data);
				} finally {
					output.close();

					// prévoit de supprimer le fichier temporaire à la fermeture de
					// l'application puisqu'on ne peut le faire dans exportDone
					tmpFile.deleteOnExit();
				}
			}
			return this;
		} else if (flavor.equals(FLAVORS[STRING])) {
			return get(0).getPath();
		} else {
			throw new UnsupportedFlavorException(flavor);
		}
	}
}
