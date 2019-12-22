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
package net.bull.javamelody.swing;

import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
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
	public Object getTransferData(DataFlavor flavor)
			throws UnsupportedFlavorException, IOException {
		if (flavor.equals(FLAVORS[FILE])) {
			final File tmpFile = get(0);
			if (!tmpFile.exists()) {
				try {
					try (final OutputStream output = new BufferedOutputStream(
							new FileOutputStream(tmpFile))) {
						output.write(data);
					}
				} finally {
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
