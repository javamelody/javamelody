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
package net.bull.javamelody;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;

/**
 * Classe chargée de l'enregistrement et de la lecture d'un counter.
 * @author Emeric Vernat
 */
class CounterStorage {
	private final Counter counter;

	/**
	 * Constructeur.
	 * @param counter Counter
	 */
	CounterStorage(Counter counter) {
		super();
		assert counter != null;
		this.counter = counter;
	}

	/**
	 * Enregistre le counter.
	 * @return Taille sérialisée non compressée du counter (estimation pessimiste de l'occupation mémoire)
	 * @throws IOException Exception d'entrée/sortie
	 */
	int writeToFile() throws IOException {
		final File file = getFile();
		if (counter.getRequestsCount() == 0 && counter.getErrorsCount() == 0 && !file.exists()) {
			// s'il n'y a pas de requête, inutile d'écrire des fichiers de compteurs vides
			// (par exemple pour le compteur ejb s'il n'y a pas d'ejb)
			return -1;
		}
		final File directory = file.getParentFile();
		if (!directory.mkdirs() && !directory.exists()) {
			throw new IOException("JavaMelody directory can't be created: " + directory.getPath());
		}
		final FileOutputStream out = new FileOutputStream(file);
		try {
			final CounterResponseStream counterOutput = new CounterResponseStream(
					new GZIPOutputStream(new BufferedOutputStream(out)));
			final ObjectOutputStream output = new ObjectOutputStream(counterOutput);
			try {
				output.writeObject(counter);
			} finally {
				// ce close libère les ressources du ObjectOutputStream et du GZIPOutputStream
				output.close();
			}
			// retourne la taille sérialisée non compressée,
			// qui est une estimation pessimiste de l'occupation mémoire
			return counterOutput.getDataLength();
		} finally {
			out.close();
		}
	}

	/**
	 * Lecture du counter depuis son fichier et retour du résultat.
	 * @return Counter
	 * @throws IOException e
	 */
	Counter readFromFile() throws IOException {
		final File file = getFile();
		if (file.exists()) {
			final FileInputStream in = new FileInputStream(file);
			try {
				final ObjectInputStream input = new ObjectInputStream(new GZIPInputStream(
						new BufferedInputStream(in)));
				try {
					// on retourne l'instance du counter lue
					return (Counter) input.readObject();
				} finally {
					// ce close libère les ressources du ObjectInputStream et du GZIPInputStream
					input.close();
				}
			} catch (final ClassNotFoundException e) {
				throw createIOException(e);
			} finally {
				in.close();
			}
		}
		// ou on retourne null si le fichier n'existe pas
		return null;
	}

	private File getFile() {
		final File storageDirectory = Parameters.getStorageDirectory(counter.getApplication());
		return new File(storageDirectory, counter.getStorageName() + ".ser.gz");
	}

	private static IOException createIOException(Exception e) {
		// Rq: le constructeur de IOException avec message et cause n'existe qu'en jdk 1.6
		final IOException ex = new IOException(e.getMessage());
		ex.initCause(e);
		return ex;
	}
}
