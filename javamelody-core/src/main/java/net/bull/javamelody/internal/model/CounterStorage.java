/*
 * Copyright 2008-2017 by Emeric Vernat
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
package net.bull.javamelody.internal.model;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.List;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;

import net.bull.javamelody.Parameter;
import net.bull.javamelody.internal.common.Parameters;

/**
 * Classe chargée de l'enregistrement et de la lecture d'un {@link Counter}.
 * @author Emeric Vernat
 */
public class CounterStorage {
	private static final int DEFAULT_OBSOLETE_STATS_DAYS = 365;
	private static boolean storageDisabled;
	private final Counter counter;

	// do not user CounterResponseStream to not depend on the net.bull.internal.web package
	private static class CounterOutputStream extends OutputStream {
		int dataLength;
		private final OutputStream output;

		CounterOutputStream(OutputStream output) {
			super();
			this.output = output;
		}

		@Override
		public void write(int b) throws IOException {
			output.write(b);
			dataLength++;
		}

		@Override
		public void write(byte[] b) throws IOException {
			output.write(b);
			dataLength += b.length;
		}

		@Override
		public void write(byte[] b, int off, int len) throws IOException {
			output.write(b, off, len);
			dataLength += len;
		}

		@Override
		public void flush() throws IOException {
			output.flush();
		}

		@Override
		public void close() throws IOException {
			output.close();
		}
	}

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
		if (storageDisabled) {
			return -1;
		}
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
		return writeToFile(counter, file);
	}

	static int writeToFile(Counter counter, File file) throws IOException {
		final FileOutputStream out = new FileOutputStream(file);
		try {
			final CounterOutputStream counterOutput = new CounterOutputStream(
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
			return counterOutput.dataLength;
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
		if (storageDisabled) {
			return null;
		}
		final File file = getFile();
		if (file.exists()) {
			return readFromFile(file);
		}
		// ou on retourne null si le fichier n'existe pas
		return null;
	}

	static Counter readFromFile(File file) throws IOException {
		final FileInputStream in = new FileInputStream(file);
		try {
			final ObjectInputStream input = TransportFormat
					.createObjectInputStream(new GZIPInputStream(new BufferedInputStream(in)));
			try {
				// on retourne l'instance du counter lue
				return (Counter) input.readObject();
			} finally {
				// ce close libère les ressources du ObjectInputStream et du GZIPInputStream
				input.close();
			}
		} catch (final ClassNotFoundException e) {
			throw new IOException(e.getMessage(), e);
		} finally {
			in.close();
		}
	}

	private File getFile() {
		final File storageDirectory = Parameters.getStorageDirectory(counter.getApplication());
		return new File(storageDirectory, counter.getStorageName() + ".ser.gz");
	}

	static long deleteObsoleteCounterFiles(String application) {
		final Calendar nowMinusOneYearAndADay = Calendar.getInstance();
		nowMinusOneYearAndADay.add(Calendar.DAY_OF_YEAR, -getObsoleteStatsDays());
		nowMinusOneYearAndADay.add(Calendar.DAY_OF_YEAR, -1);
		// filtre pour ne garder que les fichiers d'extension .ser.gz et pour éviter d'instancier des File inutiles
		long diskUsage = 0;
		for (final File file : listSerGzFiles(application)) {
			boolean deleted = false;
			if (file.lastModified() < nowMinusOneYearAndADay.getTimeInMillis()) {
				deleted = file.delete();
			}
			if (!deleted) {
				diskUsage += file.length();
			}
		}

		// on retourne true si tous les fichiers .ser.gz obsolètes ont été supprimés, false sinon
		return diskUsage;
	}

	/**
	 * @return Nombre de jours avant qu'un fichier de statistiques (extension .ser.gz),
	 * soit considéré comme obsolète et soit supprimé automatiquement, à minuit (365 par défaut, soit 1 an)
	 */
	private static int getObsoleteStatsDays() {
		final String param = Parameter.OBSOLETE_STATS_DAYS.getValue();
		if (param != null) {
			// lance une NumberFormatException si ce n'est pas un nombre
			final int result = Integer.parseInt(param);
			if (result <= 0) {
				throw new IllegalStateException(
						"The parameter obsolete-stats-days should be > 0 (365 recommended)");
			}
			return result;
		}
		return DEFAULT_OBSOLETE_STATS_DAYS;
	}

	private static List<File> listSerGzFiles(String application) {
		final File storageDir = Parameters.getStorageDirectory(application);
		// filtre pour ne garder que les fichiers d'extension .rrd et pour éviter d'instancier des File inutiles
		final FilenameFilter filenameFilter = new FilenameFilter() {
			/** {@inheritDoc} */
			@Override
			public boolean accept(File dir, String fileName) {
				return fileName.endsWith(".ser.gz");
			}
		};
		final File[] files = storageDir.listFiles(filenameFilter);
		if (files == null) {
			return Collections.emptyList();
		}
		return Arrays.asList(files);
	}

	// cette méthode est utilisée dans l'ihm Swing
	public static void disableStorage() {
		storageDisabled = true;
	}
}
