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

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Timer;

import org.jrobin.core.Archive;
import org.jrobin.core.Robin;
import org.jrobin.core.RrdDb;
import org.jrobin.core.RrdException;

/**
 * Command-line tool to merge files of several data directories into one directory.
 * @author Emeric Vernat
 */
public final class DataMerge {
	private final File storageDirectory;
	private final File mergingDirectory;

	private DataMerge(final File storageDirectory, final File mergingDirectory) {
		this.storageDirectory = storageDirectory;
		this.mergingDirectory = mergingDirectory;
		if (!storageDirectory.exists()) {
			throw new IllegalArgumentException("Directory " + storageDirectory + " does not exist");
		}
		if (!storageDirectory.isDirectory()) {
			throw new IllegalArgumentException(storageDirectory + " is not a directory");
		}

		if (!mergingDirectory.mkdirs() && !mergingDirectory.exists()) {
			throw new IllegalArgumentException(mergingDirectory + " can't be created");
		}
		if (!mergingDirectory.isDirectory()) {
			throw new IllegalArgumentException(mergingDirectory + " is not a directory");
		}
		if (!listFiles(mergingDirectory).isEmpty()) {
			throw new IllegalArgumentException(mergingDirectory + " is not empty");
		}
	}

	/**
	 * main.
	 * @param args String[]
	 * @throws IOException e
	 */
	public static void main(final String[] args) throws IOException {
		if (args == null || args.length != 2) {
			throw new IllegalArgumentException(
					"Please give the javamelody storage directory and the merging directory as arguments");
		}
		final File storageDirectory = new File(args[0]);
		final File mergingDirectory = new File(args[1]);
		final DataMerge dataMerge = new DataMerge(storageDirectory, mergingDirectory);
		dataMerge.mergeData();
	}

	private void mergeData() throws IOException {
		final Timer timer = new Timer("javamelody-datamerge", true);
		try {
			JRobin.initBackendFactory(timer);
			final List<File> directories = new ArrayList<>(listFiles(storageDirectory));
			for (final Iterator<File> it = directories.iterator(); it.hasNext();) {
				if (!it.next().isDirectory()) {
					it.remove();
				}
			}
			if (directories.isEmpty()) {
				throw new IllegalArgumentException(
						"No subdirectories found in " + storageDirectory);
			}
			mergeDirectories(directories);
		} finally {
			timer.cancel();
		}
	}

	private void mergeDirectories(final List<File> directories) throws IOException {
		final long start = System.currentTimeMillis();
		log("Merging " + directories.size() + " subdirectories from " + storageDirectory + " to "
				+ mergingDirectory);
		final List<String> mergedFileNames = new ArrayList<>();
		for (final File directory : directories) {
			final List<File> files = listFiles(directory);
			for (final File file : files) {
				final String fileName = file.getName();
				if (!mergedFileNames.contains(fileName)) {
					mergedFileNames.add(fileName);
					final List<File> filesToMerge = new ArrayList<>();
					for (final File directoryToMerge : directories) {
						final File fileToMerge = new File(directoryToMerge, fileName);
						if (fileToMerge.exists()) {
							filesToMerge.add(fileToMerge);
						}
					}
					assert !filesToMerge.isEmpty();
					final File target = new File(mergingDirectory, fileName);
					if (fileName.endsWith(".rrd")) {
						log("Merging " + fileName);
						if (filesToMerge.size() == 1) {
							Files.copy(filesToMerge.get(0).toPath(),
									new File(mergingDirectory, fileName).toPath());
						} else {
							mergeGraphs(filesToMerge, target);
						}
					} else if (fileName.endsWith(".ser.gz")) {
						log("Merging " + fileName);
						if (filesToMerge.size() == 1) {
							Files.copy(filesToMerge.get(0).toPath(),
									new File(mergingDirectory, fileName).toPath());
						} else {
							mergeStatistics(filesToMerge, target);
						}
					} else {
						log("Ignoring " + fileName);
					}
				}
			}
		}
		final long durationInSeconds = Math.round((System.currentTimeMillis() - start) / 1000D);
		log(mergedFileNames.size() + " files merged from " + storageDirectory + " to "
				+ mergingDirectory + " in " + durationInSeconds + " s");
	}

	private void mergeGraphs(final List<File> sources, final File target) throws IOException {
		try {
			RrdDb mergedRrdDb = null;
			Double coeff = null;
			final String name = target.getName();
			final boolean percentageValues = "cpu.rrd".equals(name) || "gc.rrd".equals(name)
					|| "systemCpuLoad.rrd".equals(name) || name.endsWith("SystemErrors.rrd");
			final File lastSource = sources.get(sources.size() - 1);
			for (final File source : sources) {
				if (mergedRrdDb == null) {
					Files.copy(source.toPath(), target.toPath());
					mergedRrdDb = new RrdDb(target.getPath());
				} else {
					if (percentageValues && source.equals(lastSource)) {
						// cpu and gc should be in percentage between 0 and 100,
						// so we use a coeff to compute 'sum of values / nb of values' for the last RRD when we have the sum
						coeff = 1d / sources.size();
					}
					final RrdDb sourceRrdDb = new RrdDb(source.getPath());
					try {
						mergeRrdDbs(sourceRrdDb, mergedRrdDb, coeff);
					} finally {
						sourceRrdDb.close();
					}
				}
			}
			assert mergedRrdDb != null;
			mergedRrdDb.close();
		} catch (final RrdException e) {
			log(e.toString());
			deleteFile(target);
		}
	}

	private boolean deleteFile(final File target) {
		return target.delete();
	}

	private void mergeRrdDbs(final RrdDb sourceRrdDb, final RrdDb mergedRrdDb, Double coeff)
			throws IOException, RrdException {
		// On suppose que les RRD à merger entre eux ont la même structure
		// (headers, datasources, ordres des archives, heures de dernière mise à jour, etc)
		// puisque normalement ils ont été créés toujours de la même façon.
		// Donc pour l'instant, on se contente de copier les valeurs brutes directement
		assert sourceRrdDb.getArcCount() == mergedRrdDb.getArcCount();
		assert sourceRrdDb.getDsCount() == mergedRrdDb.getDsCount();
		for (int i = 0; i < sourceRrdDb.getArcCount(); i++) {
			final Archive sourceArchive = sourceRrdDb.getArchive(i);
			final Archive mergedArchive = mergedRrdDb.getArchive(i);
			assert sourceArchive.getArcStep() == mergedArchive.getArcStep();
			assert sourceArchive.getConsolFun().equals(mergedArchive.getConsolFun());
			for (int j = 0; j < sourceRrdDb.getDsCount(); j++) {
				final Robin sourceRobin = sourceArchive.getRobin(j);
				final Robin mergedRobin = mergedArchive.getRobin(j);
				final double[] sourceValues = sourceRobin.getValues();
				final double[] mergedValues = mergedRobin.getValues();
				assert sourceValues.length == mergedValues.length;
				for (int k = 0; k < mergedValues.length; k++) {
					mergedValues[k] += sourceValues[k];
					if (coeff != null) {
						mergedValues[k] *= coeff;
					}
				}
				mergedRobin.setValues(mergedValues);
			}
		}
	}

	private void mergeStatistics(final List<File> sources, final File target) throws IOException {
		Counter mergedCounter = null;
		for (final File source : sources) {
			Counter counter = null;
			try {
				counter = CounterStorage.readFromFile(source);
			} catch (final IOException e) {
				log(e.toString() + " for " + source);
				continue;
			}
			if (mergedCounter == null) {
				mergedCounter = counter;
			} else {
				for (final CounterRequest request : counter.getRequests()) {
					mergedCounter.addHits(request);
				}
				if (counter.isErrorCounter()) {
					mergedCounter.addErrors(counter.getErrors());
				}
			}
		}
		assert mergedCounter != null;
		CounterStorage.writeToFile(mergedCounter, target);
	}

	private List<File> listFiles(final File directory) {
		final File[] files = directory.listFiles();
		if (files == null) {
			return Collections.emptyList();
		}
		return Arrays.asList(files);
	}

	private static void log(final String s) {
		System.out.println(s); // NOPMD
	}
}
