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
package net.bull.javamelody.internal.model;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import org.jrobin.core.Archive;
import org.jrobin.core.Robin;
import org.jrobin.core.RrdDb;
import org.jrobin.core.RrdException;

import net.bull.javamelody.internal.common.InputOutput;

/**
 * Merge files of several data directories into one directory.
 * @author Emeric Vernat
 */
abstract class CollectorDataMerge {
	private final List<File> sourceDirectories;
	private final File targetDirectory;

	// tri par dates décroissantes de fichiers
	private static final Comparator<File> FILES_COMPARATOR = new Comparator<File>() {
		@Override
		public int compare(File o1, File o2) {
			return (int) (o2.lastModified() - o1.lastModified());
		}
	};

	CollectorDataMerge(List<File> sourceDirectories, File targetDirectory) {
		super();
		this.sourceDirectories = sourceDirectories;
		this.targetDirectory = targetDirectory;
	}

	protected abstract void log(String msg);

	int mergeDirectories() throws IOException {
		final List<String> mergedFileNames = new ArrayList<String>();
		for (final File directory : sourceDirectories) {
			final List<File> files = listFiles(directory);
			for (final File file : files) {
				final String fileName = file.getName();
				if (!mergedFileNames.contains(fileName)) {
					if (isFileExcluded(fileName)) {
						log("Excluding " + fileName);
					} else {
						mergedFileNames.add(fileName);
						final List<File> filesToMerge = new ArrayList<File>();
						for (final File directoryToMerge : sourceDirectories) {
							final File fileToMerge = new File(directoryToMerge, fileName);
							if (fileToMerge.exists()) {
								filesToMerge.add(fileToMerge);
							}
						}
						assert !filesToMerge.isEmpty();
						final File target = new File(targetDirectory, fileName);
						if (fileName.endsWith(".rrd")) {
							log("Merging " + fileName);
							if (filesToMerge.size() == 1) {
								InputOutput.copyFile(filesToMerge.get(0),
										new File(targetDirectory, fileName));
							} else {
								Collections.sort(filesToMerge, FILES_COMPARATOR);
								mergeGraphs(filesToMerge, target);
							}
						} else if (fileName.endsWith(".ser.gz")) {
							log("Merging " + fileName);
							if (filesToMerge.size() == 1) {
								InputOutput.copyFile(filesToMerge.get(0),
										new File(targetDirectory, fileName));
							} else {
								Collections.sort(filesToMerge, FILES_COMPARATOR);
								mergeStatistics(filesToMerge, target);
							}
						} else {
							log("Ignoring " + fileName);
						}
						if (target.exists()) {
							target.setLastModified(filesToMerge.get(0).lastModified());
						}
					}
				}
			}
		}
		return mergedFileNames.size();
	}

	private boolean isFileExcluded(String fileName) {
		// on exclu les fichiers rrd tels que httpf4562103f5fef56778018769947b2e02609bc1db.rrd
		// car les temps moyens par requête sont moins importants que les autres fichiers et ce serait long à merger
		return fileName.endsWith(".rrd") && fileName.length() > 40;
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
					InputOutput.copyFile(source, target);
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
			InputOutput.deleteFile(target);
		}
	}

	private void mergeRrdDbs(final RrdDb sourceRrdDb, final RrdDb mergedRrdDb, Double coeff)
			throws IOException, RrdException {
		// On suppose que les RRD à merger entre eux ont la même structure
		// (headers, datasources, ordres des archives, etc)
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
				final int deltaSteps = (int) ((mergedArchive.getEndTime()
						- sourceArchive.getEndTime()) / mergedArchive.getArcStep());
				// les fichiers à merger sont triés par dates descendantes
				// donc deltaSteps >= 0 en général et les valeurs sources sont plus vieilles
				// càd que les valeurs à la fin de mergedValues sont plus récentes que les valeurs à la fin de sourceValues
				if (deltaSteps >= 0) {
					for (int k = 0; k < mergedValues.length - deltaSteps; k++) {
						mergedValues[k] = addDoubles(mergedValues[k], sourceValues[k + deltaSteps]);
						if (coeff != null) {
							mergedValues[k] *= coeff;
						}
					}
				} else {
					for (int k = -deltaSteps; k < mergedValues.length; k++) {
						mergedValues[k] = addDoubles(mergedValues[k], sourceValues[k + deltaSteps]);
						if (coeff != null) {
							mergedValues[k] *= coeff;
						}
					}
				}
				mergedRobin.setValues(mergedValues);
			}
		}
	}

	private double addDoubles(double d1, double d2) {
		if (Double.isNaN(d1)) {
			return d2;
		} else if (Double.isNaN(d2)) {
			return d1;
		}
		return d1 + d2;
	}

	private void mergeStatistics(final List<File> sources, final File target) throws IOException {
		Counter mergedCounter = null;
		for (final File source : sources) {
			Counter counter;
			try {
				counter = CounterStorage.readFromFile(source);
			} catch (final IOException e) {
				log(e + " for " + source);
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
}
