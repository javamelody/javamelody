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
import java.util.Iterator;
import java.util.List;
import java.util.Timer;

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
		if (mergingDirectory.listFiles().length != 0) {
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
			final List<File> directories = new ArrayList<>(
					Arrays.asList(storageDirectory.listFiles()));
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

		final CollectorDataMerge collectorDataMerge = new CollectorDataMerge(directories,
				mergingDirectory) {
			@Override
			protected void log(String msg) {
				DataMerge.log(msg);
			}
		};
		final int mergedFiles = collectorDataMerge.mergeDirectories();

		final long durationInSeconds = Math.round((System.currentTimeMillis() - start) / 1000D);
		log(mergedFiles + " files merged from " + storageDirectory + " to " + mergingDirectory
				+ " in " + durationInSeconds + " s");
	}

	static void log(final String s) {
		System.out.println(s); // NOPMD
	}
}
