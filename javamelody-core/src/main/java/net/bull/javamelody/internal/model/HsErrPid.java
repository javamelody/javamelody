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
import java.io.FilenameFilter;
import java.io.Serializable;
import java.lang.management.ManagementFactory;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;

/**
 * HsErrPid files.
 * @author Emeric Vernat
 */
public final class HsErrPid implements Serializable {
	private static final String XX_ERROR_FILE = "-XX:ErrorFile=";

	private static final long serialVersionUID = 1L;

	private static final FilenameFilter FILENAME_FILTER = new FilenameFilter() {
		@Override
		public boolean accept(File dir, String name) {
			return name.startsWith("hs_err_pid") && name.endsWith(".log");
		}
	};

	private final String file;

	private final Date date;

	/**
	 * Comparateur de HsErrPid par date.
	 */
	static final class HsErrPidComparator implements Comparator<HsErrPid>, Serializable {
		private static final long serialVersionUID = 1L;

		/** {@inheritDoc} */
		@Override
		public int compare(HsErrPid hsErrPid1, HsErrPid hsErrPid2) {
			return hsErrPid2.getDate().compareTo(hsErrPid1.getDate());
		}
	}

	private HsErrPid(String file, Date date) {
		super();
		this.file = file;
		this.date = date;
	}

	public static List<HsErrPid> buildHsErrPidList() {
		final List<File> directories = new ArrayList<File>();
		// locations of fatal error log:
		// http://www.oracle.com/technetwork/java/javase/felog-138657.html
		directories.add(new File("./"));
		// linux:
		directories.add(new File("/tmp"));
		// windows:
		final String tmp = System.getenv("TMP");
		if (tmp != null) {
			directories.add(new File(tmp));
		}
		final List<String> args = ManagementFactory.getRuntimeMXBean().getInputArguments();
		for (final String arg : args) {
			if (arg.startsWith(XX_ERROR_FILE)) {
				final String errorFile = arg.substring(XX_ERROR_FILE.length());
				final File dir = new File(errorFile).getParentFile();
				if (dir != null) {
					directories.add(dir);
				}
			}
		}

		final List<HsErrPid> result = new ArrayList<HsErrPid>();
		for (final File dir : directories) {
			final File[] files = dir.listFiles(FILENAME_FILTER);
			if (files != null) {
				for (final File file : files) {
					result.add(new HsErrPid(file.getAbsolutePath(), new Date(file.lastModified())));
				}
			}
		}
		return result;
	}

	public static List<HsErrPid> getHsErrPidList(List<JavaInformations> javaInformationsList) {
		final List<HsErrPid> result = new ArrayList<HsErrPid>();
		for (final JavaInformations javaInformations : javaInformationsList) {
			final List<HsErrPid> hsErrPidList = javaInformations.getHsErrPidList();
			if (hsErrPidList != null) {
				result.addAll(hsErrPidList);
			}
		}
		Collections.sort(result, new HsErrPidComparator());
		return result;
	}

	public String getFile() {
		return file;
	}

	public Date getDate() {
		return date;
	}
}
