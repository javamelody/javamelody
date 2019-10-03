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
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Serializable;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;

import net.bull.javamelody.internal.common.LOG;
import net.bull.javamelody.internal.common.Parameters;

/**
 * Records the versions of the webapp.
 * @author Emeric Vernat
 */
class WebappVersions {
	private static final Comparator<Map.Entry<String, Date>> WEBAPP_VERSIONS_VALUE_COMPARATOR = Collections
			.reverseOrder(new MapValueComparator<String, Date>());
	private static final String VERSIONS_FILENAME = "versions.properties";
	private static final String VERSIONS_DATE_PATTERN = "yyyy/MM/dd";

	/**
	 * Les versions de l'applications avec pour chacune la date de déploiement.
	 */
	private final Map<String, Date> datesByVersions;
	private final File versionsFile;

	private static class MapValueComparator<K, V extends Comparable<V>>
			implements Comparator<Map.Entry<K, V>>, Serializable {
		private static final long serialVersionUID = 1L;

		MapValueComparator() {
			super();
		}

		@Override
		public int compare(Map.Entry<K, V> o1, Map.Entry<K, V> o2) {
			return o1.getValue().compareTo(o2.getValue());
		}
	}

	WebappVersions(String application) {
		super();
		assert application != null;
		final File storageDirectory = Parameters.getStorageDirectory(application);
		this.versionsFile = new File(storageDirectory, VERSIONS_FILENAME);
		this.datesByVersions = readDatesByVersions();
	}

	@SuppressWarnings("unchecked")
	private Map<String, Date> readDatesByVersions() {
		final Map<String, Date> result = new HashMap<String, Date>();
		if (versionsFile.exists()) {
			final Properties versionsProperties = new Properties();
			try {
				final InputStream input = new FileInputStream(versionsFile);
				try {
					versionsProperties.load(input);
				} finally {
					input.close();
				}
				final List<String> propertyNames = (List<String>) Collections
						.list(versionsProperties.propertyNames());
				final SimpleDateFormat dateFormat = new SimpleDateFormat(VERSIONS_DATE_PATTERN,
						Locale.US);
				for (final String version : propertyNames) {
					try {
						final Date date = dateFormat.parse(versionsProperties.getProperty(version));
						result.put(version, date);
					} catch (final ParseException e) {
						continue;
					}
				}
			} catch (final IOException e) {
				// lecture échouée, tant pis
				LOG.warn("exception while reading versions in " + versionsFile, e);
			}
		}
		return result;
	}

	Map<String, Date> getDatesByVersions() {
		final List<Map.Entry<String, Date>> entries = new ArrayList<Map.Entry<String, Date>>(
				datesByVersions.entrySet());
		Collections.sort(entries, WEBAPP_VERSIONS_VALUE_COMPARATOR);
		final Map<String, Date> map = new LinkedHashMap<String, Date>();
		for (final Map.Entry<String, Date> entry : entries) {
			map.put(entry.getKey(), entry.getValue());
		}
		return Collections.unmodifiableMap(map);
	}

	void addVersionIfNeeded(String webappVersion) throws IOException {
		if (webappVersion == null || datesByVersions.containsKey(webappVersion)) {
			return;
		}

		final Properties versionsProperties = new Properties();
		if (versionsFile.exists()) {
			final InputStream input = new FileInputStream(versionsFile);
			try {
				versionsProperties.load(input);
			} finally {
				input.close();
			}
		}
		assert versionsProperties.getProperty(webappVersion) == null;

		final SimpleDateFormat dateFormat = new SimpleDateFormat(VERSIONS_DATE_PATTERN, Locale.US);
		versionsProperties.setProperty(webappVersion, dateFormat.format(new Date()));

		final File directory = versionsFile.getParentFile();
		if (!directory.mkdirs() && !directory.exists()) {
			throw new IOException("JavaMelody directory can't be created: " + directory.getPath());
		}
		final OutputStream output = new FileOutputStream(versionsFile);
		try {
			versionsProperties.store(output, "Application deployments with versions and dates");
		} finally {
			output.close();
		}
		datesByVersions.put(webappVersion, new Date());
		LOG.debug("New application version added: " + webappVersion);
	}
}
