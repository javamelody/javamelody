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
package net.bull.javamelody;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

/**
 * Lecture d'artifacts Maven.
 * @author Emeric Vernat
 */
final class MavenArtifact {
	private static final String MAVEN_CENTRAL = "http://repo1.maven.org/maven2";

	private static final File LOCAL_REPO = new File(
			System.getProperty("user.home") + "/.m2/repository");

	private MavenArtifact() {
		super();
	}

	static InputStream getWebappPomXmlAsStream() {
		final Set<?> mavenDir = Parameters.getServletContext().getResourcePaths("/META-INF/maven/");
		if (mavenDir == null || mavenDir.isEmpty()) {
			return null;
		}
		final Set<?> groupDir = Parameters.getServletContext()
				.getResourcePaths((String) mavenDir.iterator().next());
		if (groupDir == null || groupDir.isEmpty()) {
			return null;
		}
		final InputStream pomXml = Parameters.getServletContext()
				.getResourceAsStream(groupDir.iterator().next() + "pom.xml");
		if (pomXml == null) {
			return null;
		}
		return new BufferedInputStream(pomXml);
	}

	static File getSourceJarFile(URL classesJarFileUrl) throws IOException {
		final Map<String, String> mavenCoordinates = getMavenCoordinatesFromJarFile(
				classesJarFileUrl);
		if (mavenCoordinates == null) {
			return null;
		}
		final String groupId = mavenCoordinates.get("groupId");
		final String artifactId = mavenCoordinates.get("artifactId");
		final String version = mavenCoordinates.get("version");
		final File storageDirectory = Parameters
				.getStorageDirectory(Parameters.getCurrentApplication());
		final File srcJarFile = new File(storageDirectory,
				"sources/" + artifactId + '-' + version + "-sources.jar");
		if (!srcJarFile.exists() || srcJarFile.length() == 0) {
			for (final String mavenRepository : getMavenRepositories()) {
				final String url = mavenRepository + '/' + groupId.replace('.', '/') + '/'
						+ artifactId + '/' + version + '/' + artifactId + '-' + version
						+ "-sources.jar";
				if (!url.startsWith("http")) {
					if (!new File(url).exists()) { // NOPMD
						continue;
					}
					return new File(url);
				}
				mkdirs(srcJarFile.getParentFile());
				final OutputStream output = new FileOutputStream(srcJarFile);
				try {
					final URL sourceUrl = new URL(url);
					final LabradorRetriever labradorRetriever = new LabradorRetriever(sourceUrl);
					labradorRetriever.downloadTo(output);
					// si trouvé, on arrête
					break;
				} catch (final IOException e) {
					output.close();
					delete(srcJarFile);
					// si non trouvé, on continue avec le repo suivant s'il y en a un
				} finally {
					output.close();
				}
			}
		}
		if (srcJarFile.exists()) {
			return srcJarFile;
		}
		return null;
	}

	private static void mkdirs(File directory) {
		if (!directory.exists() && !directory.mkdirs()) {
			throw new IllegalStateException("Can't create directory " + directory.getPath());
		}
	}

	private static boolean delete(File file) {
		return file.delete();
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	private static Map<String, String> getMavenCoordinatesFromJarFile(URL jarFileLocation)
			throws IOException {
		final ZipInputStream zipInputStream = new ZipInputStream(
				new BufferedInputStream(jarFileLocation.openStream(), 4096));
		try {
			ZipEntry entry = zipInputStream.getNextEntry();
			while (entry != null) {
				if (entry.getName().startsWith("META-INF/maven/")
						&& entry.getName().endsWith("/pom.properties")) {
					final Properties properties = new Properties();
					properties.load(zipInputStream);
					return new HashMap<String, String>((Map) properties);
				}
				zipInputStream.closeEntry();
				entry = zipInputStream.getNextEntry();
			}
		} finally {
			zipInputStream.close();
		}
		return null;
	}

	private static List<String> getMavenRepositories() {
		final String parameter = Parameters.getParameter(Parameter.MAVEN_REPOSITORIES);
		if (parameter != null) {
			final List<String> result = new ArrayList<String>();
			for (final String repo : parameter.split(",")) {
				result.add(repo.trim());
			}
			return result;
		}
		return Arrays.asList(LOCAL_REPO.getPath(), MAVEN_CENTRAL);
	}
}
