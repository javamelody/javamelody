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
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
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
		final byte[] pomProperties = readMavenFileFromJarFile(classesJarFileUrl, "pom.properties");
		if (pomProperties == null) {
			return null;
		}
		final Properties properties = new Properties();
		properties.load(new ByteArrayInputStream(pomProperties));
		final String groupId = properties.getProperty("groupId");
		final String artifactId = properties.getProperty("artifactId");
		final String version = properties.getProperty("version");
		final String filePath = groupId.replace('.', '/') + '/' + artifactId + '/' + version + '/'
				+ artifactId + '-' + version + "-sources.jar";
		return getMavenArtifact(filePath);
	}

	private static File getMavenArtifact(String filePath) throws IOException {
		final File storageDirectory = Parameters
				.getStorageDirectory(Parameters.getCurrentApplication());
		final File file = new File(storageDirectory,
				"sources/" + filePath.substring(filePath.lastIndexOf('/') + 1));
		if (!file.exists() || file.length() == 0) {
			for (final String mavenRepository : getMavenRepositories()) {
				final String url = mavenRepository + '/' + filePath;
				if (!url.startsWith("http")) {
					if (!new File(url).exists()) {
						continue;
					}
					return new File(url);
				}
				mkdirs(file.getParentFile());
				final OutputStream output = new FileOutputStream(file);
				try {
					final LabradorRetriever labradorRetriever = new LabradorRetriever(new URL(url));
					labradorRetriever.downloadTo(output);
					// si trouvé, on arrête
					break;
				} catch (final IOException e) {
					output.close();
					delete(file);
					// si non trouvé, on continue avec le repo suivant s'il y en a un
				} finally {
					output.close();
				}
			}
		}
		if (file.exists()) {
			return file;
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

	private static byte[] readMavenFileFromJarFile(URL jarFileLocation, String pomFileName)
			throws IOException {
		final ZipInputStream zipInputStream = new ZipInputStream(
				new BufferedInputStream(jarFileLocation.openStream(), 4096));
		try {
			ZipEntry entry = zipInputStream.getNextEntry();
			while (entry != null) {
				if (entry.getName().startsWith("META-INF/maven/")
						&& entry.getName().endsWith("/" + pomFileName)) {
					final ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
					TransportFormat.pump(zipInputStream, byteArrayOutputStream);
					return byteArrayOutputStream.toByteArray();
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
