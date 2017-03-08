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
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.Reader;
import java.io.StringWriter;
import java.io.Writer;
import java.net.URL;
import java.nio.charset.Charset;
import java.security.CodeSource;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.zip.ZipEntry;
import java.util.zip.ZipException;
import java.util.zip.ZipFile;
import java.util.zip.ZipInputStream;

/**
 * Rapport html pour afficher un fichier source java.
 * @author Emeric Vernat
 */
class HtmlSourceReport extends HtmlAbstractReport {
	private static final String MAVEN_CENTRAL = "http://repo1.maven.org/maven2";

	private static final File LOCAL_REPO = new File(
			System.getProperty("user.home") + "/.m2/repository");

	private static final File JDK_SRC_FILE = getJdkSrcFile();

	private final String source;

	HtmlSourceReport(String className, Writer writer) throws IOException {
		super(writer);
		if (className.indexOf('$') == -1) {
			this.source = getSource(className);
		} else {
			this.source = getSource(className.substring(0, className.indexOf('$')));
		}
	}

	private String getSource(String className) throws IOException {
		final Class<?> clazz;
		try {
			clazz = Class.forName(className);
		} catch (final ClassNotFoundException e) {
			return null;
		}

		if (clazz.getName().startsWith("java.") || clazz.getName().startsWith("javax.")
				&& clazz.getProtectionDomain().getCodeSource() == null) {
			if (JDK_SRC_FILE != null) {
				return getSourceFromJar(clazz, JDK_SRC_FILE);
			}
			return null;
		}
		return getSourceFromMavenRepo(clazz);
	}

	private static File getJdkSrcFile() {
		File file = new File(System.getProperty("java.home"));
		if ("jre".equalsIgnoreCase(file.getName())) {
			file = file.getParentFile();
		}
		final File srcZipFile = new File(file, "src.zip");
		if (srcZipFile.exists()) {
			return srcZipFile;
		}
		return null;
	}

	private String getSourceFromJar(Class<?> clazz, File srcJarFile)
			throws ZipException, IOException {
		final ZipFile zipFile = new ZipFile(srcJarFile);
		try {
			final ZipEntry entry = zipFile.getEntry(clazz.getName().replace('.', '/') + ".java");
			if (entry == null) {
				return null;
			}
			final StringWriter writer = new StringWriter();
			final InputStream inputStream = zipFile.getInputStream(entry);
			try {
				final Reader reader = new InputStreamReader(inputStream, Charset.forName("UTF-8"));
				try {
					final char[] chars = new char[1024];
					int read = reader.read(chars);
					while (read != -1) {
						writer.write(chars, 0, read);
						read = reader.read(chars);
					}
				} finally {
					reader.close();
				}
			} finally {
				inputStream.close();
			}
			return writer.toString();
		} finally {
			zipFile.close();
		}
	}

	private String getSourceFromMavenRepo(Class<?> clazz) throws IOException {
		final Map<String, String> mavenCoordinates = getMavenCoordinates(clazz);
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
					if (!new File(url).exists()) {
						continue;
					}
					return getSourceFromJar(clazz, new File(url));
				}
				mkdirs(srcJarFile);
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
			return getSourceFromJar(clazz, srcJarFile);
		}
		return null;
	}

	private void mkdirs(File directory) {
		if (!directory.getParentFile().exists() && !directory.getParentFile().mkdirs()) {
			throw new IllegalStateException(
					"Can't create directory " + directory.getParentFile().getPath());
		}
	}

	private boolean delete(File file) {
		return file.delete();
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	private Map<String, String> getMavenCoordinates(Class<?> clazz) throws IOException {
		final CodeSource codeSource = clazz.getProtectionDomain().getCodeSource();
		if (codeSource == null) {
			return null;
		}
		final URL location = codeSource.getLocation();
		final ZipInputStream zipInputStream = new ZipInputStream(
				new BufferedInputStream(location.openStream(), 4096));
		try {
			ZipEntry entry = zipInputStream.getNextEntry();
			while (entry != null) {
				if (entry.getName().startsWith("META-INF/")
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

	private List<String> getMavenRepositories() {
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

	@Override
	void toHtml() throws IOException {
		if (source != null) {
			String html = JavaHTMLizer.htmlize(source);
			html = JavaHTMLizer.addLineNumbers(html);
			writeDirectly("<code>");
			writeDirectly(html);
			writeDirectly("</code>");
		} else {
			write("#source_not_found#");
		}
	}

	static String htmlEncodeStackTraceElement(String element) {
		if (element.endsWith(")") && !element.endsWith("(Native Method)")
				&& !element.endsWith("(Unknown Source)")) {
			final int index3 = element.lastIndexOf(':');
			final int index2 = element.lastIndexOf('(');
			final int index1 = element.lastIndexOf('.', index2);
			final int index0 = element.lastIndexOf(' ', index1);
			if (index1 > index0 && index2 != -1 && index3 > index2) {
				final String classNameEncoded = urlEncode(element.substring(index0 + 1, index1));
				return htmlEncodeButNotSpace(element.substring(0, index2 + 1)) + "<a href='?part="
						+ HttpParameters.SOURCE_PART + "&amp;class=" + classNameEncoded + '#'
						+ urlEncode(element.substring(index3 + 1, element.length() - 1))
						+ "' class='lightwindow' type='external' title='" + classNameEncoded + "'>"
						+ htmlEncode(element.substring(index2 + 1, element.length() - 1)) + "</a>)";
			}
		}
		return htmlEncodeButNotSpace(element);
	}

	static String addLinkToClassName(String className) {
		String cleanClassName = className;
		if (cleanClassName.endsWith("[]")) {
			cleanClassName = cleanClassName.substring(0, cleanClassName.length() - 2);
		}
		return "<a href='?part=" + HttpParameters.SOURCE_PART + "&amp;class=" + cleanClassName
				+ "' class='lightwindow' type='external' title='" + cleanClassName + "'>"
				+ className + "</a>";
	}
}
