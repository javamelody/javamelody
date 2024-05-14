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
package net.bull.javamelody.internal.web.html;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringWriter;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.security.CodeSource;
import java.util.Enumeration;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import net.bull.javamelody.internal.common.HttpPart;
import net.bull.javamelody.internal.model.MavenArtifact;

/**
 * Rapport html pour afficher un fichier source java.
 * @author Emeric Vernat
 */
class HtmlSourceReport extends HtmlAbstractReport {
	private static final File JDK_SRC_FILE = getJdkSrcFile();
	private static final List<String> TOMCAT_PACKAGES = List.of("org.apache.tomcat",
			"org.apache.catalina", "org.apache.coyote", "org.apache.jasper", "org.apache.el",
			"org.apache.juli", "org.apache.naming");

	private final String source;

	HtmlSourceReport(String className, Writer writer) throws IOException {
		super(writer);
		this.source = getSource(normalizeClassName(className));
	}

	private static String normalizeClassName(String className) {
		String temp = className;
		while (temp.lastIndexOf('$') != -1) {
			// className may be like org.springframework.boot.autoconfigure.web.servlet.error.ErrorMvcAutoConfiguration$$EnhancerBySpringCGLIB$$e0b05818
			temp = temp.substring(0, temp.lastIndexOf('$'));
		}
		if (temp.lastIndexOf('/') != -1) {
			// for JDK 9+
			temp = temp.substring(temp.lastIndexOf('/') + 1);
		}
		return temp;
	}

	private String getSource(String className) throws IOException {
		final Class<?> clazz;
		try {
			clazz = Class.forName(className);
		} catch (final ClassNotFoundException e) {
			return null;
		}

		final CodeSource codeSource = clazz.getProtectionDomain().getCodeSource();
		final String sourceFilePath = clazz.getName().replace('.', '/') + ".java";
		if (clazz.getName().startsWith("java.")
				|| clazz.getName().startsWith("javax.") && codeSource == null) {
			if (JDK_SRC_FILE != null) {
				return getSourceFromZip(sourceFilePath, JDK_SRC_FILE);
			}
		} else if (codeSource != null) {
			final File sourceJarFile = MavenArtifact.getSourceJarFile(codeSource.getLocation());
			if (sourceJarFile != null) {
				return getSourceFromZip(sourceFilePath, sourceJarFile);
			}
		}
		if (clazz.getName().startsWith("org.apache.")) {
			for (final String tomcatPackage : TOMCAT_PACKAGES) {
				if (clazz.getName().startsWith(tomcatPackage + '.')) {
					final File tomcatSrcFile = MavenArtifact.getTomcatSrcZipFile();
					if (tomcatSrcFile != null) {
						assert tomcatSrcFile.getName().endsWith(".zip");
						final String entryName = tomcatSrcFile.getName().substring(0,
								tomcatSrcFile.getName().length() - ".zip".length()) + "/java/"
								+ sourceFilePath;
						return getSourceFromZip(entryName, tomcatSrcFile);
					}
				}
			}
		}
		return null;
	}

	private static File getJdkSrcFile() {
		File file = new File(System.getProperty("java.home"));
		if ("jre".equalsIgnoreCase(file.getName())) {
			file = file.getParentFile();
		}
		File srcZipFile = new File(file, "src.zip");
		if (srcZipFile.exists()) {
			return srcZipFile;
		}
		// for JDK 9 +
		srcZipFile = new File(file, "lib/src.zip");
		if (srcZipFile.exists()) {
			return srcZipFile;
		}
		return null;
	}

	private String getSourceFromZip(String entryName, File srcJarFile) throws IOException {
		try (ZipFile zipFile = new ZipFile(srcJarFile)) {
			ZipEntry entry = zipFile.getEntry(entryName);
			if (entry == null) {
				// for JDK 9 + and maybe some others
				final Enumeration<? extends ZipEntry> entries = zipFile.entries();
				while (entries.hasMoreElements()) {
					entry = entries.nextElement();
					if (entry.getName().endsWith(entryName)) {
						break;
					}
				}
				if (!entry.getName().endsWith(entryName)) {
					return null;
				}
			}
			final StringWriter writer = new StringWriter();
			try (InputStream inputStream = zipFile.getInputStream(entry)) {
				try (Reader reader = new InputStreamReader(inputStream, StandardCharsets.UTF_8)) {
					final char[] chars = new char[1024];
					int read = reader.read(chars);
					while (read != -1) {
						writer.write(chars, 0, read);
						read = reader.read(chars);
					}
				}
			}
			return writer.toString();
		}
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
						+ HttpPart.SOURCE + "&amp;class=" + classNameEncoded + '#'
						+ urlEncode(element.substring(index3 + 1, element.length() - 1))
						+ "' class='lightwindow' type='external' title='" + classNameEncoded + "'>"
						+ htmlEncode(element.substring(index2 + 1, element.length() - 1)) + "</a>)";
			}
		}
		return htmlEncodeButNotSpace(element);
	}

	static String htmlEncodeStackTraceElementAndTabs(String element) {
		return htmlEncodeStackTraceElement(element).replace("\t",
				"&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
	}

	static String addLinkToClassName(String className) {
		String cleanClassName = className;
		if (cleanClassName.endsWith("[]")) {
			cleanClassName = cleanClassName.substring(0, cleanClassName.length() - 2);
		}
		return "<a href='?part=" + HttpPart.SOURCE + "&amp;class=" + cleanClassName
				+ "' class='lightwindow' type='external' title='" + cleanClassName + "'>"
				+ className + "</a>";
	}
}
