/*
 * Copyright 2008-2014 by Emeric Vernat
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

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.Writer;
import java.net.URL;
import java.net.URLConnection;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.zip.ZipFile;

/**
 * JNLP pour lancer l'ihm Swing avec JavaWebStart.
 * @author Emeric Vernat
 */
class JnlpPage {
	static final String JNLP_PREFIX = "jnlp.";
	private final Collector collector;
	private final CollectorServer collectorServer;
	private final String codebase;
	private final String cookies;
	private final Range range;
	private final Writer writer;

	JnlpPage(Collector collector, CollectorServer collectorServer, String codebase, String cookies,
			Range range, Writer writer) {
		super();
		this.collector = collector;
		this.collectorServer = collectorServer;
		this.codebase = codebase;
		this.cookies = cookies;
		this.range = range;
		this.writer = writer;
	}

	void toJnlp() throws IOException {
		final File jarFile = getJarFile();
		println("<jnlp spec='1.0+' codebase='" + codebase + "'>");
		println("   <information>");
		println("      <title>JavaMelody</title>");
		println("      <vendor>JavaMelody</vendor>");
		println("      <description>Monitoring</description>");
		if (!jarFile.exists()) {
			// pas d'icône si jar existe, sinon conflit avec l'url du jar ci-dessous, dans le cache JWS
			println("      <icon href='" + codebase + "?resource=systemmonitor.png'/>");
		}
		println("      <offline-allowed />");
		println("   </information>");
		println("   <security> <all-permissions/> </security>");
		println("   <update check='always' policy='always'/>");
		println("   <resources>");
		println("      <j2se version='1.7+' max-heap-size='300m'/>");
		if (jarFile.exists()) {
			println("      <jar href='" + codebase + "?part=" + HttpParameters.DESKTOP_JAR_PART
					+ "' size='" + jarFile.length() + "'/>");
		} else {
			final String jarFileUrl = getJarFileUrl();
			println("      <jar href='" + jarFileUrl + "' />");
		}
		final Map<String, Object> properties = new LinkedHashMap<String, Object>();
		properties.put("javamelody.application", collector.getApplication());
		properties.put("javamelody.collectorServer", collectorServer != null);
		String url;
		if (collectorServer == null) {
			url = codebase + "?format=serialized";
		} else {
			url = codebase + "?format=serialized&application=" + collector.getApplication();
		}
		properties.put("javamelody.url", url);
		properties.put("javamelody.range", range.getValue());
		properties.put("javamelody.locale", I18N.getCurrentLocale());
		// les valeurs des paramètres sont importantes notamment pour :
		// WARNING_THRESHOLD_MILLIS, SEVERE_THRESHOLD_MILLIS, SYSTEM_ACTIONS_ENABLED et NO_DATABASE
		for (final Parameter parameter : Parameter.values()) {
			if (Parameters.getParameter(parameter) != null && parameter != Parameter.ADMIN_EMAILS) {
				properties.put("javamelody." + parameter.getCode(),
						Parameters.getParameter(parameter));
			}
		}
		if (cookies != null) {
			properties.put("cookies", cookies);
		}
		// JNLP_PREFIX to fix:
		// http://stackoverflow.com/questions/19400725/with-java-update-7-45-the-system-properties-no-more-set-from-jnlp-tag-property
		// https://bugs.openjdk.java.net/browse/JDK-8023821
		for (final Map.Entry<String, Object> entry : properties.entrySet()) {
			println("      <property name='" + JNLP_PREFIX + entry.getKey() + "' value='"
					+ entry.getValue() + "'/>");
		}
		println("   </resources>");
		println("   <application-desc main-class='net.bull.javamelody.Main' />");
		println("</jnlp>");
	}

	private void println(String string) throws IOException {
		writer.write(string);
		writer.write('\n');
	}

	private static String getJarFileUrl() {
		final String jarFileUrl;
		if (Parameters.getParameter(Parameter.JAVAMELODY_SWING_URL) != null) {
			jarFileUrl = Parameters.getParameter(Parameter.JAVAMELODY_SWING_URL);
		} else if (Parameters.JAVAMELODY_VERSION != null) {
			if (Parameters.JAVAMELODY_VERSION.compareTo("1.49.0") <= 0) {
				jarFileUrl = "http://javamelody.googlecode.com/files/javamelody-swing-"
						+ Parameters.JAVAMELODY_VERSION + ".jar";
			} else {
				// TODO files can't be added in googlecode downloads anymore,		
				// at the moment, javamelody-swing v1.49 is used for v1.50 and later
				jarFileUrl = "http://javamelody.googlecode.com/files/javamelody-swing-1.49.0.jar";
			}
		} else {
			jarFileUrl = "http://javamelody.googlecode.com/files/javamelody-swing.jar";
		}
		return jarFileUrl;
	}

	static File getJarFile() {
		final String jarFileUrl = getJarFileUrl();
		if (jarFileUrl.lastIndexOf('/') != -1) {
			final String jarFileName = jarFileUrl.substring(jarFileUrl.lastIndexOf('/') + 1);
			return new File(Parameters.getStorageDirectory(""), jarFileName);
		}
		return new File(jarFileUrl);
	}

	static void cacheDesktopJarIfNeededAsync() {
		if (!getJarFile().exists()) {
			final Runnable runnable = new Runnable() {
				@Override
				public void run() {
					try {
						// wait 62s to let the webapp start first
						Thread.sleep(62000);

						cacheDesktopJar();
					} catch (final InterruptedException e) {
						LOG.debug(e.toString());
					}
				}
			};
			final Thread thread = new Thread(runnable, "JavaMelody Desktop Jar caching");
			thread.setDaemon(true);
			thread.start();
		}
	}

	static File cacheDesktopJar() {
		// téléchargement du jar depuis googlecode vers le serveur,
		// et stockage du jar sur le serveur local pour le fournir par http à javawebstart,
		// mais dans des grandes entreprises, il faut que l'adresse du proxy pour le téléchargement soit configuré
		// (sinon, c'est JWS qui téléchargera avec son proxy)
		final String jarFileUrl = getJarFileUrl();
		final File jarFile = getJarFile();
		LOG.debug("trying to download desktop jar to put in cache, from " + jarFileUrl);
		try {
			final File directory = jarFile.getParentFile();
			if (!directory.mkdirs() && !directory.exists()) {
				throw new IOException("JavaMelody directory can't be created: "
						+ directory.getPath());
			}
			final long start = System.currentTimeMillis();
			final URLConnection connection = new URL(jarFileUrl).openConnection();
			connection.setUseCaches(false);
			connection.setConnectTimeout(300000);
			connection.setReadTimeout(300000);
			final ByteArrayOutputStream output = new ByteArrayOutputStream();
			final InputStream input = connection.getInputStream();
			try {
				TransportFormat.pump(input, output);
			} finally {
				input.close();
			}

			final FileOutputStream fileOutputStream = new FileOutputStream(jarFile);
			try {
				fileOutputStream.write(output.toByteArray());
			} finally {
				fileOutputStream.close();
			}

			try {
				new ZipFile(jarFile).close();
			} catch (final Exception e) {
				if (!jarFile.delete()) {
					jarFile.deleteOnExit();
				}
				final IOException exception = new IOException(
						"desktop jar downloaded is not in zip format - proxy error page?");
				exception.initCause(e);
				throw exception;
			}
			final long duration = System.currentTimeMillis() - start;
			LOG.debug("desktop jar downloaded and put in cache, from " + jarFileUrl + " to "
					+ jarFile + ", in " + duration + " ms, for " + jarFile.length() / 1024 + " KB");
			return jarFile;
		} catch (final IOException e) {
			LOG.debug("Can't download desktop jar to put in cache, from " + jarFileUrl
					+ " - perhaps http proxy is not configured?"
					+ " So, desktop jar will be downloaded from Internet in JWS. " + e.toString());
			return null;
		}
	}
}
