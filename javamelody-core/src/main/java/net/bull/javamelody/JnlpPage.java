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

import java.io.IOException;
import java.io.Writer;
import java.util.LinkedHashMap;
import java.util.Map;

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
		println("<jnlp spec='1.0+' codebase='" + codebase + "'>");
		println("   <information>");
		println("      <title>JavaMelody</title>");
		println("      <vendor>JavaMelody</vendor>");
		println("      <description>Monitoring</description>");
		println("      <icon href='" + codebase + "?resource=systemmonitor.png'/>");
		println("      <offline-allowed />");
		println("   </information>");
		println("   <security> <all-permissions/> </security>");
		println("   <update check='always' policy='always'/>");
		println("   <resources>");
		println("      <j2se version='1.7+' max-heap-size='300m'/>");
		// il serait possible de télécharger le jar depuis googlecode vers le serveur,
		// et stocker le jar sur le serveur local pour le fournir par http à javawebstart,
		// mais dans la plupart des grandes entreprises, il faudrait connaître l'adresse du proxy pour le téléchargement
		final String jarFileUrl;
		if (Parameters.getParameter(Parameter.JAVAMELODY_SWING_URL) != null) {
			jarFileUrl = Parameters.getParameter(Parameter.JAVAMELODY_SWING_URL);
		} else if (Parameters.JAVAMELODY_VERSION != null) {
			jarFileUrl = "http://javamelody.googlecode.com/files/javamelody-swing-"
					+ Parameters.JAVAMELODY_VERSION + ".jar";
		} else {
			jarFileUrl = "http://javamelody.googlecode.com/files/javamelody-swing.jar";
		}
		println("      <jar href='" + jarFileUrl + "' />");
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
}
