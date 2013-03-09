/*
 * Copyright 2008-2012 by Emeric Vernat
 *
 *     This file is part of Java Melody.
 *
 * Java Melody is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Java Melody is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Java Melody.  If not, see <http://www.gnu.org/licenses/>.
 */
package net.bull.javamelody;

import java.io.IOException;
import java.io.Writer;

/**
 * JNLP pour lancer l'ihm Swing avec JavaWebStart.
 * @author Emeric Vernat
 */
class JnlpPage {
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
		println("      <property name='javamelody.application' value='"
				+ collector.getApplication() + "'/>");
		println("      <property name='javamelody.collectorServer' value='"
				+ (collectorServer != null) + "'  />");
		String url;
		if (collectorServer == null) {
			url = codebase + "?format=serialized";
		} else {
			url = codebase + "?format=serialized&application=" + collector.getApplication();
		}
		println("      <property name='javamelody.url' value='" + url + "' />");
		println("      <property name='javamelody.range' value='" + range.getValue() + "'/>");
		println("      <property name='javamelody.locale' value='" + I18N.getCurrentLocale()
				+ "'/>");
		// les valeurs des paramètres sont importantes notamment pour :
		// WARNING_THRESHOLD_MILLIS, SEVERE_THRESHOLD_MILLIS, SYSTEM_ACTIONS_ENABLED et NO_DATABASE
		for (final Parameter parameter : Parameter.values()) {
			if (Parameters.getParameter(parameter) != null && parameter != Parameter.ADMIN_EMAILS) {
				println("      <property name='javamelody." + parameter.getCode() + "' value='"
						+ Parameters.getParameter(parameter) + "'/>");
			}
		}
		if (cookies != null) {
			println("      <property name='cookies' value='" + cookies + "' />");
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
