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

import java.io.IOException;
import java.io.Writer;
import java.util.Map;

import net.bull.javamelody.internal.model.MavenArtifact;

/**
 * Rapport html pour afficher la liste des dépendances dans WEB-INF/lib, avec noms, urls, licences.
 * @author Emeric Vernat
 */
class HtmlDependenciesReport extends HtmlAbstractReport {
	private final Map<String, MavenArtifact> dependencies;

	HtmlDependenciesReport(Map<String, MavenArtifact> dependencies, Writer writer) {
		super(writer);
		assert dependencies != null;
		this.dependencies = dependencies;
	}

	@Override
	void toHtml() throws IOException {
		writeBackLink();
		writeln("<br/>");

		if (dependencies.isEmpty()) {
			writeln("#Aucune_dependance#");
			return;
		}
		writeTitle("beans.png", getString("Dependencies"));
		final HtmlTable table = new HtmlTable();
		table.beginTable(getString("Dependencies"));
		// table of dependencies inspired by Jenkins "/about"
		write("<th>Artifact</th><th>#Nom#</th><th>Maven ID</th><th>#Licence#</th>");
		for (final Map.Entry<String, MavenArtifact> entry : dependencies.entrySet()) {
			final String jarFilename = entry.getKey();
			final MavenArtifact dependency = entry.getValue();
			table.nextRow();
			writeDependency(jarFilename, dependency);
		}
		table.endTable();
		writeln("<div align='right'>" + getFormattedString("nb_dependencies", dependencies.size())
				+ "</div>");
	}

	private void writeBackLink() throws IOException {
		writeln("<div class='noPrint'>");
		writeln("<a href='javascript:history.back()'>");
		writeln("<img src='?resource=action_back.png' alt='#Retour#'/> #Retour#</a>");
		writeln("</div>");
	}

	private void writeDependency(String jarFilename, MavenArtifact dependency) throws IOException {
		write("<td>");
		writeDirectly(htmlEncodeButNotSpace(jarFilename));
		write("</td><td>");
		if (dependency == null) {
			write("</td><td>");
			write("</td><td>");
		} else {
			if (dependency.getName() != null) {
				if (dependency.getUrl() == null) {
					writeDirectly(htmlEncodeButNotSpace(dependency.getName()));
				} else {
					writeDirectly("<a href='" + urlEncode(dependency.getUrl()) + "'>"
							+ htmlEncodeButNotSpace(dependency.getName()) + "</a>");
				}
			}
			write("</td><td>");
			writeDirectly(dependency.getGroupId() + ':' + dependency.getArtifactId() + ':' + "<b>"
					+ dependency.getVersion() + "</b>");
			write("</td><td>");
			boolean firstLicense = true;
			for (final Map.Entry<String, String> entry : dependency.getLicenseUrlsByName()
					.entrySet()) {
				final String licenseName = entry.getKey();
				final String licenseUrl = entry.getValue();
				if (!firstLicense) {
					write("<br/>");
				}
				if (licenseUrl == null || !licenseUrl.startsWith("http")) {
					writeDirectly(htmlEncodeButNotSpace(licenseName));
				} else {
					writeDirectly("<a href='" + urlEncode(licenseUrl) + "'>"
							+ htmlEncodeButNotSpace(licenseName) + "</a>");
				}
				firstLicense = false;
			}
		}

		write("</td>");
	}
}
