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
import java.util.List;

import net.bull.javamelody.internal.model.JndiBinding;

/**
 * Partie du rapport html pour l'arbre JNDI.
 * @author Emeric Vernat
 */
class HtmlJndiTreeReport extends HtmlAbstractReport {
	private final List<JndiBinding> jndiBindings;
	private final String path;

	HtmlJndiTreeReport(List<JndiBinding> jndiBindings, String path, Writer writer) {
		super(writer);
		assert jndiBindings != null;

		this.jndiBindings = jndiBindings;
		this.path = JndiBinding.normalizePath(path);
	}

	@Override
	void toHtml() throws IOException {
		writeLinks();
		writeln("<br/>");

		final String title;
		if (path.isEmpty()) {
			title = getString("Arbre_JNDI");
		} else {
			title = getFormattedString("Arbre_JNDI_pour_contexte", htmlEncode(path));
		}
		writeTitle("jndi.png", title);
		writeTable();
	}

	private void writeTable() throws IOException {
		final HtmlTable table = new HtmlTable();
		table.beginTable(getString("Arbre_JNDI"));
		write("<th>#Nom#</th><th>#Type#</th><th>#Value#</th>");
		for (final JndiBinding binding : jndiBindings) {
			table.nextRow();
			writeBinding(binding);
		}
		table.endTable();
	}

	private void writeBinding(JndiBinding binding) throws IOException {
		write("<td>");
		final String name = binding.getName();
		final String className = binding.getClassName();
		final String contextPath = binding.getContextPath();
		final String value = binding.getValue();
		if (contextPath != null) {
			writeDirectly("<a href=\"?part=jndi&amp;path=" + urlEncode(contextPath) + "\">");
			writeDirectly("<img width='16' height='16' src='?resource=folder.png' alt='"
					+ urlEncode(name) + "' />&nbsp;");
			writeDirectly(htmlEncode(name));
			writeDirectly("</a>");
		} else {
			writeDirectly(htmlEncode(name));
		}
		write("</td>");
		write("<td>");
		writeDirectly(className != null ? htmlEncode(className) : "&nbsp;");
		write("</td>");
		write("<td>");
		writeDirectly(value != null ? htmlEncodeButNotSpace(value) : "&nbsp;");
		write("</td>");
	}

	private void writeLinks() throws IOException {
		writeln("<div class='noPrint'>");
		writeln("<a href='javascript:history.back()'><img src='?resource=action_back.png' alt='#Retour#'/> #Retour#</a>");
		writeln("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
		writeDirectly("<a href='?#systeminfo'>");
		writeln("<img src='?resource=action_home.png' alt='#Page_principale#'/> #Page_principale#</a>");
		writeln("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
		writeln("<a href='?part=jndi&amp;path=" + htmlEncode(path)
				+ "'><img src='?resource=action_refresh.png' alt='#Actualiser#'/> #Actualiser#</a>");
		if (isPdfEnabled()) {
			writeln("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
			write("<a href='?part=jndi&amp;path=" + htmlEncode(path)
					+ "&amp;format=pdf' title='#afficher_PDF#'>");
			write("<img src='?resource=pdf.png' alt='#PDF#'/> #PDF#</a>");
		}
		writeln("</div>");
	}
}
