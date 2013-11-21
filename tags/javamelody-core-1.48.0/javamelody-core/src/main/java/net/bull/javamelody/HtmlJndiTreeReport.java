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
import java.util.List;

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
		if (path.length() == 0) {
			title = getString("Arbre_JNDI");
		} else {
			title = getFormattedString("Arbre_JNDI_pour_contexte", htmlEncode(path));
		}
		writeTitle("jndi.png", title);
		writeTable();
	}

	private void writeTable() throws IOException {
		final HtmlTable table = new HtmlTable();
		table.beginTable("Arbre_JNDI");
		write("<th>#Nom#</th><th>#Type#</th><th>#Value#</th>");
		for (final JndiBinding binding : jndiBindings) {
			table.nextRow();
			writeBinding(binding);
		}
		table.endTable();
	}

	private void writeBinding(JndiBinding binding) throws IOException {
		final String name = binding.getName();
		write("<td>");
		final String encodedName = htmlEncode(name);
		final String className = binding.getClassName();
		final String contextPath = binding.getContextPath();
		final String value = binding.getValue();
		if (contextPath != null) {
			writeDirectly("<a href=\"?part=jndi&amp;path=" + htmlEncode(contextPath) + "\">");
			writeDirectly("<img width='16' height='16' src='?resource=folder.png' alt='"
					+ encodedName + "' />&nbsp;");
			writeDirectly(encodedName);
			writeDirectly("</a>");
		} else {
			writeDirectly(encodedName);
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
