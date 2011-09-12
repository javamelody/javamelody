/*
 * Copyright 2008-2010 by Emeric Vernat
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

import javax.naming.Binding;
import javax.naming.Context;
import javax.naming.NamingEnumeration;
import javax.naming.NamingException;

/**
 * Partie du rapport html pour l'arbre JNDI.
 * @author Emeric Vernat
 */
class HtmlJndiTreeReport {
	private static final String JNDI_PREFIX = "java:";
	private final Context context;
	private final String path;
	private final Writer writer;

	HtmlJndiTreeReport(Context context, String path, Writer writer) {
		super();
		assert context != null;
		assert writer != null;

		this.context = context;
		if (path != null) {
			this.path = path;
		} else if (Parameters.getServletContext().getServerInfo().contains("GlassFish")) {
			// dans glassfish 3.0.1, context.listBindings("java:") ne retourne aucun binding à part
			// lui-même, donc par défaut dans glassfish on prend le path "comp" et non ""
			this.path = "comp";
		} else {
			this.path = "";
		}
		this.writer = writer;
	}

	void toHtml() throws IOException, NamingException {
		writeLinks();
		writeln("<br/>");

		writeln("<img src='?resource=jndi.png' width='24' height='24' alt='#Arbre_JNDI#' />&nbsp;");
		if (path.length() == 0) {
			writeln("<b>#Arbre_JNDI#</b>");
		} else {
			writer.write("<b>"
					+ I18N.getFormattedString("Arbre_JNDI_pour_contexte", htmlEncode(path))
					+ "</b>\n");
		}
		writeTable();
	}

	private void writeTable() throws IOException, NamingException {
		writeln("<table class='sortable' width='100%' border='1' cellspacing='0' cellpadding='2' summary='#Arbre_JNDI#'>");
		write("<thead><tr><th>#Nom#</th><th>#Type#</th>");
		writeln("</tr></thead><tbody>");
		final String jndiName;
		if (Parameters.getServletContext().getServerInfo().contains("WebLogic")) {
			// path + '/' nécessaire pour WebLogic 10.3.1.0 mais pas supporté dans JBoss
			jndiName = JNDI_PREFIX + path + '/';
		} else {
			jndiName = JNDI_PREFIX + path;
		}
		final NamingEnumeration<Binding> enumeration = context.listBindings(jndiName);
		try {
			boolean odd = false;
			while (enumeration.hasMore()) {
				try {
					final Binding binding = enumeration.next();
					if (odd) {
						write("<tr class='odd' onmouseover=\"this.className='highlight'\" onmouseout=\"this.className='odd'\">");
					} else {
						write("<tr onmouseover=\"this.className='highlight'\" onmouseout=\"this.className=''\">");
					}
					odd = !odd; // NOPMD
					writeBinding(binding);
					writeln("</tr>");
				} catch (final Exception e) {
					// catch Exception et non catch NamingException car glassfish 3.1 par exemple
					// lance parfois des RuntimeException encapsulant des NamingException lors du next()
					continue;
				}
			}
			writeln("</tbody></table>");
		} finally {
			// Comme indiqué dans la javadoc enumeration.close() n'est pas nécessaire après que hasMore()
			// a retourné false. De plus, cela provoquerait une exception dans glassfish 3.0.1
			// "javax.naming.OperationNotSupportedException: close() not implemented"
			// enumeration.close();

			context.close();
		}
	}

	private void writeBinding(Binding binding) throws IOException {
		final String name = getBindingName(binding);
		// si on veux corriger http://java.net/jira/browse/GLASSFISH-12831
		// sous glassfish 3.0.1 et non 3.1, les bindings d'un contexte contienne le contexte lui-même
		//		if (name.length() == 0) {
		//			return;
		//		}
		write("<td>");
		final String encodedName = htmlEncode(name);
		final String className = binding.getClassName();
		if (binding.getObject() instanceof Context || "javax.naming.Context".equals(className)) {
			// "javax.naming.Context".equals(className) nécessaire pour le path "comp" dans JBoss 6.0
			final String contextPath;
			if (path.length() > 0) {
				contextPath = path + '/' + name;
			} else {
				// nécessaire pour jonas 5.1.0
				contextPath = name;
			}
			writer.write("<a href=\"?part=jndi&amp;path=" + htmlEncode(contextPath) + "\">");
			writer.write("<img width='16' height='16' src='?resource=folder.png' alt='"
					+ encodedName + "' />&nbsp;");
			writer.write(encodedName);
			writer.write("</a>");
		} else {
			writer.write(encodedName);
		}
		write("</td>");
		write("<td>");
		writer.write(className != null ? htmlEncode(className) : "&nbsp;");
		write("</td>");
	}

	private String getBindingName(Binding binding) {
		// nécessaire pour glassfish 3.0.1
		String result = binding.getName();
		if (result.startsWith(JNDI_PREFIX)) {
			result = result.substring(JNDI_PREFIX.length());
		}
		if (result.startsWith(path)) {
			result = result.substring(path.length());
		}
		if (result.length() > 0 && result.charAt(0) == '/') {
			result = result.substring(1);
		}
		return result;
	}

	private void writeLinks() throws IOException {
		writeln("<div class='noPrint'>");
		writeln("<a href='javascript:history.back()'><img src='?resource=action_back.png' alt='#Retour#'/> #Retour#</a>");
		writeln("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
		writer.write("<a href='?#systeminfo'>");
		writeln("<img src='?resource=action_home.png' alt='#Page_principale#'/> #Page_principale#</a>");
		writeln("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
		writeln("<a href='?part=jndi&amp;path=" + htmlEncode(path)
				+ "'><img src='?resource=action_refresh.png' alt='#Actualiser#'/> #Actualiser#</a>");
		writeln("</div>");
	}

	private static String htmlEncode(String text) {
		return I18N.htmlEncode(text, true);
	}

	private void write(String html) throws IOException {
		I18N.writeTo(html, writer);
	}

	private void writeln(String html) throws IOException {
		I18N.writelnTo(html, writer);
	}
}
