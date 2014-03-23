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
import java.util.List;

import net.bull.javamelody.MBeanNode.MBeanAttribute;

/**
 * Partie du rapport html pour les MBeans.
 * @author Emeric Vernat
 */
class HtmlMBeansReport extends HtmlAbstractReport {
	private static final String BR = "<br/>";
	private final List<MBeanNode> mbeans;
	private final String pid = PID.getPID();
	private int sequence;

	HtmlMBeansReport(List<MBeanNode> mbeans, Writer writer) {
		super(writer);
		assert mbeans != null;
		this.mbeans = mbeans;
	}

	@Override
	void toHtml() throws IOException {
		writeLinks();
		writeln(BR);

		writeTitle("mbeans.png", getString("MBeans"));

		writeTree();
	}

	void writeTree() throws IOException {
		if (mbeans.isEmpty()) {
			return;
		}
		final String endDiv = "</div>";

		// MBeans pour la plateforme
		final MBeanNode platformNode = mbeans.get(0);
		writeln("<div style='margin-left: 20px'>");
		writeTree(platformNode.getChildren());
		writeln(endDiv);

		for (final MBeanNode node : mbeans) {
			if (node != platformNode) {
				writeDirectly("<br/><b>" + htmlEncodeButNotSpace(node.getName()) + "</b>");
				writeln("<div style='margin-left: 20px'><br/>");
				writeTree(node.getChildren());
				writeln(endDiv);
			}
		}
	}

	private void writeTree(List<MBeanNode> nodes) throws IOException {
		boolean first = true;
		for (final MBeanNode node : nodes) {
			final String name = node.getName();
			if (first) {
				first = false;
			} else {
				write(BR);
			}
			final List<MBeanNode> children = node.getChildren();
			if (children != null) {
				final String id = getNextId();
				writePrintedShowHideLink(id, htmlEncodeButNotSpace(name));
				writeln("<div id='" + id + "' style='display: none; margin-left: 20px;'><div>");
				writeTree(children);
				writeln("</div></div>");
			} else {
				writeMBeanNode(node);
			}
		}
	}

	private void writeMBeanNode(MBeanNode mbean) throws IOException {
		String mbeanName = mbean.getName();
		final String mbeanId = getNextId();
		final int indexOfComma = mbeanName.indexOf(',');
		if (indexOfComma != -1) {
			mbeanName = mbeanName.substring(indexOfComma + 1);
			writePrintedShowHideLink(mbeanId, htmlEncodeButNotSpace(mbeanName));
			writeln("<div id='" + mbeanId + "' style='display: none; margin-left: 20px;'>");
			// pas besoin d'ajouter un div pour le scroll-down, car les attributs sont
			// dans une table
			writeAttributes(mbean);
			writeln("</div>");
		} else {
			writeAttributes(mbean);
		}
	}

	private void writeAttributes(MBeanNode mbean) throws IOException {
		final String description = mbean.getDescription();
		final List<MBeanAttribute> attributes = mbean.getAttributes();
		if (description != null || !attributes.isEmpty()) {
			writeln("<style type='text/css'>");
			writeln("    td { padding:3px; }");
			writeln("</style>");
			writeln("<table border='0' summary=''>");
			if (description != null) {
				write("<tr><td colspan='3'>(");
				writeDirectly(htmlEncodeButNotSpace(description));
				write(")</td></tr>");
			}
			for (final MBeanAttribute attribute : attributes) {
				writeAttribute(mbean, attribute);
			}
			writeln("</table>");
		}
	}

	private void writeAttribute(MBeanNode mbean, MBeanAttribute attribute) throws IOException {
		final String attributeName = attribute.getName();
		final String formattedValue = attribute.getFormattedValue();
		final String description = attribute.getDescription();
		write("<tr valign='top'><td>");
		writeDirectly("<a href='?jmxValue="
				+ mbean.getName().replace(" ", "%20").replace("'", "%27") + '.' + attributeName
				+ "' ");
		writeln("title=\"#Lien_valeur_mbeans#\">-</a>&nbsp;");
		writeDirectly(htmlEncodeButNotSpace(attributeName));
		write("</td><td>");
		// \n sera encodé dans <br/> dans htmlEncode
		writeDirectly(htmlEncodeButNotSpace(formattedValue));
		write("</td><td>");
		if (description != null) {
			write("(");
			writeDirectly(htmlEncodeButNotSpace(description));
			write(")");
		} else {
			write("&nbsp;");
		}
		write("</td></tr>");
	}

	private String getNextId() {
		return 'x' + pid + '_' + sequence++;
	}

	void writeLinks() throws IOException {
		writeln("<div class='noPrint'>");
		writeln("<a href='javascript:history.back()'><img src='?resource=action_back.png' alt='#Retour#'/> #Retour#</a>");
		writeln("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
		writeln("<a href='?part=mbeans'><img src='?resource=action_refresh.png' alt='#Actualiser#'/> #Actualiser#</a>");
		if (isPdfEnabled()) {
			writeln("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
			write("<a href='?part=mbeans&amp;format=pdf' title='#afficher_PDF#'>");
			write("<img src='?resource=pdf.png' alt='#PDF#'/> #PDF#</a>");
		}
		writeln("</div>");
	}

	private void writePrintedShowHideLink(String idToShow, String label) throws IOException {
		writeDirectly("<a href=\"javascript:showHide('" + idToShow + "');\"><img id='" + idToShow
				+ "Img' src='?resource=bullets/plus.png' alt=''/> " + label + "</a>");
	}
}
