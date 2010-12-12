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
import java.util.List;
import java.util.Map;

import javax.management.JMException;
import javax.management.MBeanAttributeInfo;
import javax.management.MBeanInfo;
import javax.management.ObjectName;

/**
 * Partie du rapport html pour les MBeans.
 * @author Emeric Vernat
 */
class HtmlMBeansReport {
	private static final String JAVA_LANG_MBEAN_DESCRIPTION = "Information on the management interface of the MBean";
	private static final String BR = "<br/>";
	private final MBeans mbeans;
	private final Writer writer;
	private final String pid = PID.getPID();
	private int sequence;

	HtmlMBeansReport(Writer writer) {
		super();
		assert writer != null;
		this.writer = writer;
		mbeans = new MBeans();
	}

	void toHtml() throws IOException, JMException {
		writeLinks();
		writeln(BR);

		writeln("<img src='?resource=mbeans.png' width='24' height='24' alt='#MBeans#' />&nbsp;");
		writeln("<b>#MBeans#</b>");
		writeTree();
	}

	/**
	 * Affiche l'arbre des MBeans.
	 * @throws IOException e
	 * @throws JMException e
	 */
	void writeTree() throws IOException, JMException {
		writeln("<div style='margin-left: 20px'>");
		final Map<String, Map<String, List<ObjectName>>> mapObjectNamesByDomainAndFirstProperty = mbeans
				.getMapObjectNamesByDomainAndFirstProperty();
		for (final Map.Entry<String, Map<String, List<ObjectName>>> entryObjectNamesByDomainAndFirstProperty : mapObjectNamesByDomainAndFirstProperty
				.entrySet()) {
			final String domain = entryObjectNamesByDomainAndFirstProperty.getKey();
			final String domainId = getNextId();
			write(BR);
			writeShowHideLink(domainId, htmlEncode(domain));
			writeln("<div id='" + domainId + "' style='display: none; margin-left: 20px;'><div>");
			final Map<String, List<ObjectName>> mapObjectNamesByFirstProperty = entryObjectNamesByDomainAndFirstProperty
					.getValue();
			boolean firstInDomain = true;
			for (final Map.Entry<String, List<ObjectName>> entryObjectNamesByFirstProperty : mapObjectNamesByFirstProperty
					.entrySet()) {
				final String firstProperty = entryObjectNamesByFirstProperty.getKey();
				final String firstPropertyId = getNextId();
				if (firstInDomain) {
					firstInDomain = false;
				} else {
					write(BR);
				}
				writeShowHideLink(firstPropertyId, htmlEncode(firstProperty));
				writeln("<div id='" + firstPropertyId
						+ "' style='display: none; margin-left: 20px;'><div>");
				final List<ObjectName> objectNames = entryObjectNamesByFirstProperty.getValue();
				boolean firstMBean = true;
				for (final ObjectName name : objectNames) {
					String mbean = name.toString();
					final String mbeanId = getNextId();
					final int indexOfComma = mbean.indexOf(',');
					if (indexOfComma != 1) {
						mbean = mbean.substring(indexOfComma + 1);
					}
					if (firstMBean) {
						firstMBean = false;
					} else {
						write(BR);
					}
					writeShowHideLink(mbeanId, htmlEncode(mbean));
					writeln("<div id='" + mbeanId + "' style='display: none; margin-left: 20px;'>");
					// pas besoin d'ajouter un div pour le scroll-down, car les attributs sont
					// dans une table
					writeAttributes(name);
					writeln("</div>");
				}
				writeln("</div></div>");
			}
			writeln("</div></div>");
		}
		writeln("</div>");
	}

	private String getNextId() {
		return 'x' + pid + '_' + sequence++;
	}

	private void writeAttributes(ObjectName name) throws JMException, IOException {
		final MBeanInfo mbeanInfo = mbeans.getMBeanInfo(name);
		final MBeanAttributeInfo[] attributeInfos = mbeanInfo.getAttributes();
		final Map<String, Object> attributes = mbeans.getAttributes(name, attributeInfos);
		final String description = mbeanInfo.getDescription();
		// les descriptions des MBeans de java.lang n'apportent aucune information utile
		final boolean descriptionDisplayed = description != null
				&& !JAVA_LANG_MBEAN_DESCRIPTION.equals(description);
		if (!attributes.isEmpty() || descriptionDisplayed) {
			writeln("<table border='0' cellspacing='0' cellpadding='3' summary=''>");
			if (descriptionDisplayed) {
				write("<tr><td colspan='3'>(");
				writer.write(htmlEncode(description));
				write(")</td></tr>");
			}
			for (final Map.Entry<String, Object> entryAttributes : attributes.entrySet()) {
				final String attributeName = entryAttributes.getKey();
				final Object attributeValue = entryAttributes.getValue();
				writeAttribute(name, attributeName, attributeValue, attributeInfos);
			}
			writeln("</table>");
		}
	}

	private void writeAttribute(ObjectName name, String attributeName, Object attributeValue,
			MBeanAttributeInfo[] attributeInfos) throws IOException {
		write("<tr valign='top'><td>");
		writer.write("<a href='?jmxValue="
				+ name.toString().replace(" ", "%20").replace("'", "%27") + '.' + attributeName
				+ "' ");
		writeln("title=\"#Lien_valeur_mbeans#\">-</a>&nbsp;");
		writer.write(htmlEncode(attributeName));
		write("</td><td>");
		try {
			if (attributeValue instanceof List) {
				write("[");
				boolean first = true;
				for (final Object value : (List<?>) attributeValue) {
					if (first) {
						first = false;
					} else {
						write(",<br/>");
					}
					writer.write(htmlEncode(String.valueOf(value)));
				}
				write("]");
			} else {
				writer.write(htmlEncode(String.valueOf(attributeValue)));
			}
		} catch (final Exception e) {
			writer.write(htmlEncode(e.toString()));
		}
		write("</td><td>");
		final String attributeDescription = mbeans.getAttributeDescription(attributeName,
				attributeInfos);
		// les attributs des MBeans de java.lang ont des descriptions égales aux noms,
		// ce sont des descriptions inutiles
		if (attributeDescription != null && !attributeDescription.equals(attributeName)) {
			write("(");
			writer.write(htmlEncode(attributeDescription));
			write(")");
		}
		write("</td></tr>");
	}

	private void writeLinks() throws IOException {
		writeln("<div class='noPrint'>");
		writeln("<a href='javascript:history.back()'><img src='?resource=action_back.png' alt='#Retour#'/> #Retour#</a>");
		writeln("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
		writeln("<a href='?part=mbeans'><img src='?resource=action_refresh.png' alt='#Actualiser#'/> #Actualiser#</a>");
		writeln("</div>");
	}

	private void writeShowHideLink(String idToShow, String label) throws IOException {
		writeln("<a href=\"javascript:showHide('" + idToShow + "');\"><img id='" + idToShow
				+ "Img' src='?resource=bullets/plus.png' alt=''/> " + label + "</a>");
	}

	private static String htmlEncode(String text) {
		return I18N.htmlEncode(text, false);
	}

	private void write(String html) throws IOException {
		I18N.writeTo(html, writer);
	}

	private void writeln(String html) throws IOException {
		I18N.writelnTo(html, writer);
	}
}
