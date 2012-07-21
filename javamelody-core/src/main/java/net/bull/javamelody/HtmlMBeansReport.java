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
import java.util.Map;

import javax.management.JMException;
import javax.management.MBeanServer;
import javax.management.ObjectName;

import net.bull.javamelody.MBean.MBeanAttribute;

/**
 * Partie du rapport html pour les MBeans.
 * @author Emeric Vernat
 */
class HtmlMBeansReport {
	private static final boolean PDF_ENABLED = HtmlCoreReport.isPdfEnabled();
	private static final String BR = "<br/>";
	private MBeans mbeans;
	private final Writer writer;
	private final String pid = PID.getPID();
	private int sequence;

	HtmlMBeansReport(Writer writer) {
		super();
		assert writer != null;
		this.writer = writer;
		// MBeans pour la plateforme
		setMBeans(new MBeans());
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
		// MBeans pour la plateforme
		writeTreeForCurrentMBeans();

		// pour JBoss 5.0.x, les MBeans de JBoss sont dans un autre MBeanServer
		final MBeanServer plateformMBeanServer = MBeans.getPlatformMBeanServer();
		for (final MBeanServer mbeanServer : MBeans.getMBeanServers()) {
			if (mbeanServer != plateformMBeanServer) {
				setMBeans(new MBeans(mbeanServer));
				writeln(BR);
				writer.write("<b>" + htmlEncode(mbeanServer.getDefaultDomain()) + "</b>");
				writeTreeForCurrentMBeans();
			}
		}
	}

	private void writeTreeForCurrentMBeans() throws IOException, JMException {
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
					if (firstMBean) {
						firstMBean = false;
					} else {
						write(BR);
					}
					final MBean mbean = mbeans.getMBean(name);
					writeMBean(mbean);
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

	private void writeMBean(MBean mbean) throws IOException {
		String mbeanName = mbean.getName();
		final String mbeanId = getNextId();
		final int indexOfComma = mbeanName.indexOf(',');
		if (indexOfComma != -1) {
			mbeanName = mbeanName.substring(indexOfComma + 1);
			writeShowHideLink(mbeanId, htmlEncode(mbeanName));
			writeln("<div id='" + mbeanId + "' style='display: none; margin-left: 20px;'>");
			// pas besoin d'ajouter un div pour le scroll-down, car les attributs sont
			// dans une table
			writeAttributes(mbean);
			writeln("</div>");
		} else {
			writeAttributes(mbean);
		}
	}

	private void writeAttributes(MBean mbean) throws IOException {
		final String description = mbean.getDescription();
		final List<MBeanAttribute> attributes = mbean.getAttributes();
		if (description != null || !attributes.isEmpty()) {
			writeln("<table border='0' cellspacing='0' cellpadding='3' summary=''>");
			if (description != null) {
				write("<tr><td colspan='3'>(");
				writer.write(htmlEncode(description));
				write(")</td></tr>");
			}
			for (final MBeanAttribute attribute : attributes) {
				writeAttribute(mbean, attribute);
			}
			writeln("</table>");
		}
	}

	private void writeAttribute(MBean mbean, MBeanAttribute attribute) throws IOException {
		final String attributeName = attribute.getName();
		final String formattedValue = attribute.getFormattedValue();
		final String description = attribute.getDescription();
		write("<tr valign='top'><td>");
		writer.write("<a href='?jmxValue="
				+ mbean.getName().replace(" ", "%20").replace("'", "%27") + '.' + attributeName
				+ "' ");
		writeln("title=\"#Lien_valeur_mbeans#\">-</a>&nbsp;");
		writer.write(htmlEncode(attributeName));
		write("</td><td>");
		// \n sera encodé dans <br/> dans htmlEncode
		writer.write(htmlEncode(formattedValue));
		write("</td><td>");
		if (description != null) {
			write("(");
			writer.write(htmlEncode(description));
			write(")");
		} else {
			write("&nbsp;");
		}
		write("</td></tr>");
	}

	private void writeLinks() throws IOException {
		writeln("<div class='noPrint'>");
		writeln("<a href='javascript:history.back()'><img src='?resource=action_back.png' alt='#Retour#'/> #Retour#</a>");
		writeln("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
		writeln("<a href='?part=mbeans'><img src='?resource=action_refresh.png' alt='#Actualiser#'/> #Actualiser#</a>");
		if (PDF_ENABLED) {
			writeln("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
			write("<a href='?part=mbeans&amp;format=pdf' title='#afficher_PDF#'>");
			write("<img src='?resource=pdf.png' alt='#PDF#'/> #PDF#</a>");
		}
		writeln("</div>");
	}

	private void writeShowHideLink(String idToShow, String label) throws IOException {
		writer.write("<a href=\"javascript:showHide('" + idToShow + "');\"><img id='" + idToShow
				+ "Img' src='?resource=bullets/plus.png' alt=''/> " + label + "</a>");
	}

	private void setMBeans(MBeans mBeans) {
		this.mbeans = mBeans;
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
