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
 * Parent abstrait des classes de rapport html.
 * @author Emeric Vernat
 */
abstract class HtmlAbstractReport {
	private static final boolean PDF_ENABLED = computePdfEnabled();
	private final Writer writer;

	class HtmlTable {
		private boolean firstRow = true;
		private boolean oddRow;

		void beginTable(String summary) throws IOException {
			writeDirectly("<table class='sortable' width='100%' border='1' cellspacing='0' cellpadding='2' summary='");
			writeDirectly(summary);
			writeDirectly("'>\n");
			writeDirectly("<thead><tr>");
		}

		void nextRow() throws IOException {
			writeDirectly("</tr>");
			if (firstRow) {
				firstRow = false;
				writeDirectly("</thead><tbody>\n");
			}
			if (oddRow) {
				writeDirectly("<tr class='odd' onmouseover=\"this.className='highlight'\" onmouseout=\"this.className='odd'\">\n");
			} else {
				writeDirectly("<tr onmouseover=\"this.className='highlight'\" onmouseout=\"this.className=''\">\n");
			}
			oddRow = !oddRow; // NOPMD
		}

		void endTable() throws IOException {
			writeDirectly("</tr>");
			if (firstRow) {
				firstRow = false;
				writeDirectly("</thead><tbody>\n");
			}
			writeDirectly("</tbody></table>\n");
		}
	}

	HtmlAbstractReport(Writer writer) {
		super();
		assert writer != null;
		this.writer = writer;
	}

	/**
	 * Perform the default html rendering of the report into the writer.
	 * @throws IOException e
	 */
	abstract void toHtml() throws IOException;

	Writer getWriter() {
		return writer;
	}

	void writeDirectly(String html) throws IOException {
		writer.write(html);
	}

	/**
	 * Écrit un texte dans un flux en remplaçant dans le texte les clés entourées de deux '#'
	 * par leurs traductions dans la locale courante.
	 * @param html texte html avec éventuellement des #clé#
	 * @throws IOException e
	 */
	void write(String html) throws IOException {
		I18N.writeTo(html, writer);
	}

	/**
	 * Écrit un texte, puis un retour chariot, dans un flux en remplaçant dans le texte les clés entourées de deux '#'
	 * par leurs traductions dans la locale courante.
	 * @param html texte html avec éventuellement des #clé#
	 * @throws IOException e
	 */
	void writeln(String html) throws IOException {
		I18N.writelnTo(html, writer);
	}

	void writeTitle(String imageFileName, String title) throws IOException {
		writeDirectly("<img src='?resource=");
		writeDirectly(imageFileName);
		writeDirectly("' width='24' height='24' alt=\"");
		writeDirectly(title);
		writeDirectly("\" />&nbsp;");
		writeDirectly("<b>");
		writeDirectly(title);
		writeDirectly("</b><br/>\n");
	}

	void writeShowHideLink(String idToShow, String label) throws IOException {
		writeln("<a href=\"javascript:showHide('" + idToShow + "');\" class='noPrint'><img id='"
				+ idToShow + "Img' src='?resource=bullets/plus.png' alt=''/> " + label + "</a>");
	}

	/**
	 * Retourne une traduction dans la locale courante.
	 * @param key clé d'un libellé dans les fichiers de traduction
	 * @return String
	 */
	static String getString(String key) {
		return I18N.getString(key);
	}

	/**
	 * Retourne une traduction dans la locale courante et l'encode pour affichage en javascript.
	 * @param key clé d'un libellé dans les fichiers de traduction
	 * @return String
	 */
	static String getStringForJavascript(String key) {
		return I18N.getStringForJavascript(key);
	}

	/**
	 * Retourne une traduction dans la locale courante et insère les arguments aux positions {i}.
	 * @param key clé d'un libellé dans les fichiers de traduction
	 * @param arguments Valeur à inclure dans le résultat
	 * @return String
	 */
	static String getFormattedString(String key, Object... arguments) {
		return I18N.getFormattedString(key, arguments);
	}

	/**
	 * Encode pour affichage en javascript.
	 * @param text message à encoder
	 * @return String
	 */
	static String javascriptEncode(String text) {
		return I18N.javascriptEncode(text);
	}

	static String urlEncode(String text) {
		return I18N.urlEncode(text);
	}

	/**
	 * Encode pour affichage en html, en encodant les espaces en nbsp (insécables).
	 * @param text message à encoder
	 * @return String
	 */
	static String htmlEncode(String text) {
		return I18N.htmlEncode(text, true);
	}

	/**
	 * Encode pour affichage en html, sans encoder les espaces en nbsp (insécables).
	 * @param text message à encoder
	 * @return String
	 */
	static String htmlEncodeButNotSpace(String text) {
		return I18N.htmlEncode(text, false);
	}

	static boolean isPdfEnabled() {
		return PDF_ENABLED;
	}

	private static boolean computePdfEnabled() {
		try {
			Class.forName("com.lowagie.text.Document");
			return true;
		} catch (final ClassNotFoundException e) {
			return false;
		}
	}
}
