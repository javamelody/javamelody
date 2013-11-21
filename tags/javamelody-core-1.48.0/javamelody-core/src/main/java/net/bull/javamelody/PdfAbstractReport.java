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

import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;

/**
 * Parent abstrait des classes de rapport pdf.
 * @author Emeric Vernat
 */
abstract class PdfAbstractReport {
	private final Document document;

	PdfAbstractReport(Document document) {
		super();
		assert document != null;
		this.document = document;
	}

	/**
	 * Perform the default pdf rendering of the report into the document.
	 * @throws DocumentException e
	 * @throws IOException e
	 */
	abstract void toPdf() throws DocumentException, IOException;

	Document getDocument() {
		return document;
	}

	/**
	 * Adds an <CODE>Element</CODE> to the <CODE>Document</CODE>.
	 *
	 * @param element
	 *            the <CODE>Element</CODE> to add
	 * @return <CODE>true</CODE> if the element was added, <CODE>false
	 *         </CODE> if not
	 * @throws DocumentException
	 *             when a document isn't open yet, or has been closed
	 */
	boolean addToDocument(Element element) throws DocumentException {
		return document.add(element);
	}

	/**
	 * Signals that an new page has to be started.
	 *
	 * @return <CODE>true</CODE> if the page was added, <CODE>false</CODE>
	 *         if not.
	 */
	boolean newPage() {
		return document.newPage();
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
	 * Retourne une traduction dans la locale courante et insère les arguments aux positions {i}.
	 * @param key clé d'un libellé dans les fichiers de traduction
	 * @param arguments Valeur à inclure dans le résultat
	 * @return String
	 */
	static String getFormattedString(String key, Object... arguments) {
		return I18N.getFormattedString(key, arguments);
	}
}
