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
package net.bull.javamelody.internal.web.pdf;

import java.io.IOException;

import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;

import net.bull.javamelody.internal.common.I18N;

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
