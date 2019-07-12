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
package net.bull.javamelody.swing;

import java.awt.Toolkit;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.Locale;

import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.PlainDocument;

/**
 * Document swing interne pour la saisie d'une date.
 * @author Emeric Vernat
 */
public class MDateDocument extends PlainDocument {
	private static final long serialVersionUID = 1L;

	// Les instances de formats suivants sont partagées pour tous les champs de date.
	// Note : Ce n'est pas gênant même pour un DateFormat car les traitements Swing se font toujours sur un seul thread.

	// format de base et 1er format pour la validation
	private static SimpleDateFormat dateFormat;

	// format alternatif de validation (sans les '/', puis juste dd et MM, puis juste dd)
	private static SimpleDateFormat alternateDateFormat;

	// format d'affichage (avec 4 chiffres pour l'année)
	private static SimpleDateFormat displayDateFormat;

	/**
	 * Retourne la valeur de la propriété alternateDateFormat.
	 * @return SimpleDateFormat
	 */
	public static SimpleDateFormat getAlternateDateFormat() {
		if (alternateDateFormat == null) { // NOPMD
			// pour construire ce dateFormat on enlève les '/' et '.' du pattern de dateFormat
			// ie on peut saisir en France 251202 au lieu de 25/12/02 ou 25/12/2002

			final StringBuilder patternSb = new StringBuilder(getDateFormat().toPattern());
			// note : il faut réévaluer pattern.length() chaque fois dans la condition
			// puisque la longueur diminue au fur et à mesure
			for (int i = 0; i < patternSb.length(); i++) {
				if (!Character.isLetter(patternSb.charAt(i))) {
					patternSb.deleteCharAt(i);
				}
			}

			final Locale locale = Locale.getDefault();
			final String pattern = patternSb.toString();
			final String pattern2 = pattern.replaceAll("y", "");
			final String pattern3 = pattern2.replaceAll("M", "");
			final SimpleDateFormat myAlternateDateFormat2 = new SimpleDateFormat(pattern2, locale);
			final SimpleDateFormat myAlternateDateFormat3 = new SimpleDateFormat(pattern3, locale);

			// CHECKSTYLE:OFF
			final SimpleDateFormat myAlternateDateFormat = new SimpleDateFormat(pattern, locale) {
				// CHECKSTYLE:ON
				private static final long serialVersionUID = 1L;

				@Override
				public Date parse(final String text) throws ParseException {
					try {
						return super.parse(text);
					} catch (final ParseException ex) {
						final Calendar myCalendar = Calendar.getInstance();
						final int year = myCalendar.get(Calendar.YEAR);
						final int month = myCalendar.get(Calendar.MONTH);
						try {
							myCalendar.setTime(myAlternateDateFormat2.parse(text));
							myCalendar.set(Calendar.YEAR, year);
							return myCalendar.getTime();
						} catch (final ParseException ex1) {
							try {
								myCalendar.setTime(myAlternateDateFormat3.parse(text));
								myCalendar.set(Calendar.YEAR, year);
								myCalendar.set(Calendar.MONTH, month);
								return myCalendar.getTime();
							} catch (final ParseException ex2) {
								throw ex;
							}
						}
					}
				}
			};

			// on n'accepte pas le 30/02 (qui serait alors le 02/03)
			myAlternateDateFormat.setLenient(false);
			myAlternateDateFormat2.setLenient(false);
			myAlternateDateFormat3.setLenient(false);

			alternateDateFormat = myAlternateDateFormat; // NOPMD
		}

		return alternateDateFormat; // NOPMD
	}

	/**
	 * Retourne la valeur de la propriété dateFormat. <br/>
	 * Ce dateFormat est le format court par défaut de java en fonction de la Locale par défaut (lenient est false).
	 * @return SimpleDateFormat
	 * @see #setDateFormat
	 */
	public static SimpleDateFormat getDateFormat() {
		if (dateFormat == null) { // NOPMD
			// ce dateFormat est bien une instance de SimpleDateFormat selon le source DateFormat.get
			final SimpleDateFormat myDateFormat = (SimpleDateFormat) DateFormat
					.getDateInstance(DateFormat.SHORT);
			// on n'accepte pas le 30/02 (qui serait alors le 02/03)
			myDateFormat.setLenient(false);
			dateFormat = myDateFormat; // NOPMD
		}

		return dateFormat; // NOPMD
	}

	/**
	 * Retourne la valeur de la propriété displayDateFormat.
	 * @return SimpleDateFormat
	 */
	public static SimpleDateFormat getDisplayDateFormat() {
		if (displayDateFormat == null) { // NOPMD
			String pattern = getDateFormat().toPattern();
			// si le pattern contient seulement 2 chiffres pour l'année on en met 4
			// parce que c'est plus clair à l'affichage
			// mais en saisie on peut toujours n'en mettre que deux qui seront alors interprétés (avec le siècle)
			if (pattern.indexOf("yy") != -1 && pattern.indexOf("yyyy") == -1) {
				pattern = new StringBuilder(pattern).insert(pattern.indexOf("yy"), "yy").toString();
			}

			displayDateFormat = new SimpleDateFormat(pattern, Locale.getDefault()); // NOPMD
		}

		return displayDateFormat; // NOPMD
	}

	/** {@inheritDoc} */
	@Override
	public void insertString(final int offset, final String string, final AttributeSet attributeSet)
			throws BadLocationException {
		if (string == null || string.length() == 0) {
			return;
		}

		// pour une date on n'accepte que les chiffres et '/' (et/ou '.', ' ', ':' selon pattern)
		char c;
		final String pattern = getDateFormat().toPattern();
		final int stringLength = string.length();
		for (int i = 0; i < stringLength; i++) {
			c = string.charAt(i);
			if (!Character.isDigit(c) && (Character.isLetter(c) || pattern.indexOf(c) == -1)) {
				Toolkit.getDefaultToolkit().beep();
				return;
			}
		}

		// mais sinon on accepte tout, même un format incomplet
		super.insertString(offset, string, attributeSet);
	}

	/**
	 * Définit la valeur de la propriété alternateDateFormat.
	 * @param newAlternateDateFormat
	 *           SimpleDateFormat
	 * @see #getAlternateDateFormat
	 */
	public static void setAlternateDateFormat(final SimpleDateFormat newAlternateDateFormat) {
		alternateDateFormat = newAlternateDateFormat; // NOPMD
	}

	/**
	 * Définit la valeur de la propriété dateFormat (et réinitialise alternateDateFormat et displayDateFormat). <br/>
	 * Il est possible notamment de fournir un format acceptant plusieurs syntaxes ou lançant une RuntimeException "Date invalide" pour l'afficher en rouge si la date n'est pas entre MDateField.getMinimumValue() et MDateField.getMaximumValue().
	 * @param newDateFormat
	 *           SimpleDateFormat
	 * @see #getDateFormat
	 */
	public static void setDateFormat(final SimpleDateFormat newDateFormat) {
		dateFormat = newDateFormat; // NOPMD
		// force la reconstruction des autres formats
		setAlternateDateFormat(null);
		setDisplayDateFormat(null);
	}

	/**
	 * Définit la valeur de la propriété displayDateFormat.
	 * @param newDisplayDateFormat
	 *           SimpleDateFormat
	 * @see #getDisplayDateFormat
	 */
	public static void setDisplayDateFormat(final SimpleDateFormat newDisplayDateFormat) {
		displayDateFormat = newDisplayDateFormat; // NOPMD
	}
}
