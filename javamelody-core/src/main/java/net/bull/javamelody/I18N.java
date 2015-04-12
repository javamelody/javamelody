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
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.MessageFormat;
import java.util.Date;
import java.util.Locale;
import java.util.ResourceBundle;
import java.util.TimeZone;

/**
 * Classe de gestion des traductions et de l'internationalisation (formats de dates et de nombre).
 * La locale pour les rapports vient de la requête et est associée au thread courant.
 * @author Emeric Vernat
 */
final class I18N {
	// RESOURCE_BUNDLE_BASE_NAME vaut "net.bull.javamelody.resource.translations"
	// ce qui charge net.bull.javamelody.resource.translations.properties
	// et net.bull.javamelody.resource.translations_fr.properties
	// (Parameters.getResourcePath("translations") seul ne fonctionne pas si on est dans un jar/war)
	private static final String RESOURCE_BUNDLE_BASE_NAME = Parameters
			.getResourcePath("translations").replace('/', '.').substring(1);
	private static final ThreadLocal<Locale> LOCALE_CONTEXT = new ThreadLocal<Locale>();
	// Locale.ROOT needs 1.6
	private static final Locale ROOT_LOCALE = Locale.ROOT;

	private static final Locale FIXED_LOCALE = getFixedLocale();

	private I18N() {
		super();
	}

	/**
	 * Définit la locale (langue et formats dates et nombres) pour le thread courant.
	 * @param locale Locale
	 */
	static void bindLocale(Locale locale) {
		LOCALE_CONTEXT.set(locale);
	}

	/**
	 * Retourne la locale pour le thread courant ou la locale par défaut si elle n'a pas été définie.
	 * @return Locale
	 */
	static Locale getCurrentLocale() {
		if (FIXED_LOCALE != null) {
			return FIXED_LOCALE;
		}
		final Locale currentLocale = LOCALE_CONTEXT.get();
		if (currentLocale == null) {
			return Locale.getDefault();
		}
		return currentLocale;
	}

	/**
	 * Retourne les traductions pour la locale courante.
	 * @return Locale
	 */
	static ResourceBundle getResourceBundle() {
		final Locale currentLocale = getCurrentLocale();
		if (Locale.ENGLISH.getLanguage().equals(currentLocale.getLanguage())) {
			// there is no translations_en.properties because translations.properties is in English
			// but if user is English, do not let getBundle fallback on server's default locale
			return ResourceBundle.getBundle(RESOURCE_BUNDLE_BASE_NAME, ROOT_LOCALE);
		}
		// and if user is not English, use the bundle if it exists for his/her Locale
		// or the bundle for the server's default locale if it exists
		// or default (English) bundle otherwise
		return ResourceBundle.getBundle(RESOURCE_BUNDLE_BASE_NAME, currentLocale);
	}

	/**
	 * Enlève le lien entre la locale et le thread courant.
	 */
	static void unbindLocale() {
		LOCALE_CONTEXT.remove();
	}

	/**
	 * Retourne une traduction dans la locale courante.
	 * @param key clé d'un libellé dans les fichiers de traduction
	 * @return String
	 */
	static String getString(String key) {
		return getResourceBundle().getString(key);
	}

	/**
	 * Retourne une traduction dans la locale courante et l'encode pour affichage en javascript.
	 * @param key clé d'un libellé dans les fichiers de traduction
	 * @return String
	 */
	static String getStringForJavascript(String key) {
		final String string = getString(key);
		// ici, le résultat ne contient pas de valeur variable ni d'attaque puisque ce sont des messages internes et fixes,
		// donc pas besoin d'encoder avec javascriptEncode, et on conserve les apostrophes lisibles dans les messages
		return string.replace("\\", "\\\\").replace("\n", "\\n").replace("\"", "\\\"")
				.replace("'", "\\'");
	}

	/**
	 * Retourne une traduction dans la locale courante et insère les arguments aux positions {i}.
	 * @param key clé d'un libellé dans les fichiers de traduction
	 * @param arguments Valeur à inclure dans le résultat
	 * @return String
	 */
	static String getFormattedString(String key, Object... arguments) {
		// échappement des quotes qui sont des caractères spéciaux pour MessageFormat
		final String string = getString(key).replace("'", "''");
		return new MessageFormat(string, getCurrentLocale()).format(arguments);
	}

	/**
	 * Encode pour affichage en javascript.
	 * @param text message à encoder
	 * @return String
	 */
	static String javascriptEncode(String text) {
		return text.replace("\\", "\\\\").replace("\n", "\\n").replace("\"", "%22")
				.replace("'", "%27");
	}

	static String urlEncode(String text) {
		return javascriptEncode(text);
	}

	/**
	 * Encode pour affichage en html.
	 * @param text message à encoder
	 * @param encodeSpace booléen selon que les espaces sont encodés en nbsp (insécables)
	 * @return String
	 */
	static String htmlEncode(String text, boolean encodeSpace) {
		// ces encodages html sont incomplets mais suffisants pour le monitoring
		String result = text.replaceAll("[&]", "&amp;").replaceAll("[<]", "&lt;")
				.replaceAll("[>]", "&gt;").replaceAll("[\n]", "<br/>");
		if (encodeSpace) {
			result = result.replaceAll(" ", "&nbsp;");
		}
		return result;
	}

	/**
	 * Écrit un texte dans un flux en remplaçant dans le texte les clés entourées de deux '#'
	 * par leurs traductions dans la locale courante.
	 * @param html texte html avec éventuellement des #clé#
	 * @param writer flux
	 * @throws IOException e
	 */
	static void writeTo(String html, Writer writer) throws IOException {
		int index = html.indexOf('#');
		if (index == -1) {
			writer.write(html);
		} else {
			final ResourceBundle resourceBundle = getResourceBundle();
			int begin = 0;
			while (index != -1) {
				writer.write(html, begin, index - begin);
				final int nextIndex = html.indexOf('#', index + 1);
				final String key = html.substring(index + 1, nextIndex);
				writer.write(resourceBundle.getString(key));
				begin = nextIndex + 1;
				index = html.indexOf('#', begin);
			}
			writer.write(html, begin, html.length() - begin);
		}
	}

	/**
	 * Écrit un texte, puis un retour chariot, dans un flux en remplaçant dans le texte les clés entourées de deux '#'
	 * par leurs traductions dans la locale courante.
	 * @param html texte html avec éventuellement des #clé#
	 * @param writer flux
	 * @throws IOException e
	 */
	static void writelnTo(String html, Writer writer) throws IOException {
		writeTo(html, writer);
		writer.write('\n');
	}

	// méthodes utilitaires de formatage de dates et de nombres
	static DecimalFormat createIntegerFormat() {
		// attention ces instances de DecimalFormat ne doivent pas être statiques
		// car DecimalFormat n'est pas multi-thread-safe,
		return new DecimalFormat("#,##0", getDecimalFormatSymbols());
	}

	static DecimalFormat createPercentFormat() {
		return new DecimalFormat("0.00", getDecimalFormatSymbols());
	}

	private static DecimalFormatSymbols getDecimalFormatSymbols() {
		// optimisation mémoire (si Java 1.6)
		return DecimalFormatSymbols.getInstance(getCurrentLocale());
	}

	static DateFormat createDateFormat() {
		// attention ces instances de DateFormat ne doivent pas être statiques
		// car DateFormat n'est pas multi-thread-safe,
		// voir http://java.sun.com/javase/6/docs/api/java/text/DateFormat.html#synchronization
		return DateFormat.getDateInstance(DateFormat.SHORT, getCurrentLocale());
	}

	static DateFormat createDateAndTimeFormat() {
		return DateFormat.getDateTimeInstance(DateFormat.SHORT, DateFormat.SHORT,
				getCurrentLocale());
	}

	static DateFormat createDurationFormat() {
		// Locale.FRENCH et non getCurrentLocale() car pour une durée on veut
		// "00:01:02" (1min 02s) et non "12:01:02 AM"
		final DateFormat durationFormat = DateFormat.getTimeInstance(DateFormat.MEDIUM,
				Locale.FRENCH);
		// une durée ne dépend pas de l'horaire été/hiver du fuseau horaire de Paris
		durationFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
		return durationFormat;
	}

	static String getCurrentDate() {
		return createDateFormat().format(new Date());
	}

	static String getCurrentDateAndTime() {
		return createDateAndTimeFormat().format(new Date());
	}

	private static Locale getFixedLocale() {
		final String locale = Parameters.getParameter(Parameter.LOCALE);
		if (locale != null) {
			for (final Locale l : Locale.getAvailableLocales()) {
				if (l.toString().equals(locale)) {
					return l;
				}
			}
		}
		return null;
	}
}
