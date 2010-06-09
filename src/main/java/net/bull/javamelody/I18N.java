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
	private static final String RESOURCE_BUNDLE_BASE_NAME = Parameters.getResourcePath(
			"translations").replace('/', '.').substring(1);
	private static final ThreadLocal<Locale> LOCALE_CONTEXT = new ThreadLocal<Locale>();
	private static final boolean JAVA_16 = "1.6".compareTo(Parameters.JAVA_VERSION) < 0;

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
	private static ResourceBundle getResourceBundle() {
		return ResourceBundle.getBundle(RESOURCE_BUNDLE_BASE_NAME, getCurrentLocale());
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
		return javascriptEncode(getString(key));
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
		return text.replace("\n", "\\n").replace("\"", "\\\"").replace("'", "\\'");
	}

	/**
	 * Encode pour affichage en html.
	 * @param text message à encoder
	 * @param encodeSpace booléen selon que les espaces sont encodés en nbsp (insécables)
	 * @return String
	 */
	static String htmlEncode(String text, boolean encodeSpace) {
		// ces encodages html sont incomplets mais suffisants pour le monitoring
		String result = text.replaceAll("[&]", "&amp;").replaceAll("[<]", "&lt;").replaceAll("[>]",
				"&gt;").replaceAll("[\n]", "<br/>");
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
			final ResourceBundle resourceBundle2 = getResourceBundle();
			int begin = 0;
			while (index != -1) {
				writer.write(html, begin, index - begin);
				final int nextIndex = html.indexOf('#', index + 1);
				final String key = html.substring(index + 1, nextIndex);
				writer.write(resourceBundle2.getString(key));
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
		if (JAVA_16) {
			// optimisation mémoire si Java 1.6
			return DecimalFormatSymbols.getInstance(getCurrentLocale());
		}
		return new DecimalFormatSymbols(getCurrentLocale());
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
}
