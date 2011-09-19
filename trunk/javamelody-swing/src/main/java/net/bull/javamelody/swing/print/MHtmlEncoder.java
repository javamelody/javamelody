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
package net.bull.javamelody.swing.print;

import java.util.Arrays;

/**
 * Encodage de chaines au format HTML.
 *
 * @author Emeric Vernat
 */
final class MHtmlEncoder {
	/**
	 * Liste des caracteres speciaux HTML triée. Remplie dans le init static
	 */
	private static final char[] TO_REPLACE;

	/**
	 * Liste des chaines remplacantes triée dans le même ordre que to_replace. Remplie dans le init static
	 */
	private static final char[][] REPLACE_BY;

	/**
	 * Initialisation de la classe. 1) tri de la liste de caracteres speciaux 2) generation des tables de remplacement
	 *
	 * @formatter:off
	 */
	static {
		// Mapping des caractères spéciaux vers leur code HTML
		// Respecter l'espace entre le caractere et son code.
		final String[] htmlCharacters = {
				"€ &euro;",
				"Œ &#140;",
				"œ &#156;", // latin-15

				// pas de nbsp (insécable à l'impression) "  &nbsp;",
				"\n <br/>", "\r ", "\t &nbsp;&nbsp;&nbsp;&nbsp;",

				"< &lt;", "& &amp;", "Æ &AElig;", "À &Agrave;", "Ä &Auml;", "É &Eacute;",
				"Ë &Euml;", "Ì &Igrave;", "Ó &Oacute;", "Ø &Oslash;", "Þ &THORN;", "Ù &Ugrave;",
				"á &aacute;", "à &agrave;", "ä &auml;", "ê &ecirc;", "ë &euml;", "ì &igrave;",
				"ó &oacute;", "ø &oslash;", "ß &szlig;", "û &ucirc;", "ý &yacute;", "¤ &curren;",
				"§ &sect;", "ª &ordf;", "­ &shy;", "° &deg;", "³ &sup3;", "¶ &para;", "¹ &sup1;",
				"¼ &frac14;", "¿ &iquest;", "× &times;", "÷ &divide;", "¢ &cent;", "> &gt;",
				"\" &quot;", "' &#39;", "` &#39;", "Á &Aacute;", "Å &Aring;", "Ç &Ccedil;",
				"Ê &Ecirc;", "Í &Iacute;", "Ï &Iuml;", "Ô &Ocirc;", "Õ &Otilde;", "Ú &Uacute;",
				"Ü &Uuml;", "â &acirc;", "å &aring;", "ç &ccedil;", "è &egrave;", "í &iacute;",
				"ï &iuml;", "ô &ocirc;", "õ &otilde;", "þ &thorn;", "ù &ugrave;", "ÿ &yuml;",
				"¡ &iexcl;", "¥ &yen;", "¨ &uml;", "« &laquo;", "® &reg;", "± &plusmn;",
				"´ &acute;", "· &middot;", "º &ordm;", "½ &frac12;", "Â &Acirc;", "Ã &Atilde;",
				"Ð &ETH;", "È &Egrave;", "Î &Icirc;", "Ñ &Ntilde;", "Ò &Ograve;", "Ö &Ouml;",
				"Û &Ucirc;", "Ý &Yacute;", "æ &aelig;", "ã &atilde;", "é &eacute;", "ð &eth;",
				"î &icirc;", "ñ &ntilde;", "ò &ograve;", "ö &ouml;", "ú &uacute;", "ü &uuml;",
				"£ &pound;", "¦ &brvbar;", "© &copy;", "¬ &not;", "¯ &macr;", "² &sup2;",
				"µ &micro;", "¸ &cedil;", "» &raquo;", "¾ &frac34;",

				"‘ &lsquo;", "’ &rsquo;", "‚ &sbquo;", "“ &ldquo;", "” &rdquo;", "„ &bdquo;",
				"† &dagger;", "‡ &Dagger;", "‰ &permil;", "‹ &lsaquo;", "› &rsaquo;", "™ &trade;",
				"– &ndash;", "— &mdash;", };

		// tri de la table des caracteres HTML
		Arrays.sort(htmlCharacters);
		final int length = htmlCharacters.length;
		TO_REPLACE = new char[length];
		REPLACE_BY = new char[length][];
		for (int i = 0; i < length; i++) {
			// premier caractere dans la table des carateres a remplacer
			TO_REPLACE[i] = htmlCharacters[i].charAt(0);
			// à partir du troisième caractere :
			// dans la table des codes remplacants
			REPLACE_BY[i] = htmlCharacters[i].substring(2).toCharArray();
		}
	}

	/**
	 * Constructeur.
	 */
	private MHtmlEncoder() {
		super();
	}

	/**
	 * Retourne une chaine encodée.
	 *
	 * @return String Chaine encodée
	 * @param s
	 *           String Chaine à encoder
	 */
	public static String encodeString(final String s) {
		if (s != null) {
			final StringBuilder sb = new StringBuilder(s.length() + s.length() / 4);
			encodeString(sb, s);
			return sb.toString();
		}
		return "";
	}

	/**
	 * Append une chaine à un StringBuilder apres l'avoir encodée. Plus la chaine à encoder est longue, plus les gains de perfs sont sensibles.
	 *
	 * @param sb
	 *           String StringBuilder à appender.
	 * @param s
	 *           String Chaine à encoder et à ajouter à <CODE>sb</CODE>
	 */
	public static void encodeString(final StringBuilder sb, final String s) {
		if (s == null) {
			return;
		}
		final int len = s.length();
		// réserve un peu plus de place dans le StringBuilder
		sb.ensureCapacity(sb.length() + len + len / 4);
		int i;
		int index;
		char c;
		for (i = 0; i < len; i++) {
			c = s.charAt(i);
			// petite optimisation (qui represente 90% des cas...)
			if (isSimpleLetterOrDigit(c)) {
				sb.append(c);
			} else {
				// cherche dans le tableau des caractères
				index = Arrays.binarySearch(TO_REPLACE, c);
				if (index >= 0) {
					// si trouvé, append la chaîne remplaçante
					sb.append(REPLACE_BY[index]);
				} else if (c < '\u0020' || c > '\u007e') {
					// si c'est un caractère bizarre non reconnu, on code son numéro décimal (en charset iso-8859-1)
					sb.append("&#").append(Integer.toString(c)).append(';');
				} else {
					// sinon append le caractère sans le modifier
					sb.append(c); // nécessite un charset système genre windows-1252
				}
			}
		}
	}

	private static boolean isSimpleLetterOrDigit(char c) {
		return 'a' <= c && c <= 'z' || 'A' <= c && c <= 'Z' || '0' <= c && c <= '9';
	}
}
