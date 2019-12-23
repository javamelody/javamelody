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
package net.bull.javamelody.internal.web.html;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

/**
 * Transformation de code source Java en HTML.
 * @author Emeric Vernat
 */
final class JavaHTMLizer {
	private static final String BR = "<br />\n";

	private static final List<Pattern> RESERVED_WORDS_PATTERNS = createReservedWordPatterns(
			Arrays.asList("class", "finally", "return", "new", "public", "static", "final", "void",
					"synchronized", "interface", "enum", "private", "protected", "import",
					"package", "try", "catch", "for", "while", "do", "if", "else", "switch", "case",
					"default", "goto", "byte", "short", "int", "long", "float", "double", "char",
					"boolean", "extends", "implements", "super", "this", "true", "false", "null",
					"abstract", "break", "continue", "assert", "instanceof", "native", "strictfp",
					"throws", "throw", "transient", "volatile"));

	private static final Map<Character, String> ESCAPE_MAPS = createEscapeMaps();

	private static final Pattern MULTILINE_COMMENT_PATTERN = Pattern.compile("/\\*(.*?)\\*/",
			Pattern.DOTALL);

	private static final Pattern SINGLELINE_COMMENT_PATTERN = Pattern.compile("//(.*?)<br />",
			Pattern.DOTALL);

	private static final Pattern STRING_PATTERN = Pattern.compile("&quot;(.*?)&quot;");

	private JavaHTMLizer() {
		super();
	}

	static String htmlize(final String javaSource) {
		String result = "-" + javaSource;
		result = htmlEscape(result);
		result = formatReservedWords(result);
		result = formatComments(result);
		result = formatStrings(result);
		return result.substring(1);
	}

	static String htmlizeFull(final String javaSource) {
		final String result = htmlize(javaSource);
		final String start = "<html><body><style>" + "code { font-size: 12px; } "
				+ "code .string { color: blue; } "
				+ "code .comment { font-style: italic; color: green; } "
				+ "code .keyword { font-weight: bold; color: purple; } "
				+ "code .comment .keyword { color: green; font-weight: normal; } "
				+ "code .comment .string { color: green; } " + "</style><code>";
		final String end = "</code></body></html>";
		return start + result + end;
	}

	static String addLineNumbers(final String javaSource) {
		final StringBuilder sb = new StringBuilder(javaSource);
		sb.insert(0, "<a name=1 href=#1>1</a> ");
		int line = 2;
		int index = sb.indexOf(BR);
		while (index != -1) {
			final int offset = index + BR.length();
			final String strLine = Integer.toString(line);
			sb.insert(offset, "</a> ");
			sb.insert(offset, strLine);
			sb.insert(offset, '>');
			sb.insert(offset, strLine);
			sb.insert(offset, " href=#");
			sb.insert(offset, strLine);
			sb.insert(offset, "<a name=");
			index = sb.indexOf(BR, index + 1);
			line++;
		}
		return sb.toString();
	}

	private static List<Pattern> createReservedWordPatterns(final List<String> reservedWords) {
		final List<Pattern> result = new ArrayList<Pattern>(reservedWords.size());
		for (final String reservedWord : reservedWords) {
			result.add(Pattern.compile("(\\W)(" + reservedWord + ")(\\W)"));
		}
		return result;
	}

	private static Map<Character, String> createEscapeMaps() {
		final Map<Character, String> escapeMaps = new LinkedHashMap<Character, String>();
		escapeMaps.put(' ', "&nbsp;");
		escapeMaps.put('\t', "&nbsp;&nbsp;&nbsp;&nbsp;");
		escapeMaps.put('<', "&lt;");
		escapeMaps.put('>', "&gt;");
		escapeMaps.put('\"', "&quot;");
		escapeMaps.put('&', "&amp;");
		escapeMaps.put('\'', "&#39;");
		escapeMaps.put('\n', BR);
		return escapeMaps;
	}

	private static String escapeChar(final char c) {
		return ESCAPE_MAPS.get(c);
	}

	private static String htmlEscape(final String text) {
		final StringBuilder sb = new StringBuilder();
		for (int i = 0; i < text.length(); i++) {
			final char c = text.charAt(i);
			final String escapedOrNull = escapeChar(c);
			if (escapedOrNull == null) {
				sb.append(c);
			} else {
				sb.append(escapedOrNull);
			}
		}
		return sb.toString();
	}

	private static String formatReservedWords(final String text) {
		String result = text;
		for (final Pattern reservedWordPattern : RESERVED_WORDS_PATTERNS) {
			result = reservedWordPattern.matcher(result)
					.replaceAll("$1<span class=\"keyword\">$2</span>$3");
		}
		return result;
	}

	private static String formatComments(final String text) {
		String result = text;
		result = MULTILINE_COMMENT_PATTERN.matcher(result)
				.replaceAll("<span class=\"comment\">/*$1*/</span>");
		result = SINGLELINE_COMMENT_PATTERN.matcher(result)
				.replaceAll("<span class=\"comment\">//$1</span><br />");
		return result;
	}

	private static String formatStrings(final String text) {
		return STRING_PATTERN.matcher(text)
				.replaceAll("<span class=\"string\">&quot;$1&quot;</span>");
	}
}
