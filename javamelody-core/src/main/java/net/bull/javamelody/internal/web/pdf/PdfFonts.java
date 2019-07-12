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

import java.awt.Color;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Field;
import java.util.Locale;
import java.util.Properties;

import com.lowagie.text.DocumentException;
import com.lowagie.text.Font;
import com.lowagie.text.FontFactory;
import com.lowagie.text.pdf.BaseFont;

import net.bull.javamelody.internal.common.I18N;
import net.bull.javamelody.internal.common.InputOutput;
import net.bull.javamelody.internal.common.Parameters;

/**
 * Enumération des fontes pour les documents pdf.
 * @author Emeric Vernat
 */
enum PdfFonts {
	NORMAL(getFont(8f, Font.NORMAL)),
	BOLD(getFont(8f, Font.BOLD)),
	PARAGRAPH_TITLE(getFont(10f, Font.BOLD)),
	TABLE_CELL(getFont(5.5f, Font.NORMAL)),
	BOLD_CELL(getFont(5.5f, Font.BOLD)),
	BLUE(getFont(5.5f, Font.NORMAL)),
	INFO_CELL(getFont(5.5f, Font.NORMAL)),
	WARNING_CELL(getFont(5.5f, Font.BOLD)),
	SEVERE_CELL(getFont(5.5f, Font.BOLD)),
	TABLE_HEADER(getFont(5.5f, Font.BOLD));

	private static final String UKRAINIAN_LANGUAGE = "uk";
	private static final String CZECH_LANGUAGE = "cs";

	static {
		BLUE.font.setColor(Color.BLUE);
		INFO_CELL.font.setColor(Color.GREEN);
		WARNING_CELL.font.setColor(Color.ORANGE);
		SEVERE_CELL.font.setColor(Color.RED);
	}

	private static BaseFont chineseBaseFont;
	private static BaseFont dejaVuSansBaseFont;
	private final transient Font font;
	private transient Font chineseFont;
	private transient Font dejaVuSansFont;

	PdfFonts(Font font) {
		this.font = font;
	}

	Font getFont() {
		final String language = I18N.getResourceBundle().getLocale().getLanguage();
		if (Locale.CHINESE.getLanguage().equals(language)) {
			return getChineseFont();
		} else if (UKRAINIAN_LANGUAGE.equals(language) || CZECH_LANGUAGE.equals(language)) {
			return getDejaVuSansFont();
		}
		return font;
	}

	private static Font getFont(float size, int style) {
		return FontFactory.getFont(FontFactory.HELVETICA, size, style);
	}

	private Font getChineseFont() {
		if (chineseFont == null) {
			final BaseFont bfChinese = getChineseBaseFont();
			chineseFont = new Font(bfChinese, font.getSize(), font.getStyle());
		}
		return chineseFont;
	}

	private static synchronized BaseFont getChineseBaseFont() {
		if (chineseBaseFont == null) {
			try {
				try {
					chineseBaseFont = BaseFont.createFont("STSong-Light", "UniGB-UCS2-H",
							BaseFont.NOT_EMBEDDED);
				} catch (final DocumentException e) {
					// now CJKFont.propertiesLoaded==true, load properties renamed (cf issue 258)
					loadCJKFonts();
					chineseBaseFont = BaseFont.createFont("STSong-Light", "UniGB-UCS2-H",
							BaseFont.NOT_EMBEDDED);
				}
			} catch (final DocumentException e) {
				throw new IllegalStateException(e);
			} catch (final IOException e) {
				throw new IllegalStateException(e);
			}
		}
		return chineseBaseFont;
	}

	/**
	 * Chargement des cjkfonts et cjkencodings. <br/>
	 * Les fichiers cjkfonts.properties et cjkencoding.properties ont été renommés <br/>
	 * pour que FlyingSaucer ne croit pas qu'iTextAsian.jar est présent (issue 258). <br/>
	 * Cette méthode se charge de charger quand même les fichiers renommés pour la fonte en langue chinoise.
	 */
	private static void loadCJKFonts() {
		try {
			final Class<?> cjkFontClass = Class.forName("com.lowagie.text.pdf.CJKFont");
			final Field cjkFontsField = cjkFontClass.getDeclaredField("cjkFonts");
			final Field cjkEncodingsField = cjkFontClass.getDeclaredField("cjkEncodings");
			cjkFontsField.setAccessible(true);
			cjkEncodingsField.setAccessible(true);
			final Properties cjkFonts = (Properties) cjkFontsField.get(null);
			final Properties cjkEncodings = (Properties) cjkEncodingsField.get(null);

			if (cjkFonts.isEmpty()) {
				final InputStream is = BaseFont.getResourceStream(
						BaseFont.RESOURCE_PATH + "cjkfonts.properties.renamedForIssue258");
				try {
					cjkFonts.load(is);
				} finally {
					is.close();
				}
			}
			if (cjkEncodings.isEmpty()) {
				final InputStream is = BaseFont.getResourceStream(
						BaseFont.RESOURCE_PATH + "cjkencodings.properties.renamedForIssue258");
				try {
					cjkEncodings.load(is);
				} finally {
					is.close();
				}
			}
		} catch (final Exception e) {
			throw new IllegalStateException(e);
		}
	}

	private Font getDejaVuSansFont() {
		if (dejaVuSansFont == null) {
			final BaseFont bfDejaVuSans = getDejaVuSansBaseFont();
			dejaVuSansFont = new Font(bfDejaVuSans, font.getSize(), font.getStyle());
		}
		return dejaVuSansFont;
	}

	private static synchronized BaseFont getDejaVuSansBaseFont() {
		if (dejaVuSansBaseFont == null) {
			final InputStream input = PdfFonts.class.getResourceAsStream("/DejaVuSans-Bold.ttf");
			// input may be null if the jrobin jar file is not the 1.5.9 from Maven central or ...
			if (input != null) {
				try {
					try {
						final File file = new File(Parameters.TEMPORARY_DIRECTORY,
								"javamelody-ukrainian.ttf");
						InputOutput.pumpToFile(input, file);
						dejaVuSansBaseFont = BaseFont.createFont(file.getPath(),
								BaseFont.IDENTITY_H, BaseFont.EMBEDDED);
					} finally {
						input.close();
					}
				} catch (final IOException e) {
					throw new IllegalStateException(e);
				} catch (final DocumentException e) {
					throw new IllegalStateException(e);
				}
			}
		}
		return dejaVuSansBaseFont;
	}

	static boolean shouldUseEnglishInsteadOfUkrainian() {
		return UKRAINIAN_LANGUAGE.equals(I18N.getResourceBundle().getLocale().getLanguage())
				&& getDejaVuSansBaseFont() == null;
	}
}
