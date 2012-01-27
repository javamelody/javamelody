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

import java.awt.Color;
import java.io.IOException;
import java.util.Locale;

import com.lowagie.text.DocumentException;
import com.lowagie.text.Font;
import com.lowagie.text.FontFactory;
import com.lowagie.text.pdf.BaseFont;

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

	static {
		BLUE.font.setColor(Color.BLUE);
		INFO_CELL.font.setColor(Color.GREEN);
		WARNING_CELL.font.setColor(Color.ORANGE);
		SEVERE_CELL.font.setColor(Color.RED);
	}

	@SuppressWarnings("all")
	private final Font font;
	@SuppressWarnings("all")
	private Font chineseFont;

	PdfFonts(Font font) {
		this.font = font;
	}

	Font getFont() {
		if (Locale.CHINESE.getLanguage().equals(I18N.getResourceBundle().getLocale().getLanguage())) {
			return getChineseFont();
		}
		return font;
	}

	private Font getChineseFont() {
		if (chineseFont == null) {
			try {
				final BaseFont bfChinese = BaseFont.createFont("STSong-Light", "UniGB-UCS2-H",
						BaseFont.NOT_EMBEDDED);
				chineseFont = new Font(bfChinese, font.getSize(), font.getStyle());
			} catch (final DocumentException e) {
				throw new IllegalStateException(e);
			} catch (final IOException e) {
				throw new IllegalStateException(e);
			}
		}
		return chineseFont;
	}

	private static Font getFont(float size, int style) {
		return FontFactory.getFont(FontFactory.HELVETICA, size, style);
	}
}
