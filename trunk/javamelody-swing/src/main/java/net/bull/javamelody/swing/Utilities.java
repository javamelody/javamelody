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
package net.bull.javamelody.swing;

import java.awt.Dimension;
import java.awt.Font;

import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingUtilities;

import net.bull.javamelody.ImageIconCache;
import net.bull.javamelody.util.MSwingUtilities;

/**
 * Classe utilitaire.
 * @author Emeric Vernat
 */
public final class Utilities {
	private Utilities() {
		super();
	}

	/**
	 * Création d'un JLabel de paragraphe.
	 * @param title String
	 * @param iconName String
	 * @return JLabel
	 */
	public static JLabel createParagraphTitle(String title, String iconName) {
		final JLabel label = new JLabel(title);
		label.setIcon(ImageIconCache.getScaledImageIcon(iconName, 24, 24));
		label.setFont(label.getFont().deriveFont(Font.BOLD, label.getFont().getSize() + 4));
		// séparateur avec composants au-dessus et en-dessous
		label.setBorder(BorderFactory.createEmptyBorder(10, 0, 10, 0));
		return label;
	}

	/**
	 * Fixe la taille exacte d'une JTable à celle nécessaire pour afficher les données.
	 * @param table JTable
	 */
	public static void adjustTableHeight(final JTable table) {
		table.setPreferredScrollableViewportSize(new Dimension(-1, table.getPreferredSize().height));
		SwingUtilities.invokeLater(new Runnable() {
			@Override
			public void run() {
				final JScrollPane scrollPane = MSwingUtilities.getAncestorOfClass(
						JScrollPane.class, table);
				scrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER);
			}
		});
	}
}
