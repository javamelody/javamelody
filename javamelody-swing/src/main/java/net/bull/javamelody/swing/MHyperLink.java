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

import java.awt.Color;
import java.awt.Cursor;
import java.awt.Desktop;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.net.URI;

import javax.swing.JLabel;

import net.bull.javamelody.swing.util.MSwingUtilities;

/**
 * Label de type lien hypertexte.
 * @author Emeric Vernat
 */
public class MHyperLink extends JLabel {
	private static final long serialVersionUID = 1L;

	private static final Color DARKER_BLUE = Color.BLUE.darker();
	private static final Cursor HAND_CURSOR = Cursor.getPredefinedCursor(Cursor.HAND_CURSOR);

	@SuppressWarnings("all")
	private static final MouseAdapter MOUSE_LISTENER = new MouseAdapter() {
		@Override
		public void mouseClicked(MouseEvent e) {
			final MHyperLink source = (MHyperLink) e.getSource();
			try {
				Desktop.getDesktop().browse(new URI(source.url));
			} catch (final Exception ex) {
				MSwingUtilities.showException(ex);
			}
		}
	};

	private final String url;

	/**
	 * Constructeur.
	 * @param text Texte à afficher
	 * @param url URL à ouvrir, par exemple une URL http
	 */
	public MHyperLink(String text, final String url) {
		super(text);
		this.url = url;
		setToolTipText(url);
		setForeground(DARKER_BLUE);
		setCursor(HAND_CURSOR);
		addMouseListener(MOUSE_LISTENER);
	}
}
