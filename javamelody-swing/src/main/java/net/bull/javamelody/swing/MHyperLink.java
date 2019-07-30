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
