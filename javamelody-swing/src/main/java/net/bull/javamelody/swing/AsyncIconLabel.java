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

import java.net.URL;

import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.SwingWorker;

import net.bull.javamelody.swing.util.MSwingUtilities;

/**
 * Label pour afficher une icône dont le chargement est asynchrone.
 * @author Emeric Vernat
 */
public class AsyncIconLabel extends JLabel {
	private static final long serialVersionUID = 1L;

	// sablier d'attente animé comme firefox
	private static final ImageIcon THROBBER_ICON = new ImageIcon(
			AsyncIconLabel.class.getResource("/icons/throbber.gif"));

	private URL iconUrl;

	/**
	 * Constructeur.
	 */
	public AsyncIconLabel() {
		super();
	}

	/**
	 * Constructeur.
	 * @param iconURL URL d'une icône
	 */
	public AsyncIconLabel(URL iconURL) {
		super();
		setIconURL(iconURL); // NOPMD
	}

	/**
	 * @return URL d'une icône
	 */
	public URL getIconURL() {
		return iconUrl;
	}

	/**
	 * @param url URL d'une icône
	 */
	public void setIconURL(final URL url) {
		this.iconUrl = url;
		if (iconUrl == null) {
			setIcon(null);
			setToolTipText(null);
		} else {
			setIcon(THROBBER_ICON);
			setToolTipText(url.toString());

			final SwingWorker<ImageIcon, Object> swingWorker = new SwingWorker<ImageIcon, Object>() {
				@Override
				protected ImageIcon doInBackground() throws Exception { // NOPMD
					// cette méthode est appelée dans un thread asynchrone hors de l'EDT
					return new ImageIcon(url);
				}

				@Override
				protected void done() {
					// cette méthode est appelée dans le thread de l'EDT après doInBackground(),
					// c'est dans cette méthode que l'on peut modifier le label
					try {
						setIcon(get()); // setIcon appelle déjà revalidate() et repaint()
						setToolTipText(null);
					} catch (final Exception e) {
						MSwingUtilities.showException(e);
					}
				}
			};
			swingWorker.execute();
		}
	}
}
