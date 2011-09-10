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
package net.bull.javamelody.swing.util;

import java.awt.Component;
import java.awt.Cursor;
import java.awt.Window;

import javax.swing.RootPaneContainer;
import javax.swing.SwingUtilities;

/**
 * Petit utilitaire pour gérer le curseur de mise en attente en cas de traitement long.
 * @author Emeric Vernat
 */
public class MWaitCursor {
	private static final Cursor WAIT_CURSOR = Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR);
	private final Window window;
	private final Cursor oldWindowCursor;
	private final boolean windowGlassPaneVisible;

	/**
	 * Constructeur : crée le WaitCursor et affiche le sablier.
	 * @param comp Component
	 */
	public MWaitCursor(Component comp) {
		// Curseur de la frame contenant le component
		window = SwingUtilities.windowForComponent(comp);
		windowGlassPaneVisible = window instanceof RootPaneContainer
				&& ((RootPaneContainer) window).getGlassPane().isVisible();
		oldWindowCursor = window != null ? window.getCursor() : null;

		// On ne change pas le curseur du component car cela poserait problème en cas d'imbrication
		// pour le remettre à sa valeur initiale (Component.getCursor renvoyant le cursor de son parent si non défini)

		// On active le curseur d'attente
		// (l'utilisation du glassPane rend le curseur visible lors d'un double-clique sur une ligne par ex.
		// et l'utilisation de curseur de la window rend celui-ci visible même si on sort de la fenêtre pour revenir)
		if (window instanceof RootPaneContainer) {
			final Component glassPane = ((RootPaneContainer) window).getGlassPane();
			glassPane.setVisible(true);
			glassPane.setCursor(WAIT_CURSOR);
		}
		if (window != null) {
			window.setCursor(WAIT_CURSOR);
		}
	}

	/**
	 * Restore l'ancien curseur, en fin de traitement.
	 */
	public void restore() {
		if (window instanceof RootPaneContainer) {
			final Component glassPane = ((RootPaneContainer) window).getGlassPane();
			glassPane.setVisible(windowGlassPaneVisible);
			glassPane.setCursor(oldWindowCursor);
		}
		if (window != null) {
			window.setCursor(oldWindowCursor);
		}
	}
}
