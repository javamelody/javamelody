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
