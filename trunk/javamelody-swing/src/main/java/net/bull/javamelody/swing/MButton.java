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

import java.awt.event.ActionEvent;

import javax.swing.Icon;
import javax.swing.JButton;

import net.bull.javamelody.swing.util.MSwingUtilities;
import net.bull.javamelody.swing.util.MWaitCursor;

/**
 * Bouton.
 * @author Emeric Vernat
 */
public class MButton extends JButton {
	private static final long serialVersionUID = 1L;

	/**
	 * Constructeur.
	 */
	public MButton() {
		super();
	}

	/**
	 * Constructeur.
	 * @param text String
	 */
	public MButton(String text) {
		super(text);
	}

	/**
	 * Constructeur.
	 * @param text String
	 * @param icon Icon
	 */
	public MButton(String text, Icon icon) {
		super(text, icon);
	}

	/**
	 * Méthode interne pour notifier tous les listeners qui ont enregistré leur intérêt
	 * par button.addActionListener pour les événements d'action sur ce bouton.
	 *
	 * La surcharge de cette méthode permet pour les listeners d'actions sur des MButton
	 * que le curseur sablier est automatiquement affiché.
	 * @param event ActionEvent
	 */
	@Override
	protected void fireActionPerformed(ActionEvent event) {
		try {
			final MWaitCursor waitCursor = new MWaitCursor(this);
			try {
				super.fireActionPerformed(event);
			} finally {
				waitCursor.restore();
			}
		} catch (final Throwable t) { // NOPMD
			MSwingUtilities.showException(t);
		}
	}
}
