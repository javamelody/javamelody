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
