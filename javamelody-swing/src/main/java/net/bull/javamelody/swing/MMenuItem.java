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
import javax.swing.JMenuItem;

import net.bull.javamelody.swing.util.MSwingUtilities;
import net.bull.javamelody.swing.util.MWaitCursor;

/**
 * MenuItem.
 *
 * @author Emeric Vernat
 */
public class MMenuItem extends JMenuItem {
	private static final long serialVersionUID = 1L;

	/**
	 * Constructeur.
	 */
	public MMenuItem() {
		this(null);
	}

	/**
	 * Constructeur.
	 *
	 * @param text
	 *           String
	 */
	public MMenuItem(final String text) {
		super(text);
	}

	/**
	 * Constructeur.
	 *
	 * @param text
	 *           String
	 * @param icon
	 *           Icon
	 */
	public MMenuItem(final String text, final Icon icon) {
		super(text, icon);
	}

	/**
	 * Méthode interne pour notifier tous les listeners qui ont enregistré leur intérêt par menuItem.addActionListener pour les évènements d'action sur cet item.
	 *
	 * Dans la surcharge de cette méthode, le curseur sablier est ici automatiquement affiché.
	 *
	 * @param event
	 *           ActionEvent
	 */
	@Override
	protected void fireActionPerformed(final ActionEvent event) {
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
