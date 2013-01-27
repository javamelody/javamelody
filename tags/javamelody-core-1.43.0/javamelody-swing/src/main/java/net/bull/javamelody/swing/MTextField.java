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
package net.bull.javamelody.swing;

import java.awt.Toolkit;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

import javax.swing.JTextField;
import javax.swing.text.Document;

/**
 * Classe abstraite servant de base aux champs de saisie. Cette classe hérite de JTextField.
 * @param <T> Type de la valeur saisie
 * @author Emeric Vernat
 */
public abstract class MTextField<T> extends JTextField {
	static final boolean TEXT_SELECTION_ON_FOCUS_GAINED = true;

	private static final long serialVersionUID = 1L;

	// ces gestionnaires d'événement sont statiques et non réinstanciés pour économie mémoire
	// l'instance du composant est déduite à partir de la source de l'événement
	private static final FocusHandler FOCUS_HANDLER = new FocusHandler();

	private static final KeyHandler KEY_HANDLER = new KeyHandler();

	/**
	 * FocusListener.
	 * @author Emeric Vernat
	 */
	private static class FocusHandler implements FocusListener {
		/**
		 * Constructeur.
		 */
		FocusHandler() {
			super();
		}

		@Override
		public void focusGained(final FocusEvent focusEvent) {
			final Object source = focusEvent.getSource();
			if (source instanceof MTextField) {
				final MTextField<?> field = (MTextField<?>) source;
				if (TEXT_SELECTION_ON_FOCUS_GAINED && field.isEditable()) {
					field.selectAll();
				}
			}
		}

		@Override
		public void focusLost(final FocusEvent focusEvent) {
			final Object source = focusEvent.getSource();
			if (source instanceof MTextField) {
				@SuppressWarnings("unchecked")
				final MTextField<Object> field = (MTextField<Object>) source;
				final Object value = field.getValue();
				field.setValue(value);
			}
		}
	}

	/**
	 * KeyListener.
	 * @author Emeric Vernat
	 */
	private static class KeyHandler implements KeyListener {
		/**
		 * Constructeur.
		 */
		KeyHandler() {
			super();
		}

		@Override
		public void keyPressed(final KeyEvent keyEvent) {
			final Object source = keyEvent.getSource();
			if (source instanceof MTextField) {
				((MTextField<?>) source).keyEvent(keyEvent);
			}
		}

		@Override
		public void keyReleased(final KeyEvent keyEvent) {
			final Object source = keyEvent.getSource();
			if (source instanceof MTextField) {
				((MTextField<?>) source).keyEvent(keyEvent);
			}
		}

		@Override
		public void keyTyped(final KeyEvent keyEvent) {
			final Object source = keyEvent.getSource();
			if (source instanceof MTextField) {
				((MTextField<?>) source).keyEvent(keyEvent);
			}
		}
	}

	/**
	 * Constructeur.
	 */
	protected MTextField() {
		this(null);
	}

	/**
	 * Constructeur.
	 * @param document
	 *           Document
	 */
	protected MTextField(Document document) {
		super(document, null, 0);
		addFocusListener(FOCUS_HANDLER);
		addKeyListener(KEY_HANDLER);

		// setDragEnabled(true);
	}

	/**
	 * Retourne la valeur saisie.
	 * @return TypeValue
	 */
	public abstract T getValue();

	/**
	 * Définit la valeur à afficher.
	 * @param value TypeValue
	 */
	public abstract void setValue(T value);

	/**
	 * Cette méthode appelée en cas de saisie invalide émet un beep.
	 */
	protected void beep() {
		Toolkit.getDefaultToolkit().beep();
	}

	/**
	 * Gestion des événements claviers. <br/>
	 * La touche Entrée valide la saisie comme à la perte du focus. <br/>
	 * Cette méthode est surchargée dans MDateField pour la date du jour
	 * @param event
	 *           KeyEvent
	 */
	protected void keyEvent(final KeyEvent event) {
		try {
			if (event.getID() == KeyEvent.KEY_PRESSED && event.getKeyCode() == KeyEvent.VK_ENTER
					&& isEditable()) {
				// voir aussi focusEvent
				setValue(getValue());
			}
		} catch (final Exception e) {
			beep();
		}
	}
}
