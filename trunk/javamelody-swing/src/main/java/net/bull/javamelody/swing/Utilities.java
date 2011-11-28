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

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;

import javax.swing.BorderFactory;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextArea;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingUtilities;

import net.bull.javamelody.ImageIconCache;
import net.bull.javamelody.swing.util.MSwingUtilities;

/**
 * Classe utilitaire.
 * @author Emeric Vernat
 */
public final class Utilities {
	static final MouseWheelListener DELEGATE_TO_PARENT_MOUSE_WHEEL_LISTENER = new MouseWheelListener() {
		@Override
		public void mouseWheelMoved(MouseWheelEvent event) {
			// on reporte l'évènement mouseWheelMoved de ce scrollPane vers son parent
			final Container parent = event.getComponent().getParent();
			parent.dispatchEvent(SwingUtilities.convertMouseEvent(event.getComponent(), event,
					parent));
		}
	};

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
	 * Création panel non opaque et avec des composants alignés à droite.
	 * @param components JComponent...
	 * @return JPanel
	 */
	public static JPanel createButtonsPanel(JComponent... components) {
		final JPanel buttonsPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
		buttonsPanel.setOpaque(false);
		for (final JComponent component : components) {
			buttonsPanel.add(component);
		}
		return buttonsPanel;
	}

	/**
	 * Fixe la taille exacte d'une JTable à celle nécessaire pour afficher les données.
	 * @param table JTable
	 */
	public static void adjustTableHeight(final JTable table) {
		table.setPreferredScrollableViewportSize(new Dimension(-1, table.getPreferredSize().height));
		// on utilise invokeLater pour configurer le scrollPane car lors de l'exécution ce cette méthode
		// la table n'est pas encore dans son scrollPane parent
		SwingUtilities.invokeLater(new Runnable() {
			@Override
			public void run() {
				final JScrollPane scrollPane = MSwingUtilities.getAncestorOfClass(
						JScrollPane.class, table);
				scrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER);
				// Puisqu'il n'y a pas d'ascenceur sur ce scrollPane,
				// il est inutile que la mollette de souris serve à bouger cet ascenseur,
				// mais il est très utile en revanche que ce scrollPane ne bloque pas l'utilisation
				// de la mollette de souris pour le scrollPane global de l'onglet principal.
				// On commence par enlever le listener au cas où la méthode soit appelée deux fois sur la même table.
				scrollPane.removeMouseWheelListener(DELEGATE_TO_PARENT_MOUSE_WHEEL_LISTENER);
				scrollPane.addMouseWheelListener(DELEGATE_TO_PARENT_MOUSE_WHEEL_LISTENER);
			}
		});
	}

	/**
	 * Affiche un texte scrollable non éditable dans une popup.
	 * @param component Parent
	 * @param title Titre de la popup
	 * @param text Texte
	 */
	public static void showTextInPopup(Component component, String title, String text) {
		final JTextArea textArea = new JTextArea();
		textArea.setText(text);
		textArea.setEditable(false);
		textArea.setCaretPosition(0);
		// background nécessaire avec la plupart des look and feels dont Nimbus,
		// sinon il reste blanc malgré editable false
		textArea.setBackground(Color.decode("#E6E6E6"));
		final JScrollPane scrollPane = new JScrollPane(textArea);

		final JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT, 0, 5));
		// TODO traduction
		final MButton clipBoardButton = new MButton("Copier dans presse-papiers");
		clipBoardButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				textArea.selectAll();
				textArea.copy();
				textArea.setCaretPosition(0);
			}
		});
		buttonPanel.setOpaque(false);
		buttonPanel.add(clipBoardButton);

		final Window window = SwingUtilities.getWindowAncestor(component);
		final JDialog dialog = new JDialog((JFrame) window, title, true);
		final JPanel contentPane = new JPanel(new BorderLayout());
		contentPane.add(scrollPane, BorderLayout.CENTER);
		contentPane.add(buttonPanel, BorderLayout.SOUTH);
		dialog.setContentPane(contentPane);
		dialog.pack();
		dialog.setLocationRelativeTo(window);
		dialog.setVisible(true);
	}
}
