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
package net.bull.javamelody;

import java.awt.Desktop;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.net.URI;
import java.net.URL;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;

/**
 * Panel des boutons principaux.
 * @author Emeric Vernat
 */
class MainButtonsPanel extends JPanel {
	private static final long serialVersionUID = 1L;

	MainButtonsPanel(final URL onlineHelpUrl) {
		super();

		setOpaque(false);
		setLayout(new FlowLayout(FlowLayout.CENTER));

		final JButton refreshButton = new JButton(I18N.getString("Actualiser"),
				ImageIconCache.getImageIcon("action_refresh.png"));
		refreshButton.setToolTipText(I18N.getString("Rafraichir"));
		final JButton pdfButton = new JButton(I18N.getString("PDF"),
				ImageIconCache.getImageIcon("pdf.png"));
		pdfButton.setToolTipText(I18N.getString("afficher_PDF"));
		final JButton onlineHelpButton = new JButton(I18N.getString("Aide_en_ligne"),
				ImageIconCache.getImageIcon("action_help.png"));
		onlineHelpButton.setToolTipText(I18N.getString("Afficher_aide_en_ligne"));
		add(refreshButton);
		add(pdfButton);
		add(onlineHelpButton);
		add(new JLabel("        " + I18N.getString("Choix_periode") + " : "));
		for (final Period myPeriod : Period.values()) {
			final JButton myPeriodButton = new JButton(myPeriod.getLinkLabel(),
					ImageIconCache.getImageIcon(myPeriod.getIconName()));
			myPeriodButton.setToolTipText(I18N.getFormattedString("Choisir_periode",
					myPeriod.getLinkLabel()));
			add(myPeriodButton);
		}

		onlineHelpButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				try {
					Desktop.getDesktop().browse(
							new URI(onlineHelpUrl.toExternalForm() + "?resource="
									+ I18N.getString("help_url")));
				} catch (final Exception ex) {
					// TODO Auto-generated catch block
					ex.printStackTrace();
				}
			}
		});
	}
}
