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

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;

import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JPanel;

import net.bull.javamelody.swing.MButton;
import net.bull.javamelody.swing.Utilities;

/**
 * Panel d'un graphique zoomé.
 * @author Emeric Vernat
 */
class ChartPanel extends MelodyPanel {
	private static final long serialVersionUID = 1L;

	private static final int CHART_WIDTH = 960;
	private static final int CHART_HEIGHT = 400;
	private final String graphName;

	ChartPanel(RemoteCollector remoteCollector, String graphName) throws IOException {
		super(remoteCollector);
		this.graphName = graphName;
		setName(I18N.getString(graphName));

		refresh();
	}

	final void refresh() throws IOException {
		removeAll();

		final byte[] imageData = getRemoteCollector().collectJRobin(graphName, CHART_WIDTH,
				CHART_HEIGHT);
		final JLabel label = new JLabel(new ImageIcon(imageData));
		add(label, BorderLayout.CENTER);

		add(createButtonsPanel(), BorderLayout.SOUTH);
	}

	private JPanel createButtonsPanel() {
		final MButton refreshButton = new MButton(I18N.getString("Actualiser"),
				ImageIconCache.getImageIcon("action_refresh.png"));
		refreshButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				try {
					refresh();
				} catch (final IOException ex) {
					showException(ex);
				}
			}
		});

		// TODO boutons périodes et slider pour rezoom
		return Utilities.createButtonsPanel(refreshButton);
	}
}
