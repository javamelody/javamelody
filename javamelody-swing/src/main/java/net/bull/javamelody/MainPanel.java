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
import java.awt.Color;
import java.awt.Component;
import java.io.IOException;
import java.net.URL;

import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ScrollPaneConstants;
import javax.swing.WindowConstants;

import net.bull.javamelody.util.MSwingUtilities;

/**
 * Panel principal.
 * @author Emeric Vernat
 */
class MainPanel extends JPanel {
	private static final Color BACKGROUND = new Color(230, 230, 230);
	private static final long serialVersionUID = 1L;

	// TODO mettre exporter en pdf, rtf, xml et json dans un menu contextuel

	MainPanel(RemoteCollector remoteCollector, URL monitoringUrl) throws IOException {
		super(new BorderLayout());
		final ScrollingPanel scrollingPanel = new ScrollingPanel(remoteCollector, monitoringUrl);
		final JScrollPane scrollPane = new JScrollPane(scrollingPanel);
		scrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
		scrollPane.getVerticalScrollBar().setUnitIncrement(20);
		final JPanel southPanel = new JPanel(new BorderLayout());
		southPanel.setOpaque(false);
		//		southPanel.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		southPanel.add(scrollPane, BorderLayout.CENTER);

		add(new MainButtonsPanel(remoteCollector, monitoringUrl), BorderLayout.NORTH);
		add(southPanel, BorderLayout.CENTER);
	}

	static void addOngletFromChild(Component child, JPanel panel) {
		final MainPanel mainPanel = MSwingUtilities.getAncestorOfClass(MainPanel.class, child);
		// TODO
		panel.setOpaque(true);
		panel.setBackground(BACKGROUND);
		MSwingUtilities.run(panel).setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
	}
}
