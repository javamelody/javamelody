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
import java.io.IOException;
import java.net.URL;
import java.util.List;

import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ScrollPaneConstants;

/**
 * Panel principal.
 * @author Emeric Vernat
 */
class MainPanel extends JPanel {
	private static final long serialVersionUID = 1L;

	MainPanel(Collector collector, List<JavaInformations> javaInformationsList, URL monitoringUrl)
			throws IOException {
		super();
		setLayout(new BorderLayout());
		final ScrollingPanel scrollingPanel = new ScrollingPanel(collector, javaInformationsList);
		final JScrollPane scrollPane = new JScrollPane(scrollingPanel);
		scrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
		final JPanel southPanel = new JPanel(new BorderLayout());
		southPanel.setOpaque(false);
		//		southPanel.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		southPanel.add(scrollPane, BorderLayout.CENTER);

		add(new MainButtonsPanel(collector, javaInformationsList, monitoringUrl),
				BorderLayout.NORTH);
		add(southPanel, BorderLayout.CENTER);
	}
}
