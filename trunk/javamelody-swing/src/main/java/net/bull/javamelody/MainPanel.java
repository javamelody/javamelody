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
import java.awt.Font;
import java.io.IOException;
import java.net.URL;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ScrollPaneConstants;

/**
 * Panel principal.
 * @author Emeric Vernat
 */
class MainPanel extends JPanel {
	private static final long serialVersionUID = 1L;

	@SuppressWarnings("all")
	private final Collector collector;
	// TODO range selon sélection (jour par défaut)
	private final Range range = Period.TOUT.getRange();
	private final JPanel scrollPanel = new JPanel();

	MainPanel(Collector collector, List<JavaInformations> javaInformationsList, URL onlineHelpUrl)
			throws IOException {
		super();
		this.collector = collector;

		setLayout(new BorderLayout());

		scrollPanel.setBorder(BorderFactory.createEmptyBorder(3, 3, 3, 3));
		scrollPanel.setLayout(new BoxLayout(scrollPanel, BoxLayout.Y_AXIS));
		scrollPanel.add(new ChartsPanel());
		scrollPanel.setBackground(new Color(230, 230, 230));
		for (final Counter counter : collector.getRangeCountersToBeDisplayed(range)) {
			final String counterLabel = I18N.getString(counter.getName() + "Label");
			addParagraphTitle(I18N.getFormattedString("Statistiques_compteur", counterLabel)
					+ " - " + range.getLabel(), counter.getIconName());
			final StatisticsPanel statisticsPanel = new StatisticsPanel(counter, range);
			statisticsPanel.showGlobalRequests();
			scrollPanel.add(statisticsPanel);
		}
		for (final JavaInformations javaInformations : javaInformationsList) {
			addParagraphTitle(I18N.getString("Informations_systemes"), "systeminfo.png");
			final JPanel westPanel = new JPanel(new BorderLayout());
			westPanel.setOpaque(false);
			westPanel.add(new JavaInformationsPanel(javaInformations), BorderLayout.WEST);
			scrollPanel.add(westPanel);
		}
		for (final Component component : scrollPanel.getComponents()) {
			((JComponent) component).setAlignmentX(Component.LEFT_ALIGNMENT);
		}
		final JScrollPane scrollPane = new JScrollPane(scrollPanel);
		scrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
		final JPanel southPanel = new JPanel(new BorderLayout());
		southPanel.setOpaque(false);
		//		southPanel.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		southPanel.add(scrollPane, BorderLayout.CENTER);

		add(new MainButtonsPanel(onlineHelpUrl), BorderLayout.NORTH);
		add(southPanel, BorderLayout.CENTER);
	}

	private void addParagraphTitle(String title, String iconName) {
		final JLabel label = new JLabel(title);
		label.setIcon(ImageIconCache.getScaledImageIcon(iconName, 24, 24));
		label.setFont(label.getFont().deriveFont(Font.BOLD, label.getFont().getSize() + 4));
		scrollPanel.add(label);
	}
}
