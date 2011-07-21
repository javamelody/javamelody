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

import java.awt.Color;
import java.awt.Component;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;

/**
 * Panel principal.
 * @author Emeric Vernat
 */
class ScrollingPanel extends JPanel {
	static final ImageIcon PLUS_ICON = ImageIconCache.getImageIcon("bullets/plus.png");
	static final ImageIcon MINUS_ICON = ImageIconCache.getImageIcon("bullets/minus.png");

	private static final long serialVersionUID = 1L;

	@SuppressWarnings("all")
	private final Collector collector;
	@SuppressWarnings("all")
	private final List<JavaInformations> javaInformationsList;
	private final URL monitoringUrl;
	// TODO range selon sélection (jour par défaut)
	private final Range range = Period.TOUT.getRange();

	ScrollingPanel(Collector collector, List<JavaInformations> javaInformationsList,
			URL monitoringUrl) throws IOException {
		super();
		this.collector = collector;
		this.javaInformationsList = javaInformationsList;
		this.monitoringUrl = monitoringUrl;

		setBorder(BorderFactory.createEmptyBorder(3, 3, 3, 3));
		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		setBackground(new Color(230, 230, 230));
		add(new ChartsPanel());

		addCounters();

		addSystemInformations();

		addThreadInformations();

		for (final Component component : getComponents()) {
			((JComponent) component).setAlignmentX(Component.LEFT_ALIGNMENT);
		}
	}

	private void addCounters() throws IOException {
		for (final Counter counter : collector.getRangeCountersToBeDisplayed(range)) {
			final String counterLabel = I18N.getString(counter.getName() + "Label");
			addParagraphTitle(I18N.getFormattedString("Statistiques_compteur", counterLabel)
					+ " - " + range.getLabel(), counter.getIconName());
			final StatisticsPanel statisticsPanel = new StatisticsPanel(counter, range);
			statisticsPanel.showGlobalRequests();
			add(statisticsPanel);
		}
	}

	private void addSystemInformations() {
		addParagraphTitle(I18N.getString("Informations_systemes"), "systeminfo.png");
		final List<JavaInformations> list = javaInformationsList;
		// TODO mettre propriété système system-actions-enabled dans jnlp
		if (Parameters.isSystemActionsEnabled()) {
			add(new SystemInformationsButtonsPanel(list, monitoringUrl));
		}

		final List<JavaInformationsPanel> javaInformationsPanelList = new ArrayList<JavaInformationsPanel>(
				list.size());
		final JPanel westJavaInformationsPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 5, 5));
		westJavaInformationsPanel.setOpaque(false);
		for (final JavaInformations javaInformations : list) {
			final JavaInformationsPanel javaInformationsPanel = new JavaInformationsPanel(
					javaInformations, monitoringUrl);
			javaInformationsPanel.showSummary();
			javaInformationsPanelList.add(javaInformationsPanel);
			westJavaInformationsPanel.add(javaInformationsPanel);
		}
		final MButton javaInformationsDetailsButton = new MButton(I18N.getString("Details"),
				PLUS_ICON);
		javaInformationsDetailsButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				final boolean repeatHost = list.size() > 1;
				for (final JavaInformationsPanel javaInformationsPanel : javaInformationsPanelList) {
					javaInformationsPanel.showDetails(repeatHost);
					javaInformationsPanel.validate();
				}
				if (javaInformationsDetailsButton.getIcon() == PLUS_ICON) {
					javaInformationsDetailsButton.setIcon(MINUS_ICON);
				} else {
					javaInformationsDetailsButton.setIcon(PLUS_ICON);
				}
			}
		});
		westJavaInformationsPanel.add(javaInformationsDetailsButton);
		add(westJavaInformationsPanel);
		add(new JLabel(" "));
	}

	private void addThreadInformations() {
		addParagraphTitle(I18N.getString("Threads"), "threads.png");
		for (final JavaInformations javaInformations : javaInformationsList) {
			javaInformations.getThreadInformationsList();
		}
	}

	private void addParagraphTitle(String title, String iconName) {
		final JLabel label = new JLabel(title);
		label.setIcon(ImageIconCache.getScaledImageIcon(iconName, 24, 24));
		label.setFont(label.getFont().deriveFont(Font.BOLD, label.getFont().getSize() + 4));
		add(label);
		add(new JLabel(" ")); // séparateur
	}
}
