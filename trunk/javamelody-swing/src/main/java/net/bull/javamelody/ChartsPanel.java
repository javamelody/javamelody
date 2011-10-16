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
import java.awt.Cursor;
import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import java.util.Map;

import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingConstants;
import javax.swing.SwingWorker;

import net.bull.javamelody.swing.MButton;
import net.bull.javamelody.swing.Utilities;
import net.bull.javamelody.swing.util.MSwingUtilities;

/**
 * Panel des graphiques principaux.
 * @author Emeric Vernat
 */
class ChartsPanel extends MelodyPanel {
	static final ImageIcon PLUS_ICON = ImageIconCache.getImageIcon("bullets/plus.png");
	static final ImageIcon MINUS_ICON = ImageIconCache.getImageIcon("bullets/minus.png");

	private static final long serialVersionUID = 1L;

	private static final Cursor HAND_CURSOR = Cursor.getPredefinedCursor(Cursor.HAND_CURSOR);
	private static final ImageIcon THROBBER_ICON = new ImageIcon(
			ChartsPanel.class.getResource("/icons/throbber.gif"));
	private static final int NB_COLS = 3;
	private static final int CHART_HEIGHT = 50;
	private static final int CHART_WIDTH = 200;

	private JPanel otherJRobinsPanel;

	ChartsPanel(RemoteCollector remoteCollector) {
		super(remoteCollector);

		final JLabel throbberLabel = new JLabel(THROBBER_ICON);
		add(throbberLabel, BorderLayout.NORTH);
		add(createButtonsPanel(), BorderLayout.CENTER);

		// SwingWorker pour afficher le reste de l'écran et pour éviter de faire attendre rien que pour les graphiques
		final SwingWorker<Map<String, byte[]>, Object> swingWorker = new SwingWorker<Map<String, byte[]>, Object>() {
			@Override
			protected Map<String, byte[]> doInBackground() throws IOException {
				return getRemoteCollector().collectJRobins(CHART_WIDTH, CHART_HEIGHT);
			}

			@Override
			protected void done() {
				try {
					final Map<String, byte[]> jrobins = get();
					final JPanel mainJRobinsPanel = createJRobinPanel(jrobins);
					remove(throbberLabel);
					add(mainJRobinsPanel, BorderLayout.NORTH);
					revalidate();
				} catch (final Exception e) {
					MSwingUtilities.showException(e);
					remove(throbberLabel);
				}
			}
		};
		swingWorker.execute();
	}

	final JPanel createJRobinPanel(Map<String, byte[]> jrobins) {
		final JPanel centerPanel = new JPanel(new GridLayout(-1, NB_COLS));
		centerPanel.setOpaque(false);
		for (final Map.Entry<String, byte[]> entry : jrobins.entrySet()) {
			final ImageIcon icon = new ImageIcon(entry.getValue());
			final JLabel label = new JLabel(icon);
			label.setHorizontalAlignment(SwingConstants.CENTER);
			label.setCursor(HAND_CURSOR);
			// TODO MouseListener pour zoom
			centerPanel.add(label);
		}

		final JPanel graphicsPanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
		graphicsPanel.setOpaque(false);
		graphicsPanel.add(centerPanel);
		return graphicsPanel;
	}

	private JPanel createButtonsPanel() {
		final MButton detailsButton = new MButton(I18N.getString("Autres_courbes"), PLUS_ICON);
		detailsButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				try {
					showOtherJRobinsPanel();
					if (detailsButton.getIcon() == PLUS_ICON) {
						detailsButton.setIcon(MINUS_ICON);
					} else {
						detailsButton.setIcon(PLUS_ICON);
					}
				} catch (final IOException ex) {
					MSwingUtilities.showException(ex);
				}
			}
		});

		return Utilities.createButtonsPanel(detailsButton);
	}

	final void showOtherJRobinsPanel() throws IOException {
		if (otherJRobinsPanel == null) {
			final Map<String, byte[]> otherJRobins = getRemoteCollector().collectOtherJRobins(
					CHART_WIDTH, CHART_HEIGHT);
			otherJRobinsPanel = createJRobinPanel(otherJRobins);
			otherJRobinsPanel.setVisible(false);
			add(otherJRobinsPanel, BorderLayout.SOUTH);
		}
		otherJRobinsPanel.setVisible(!otherJRobinsPanel.isVisible());
		otherJRobinsPanel.validate();
	}
}
