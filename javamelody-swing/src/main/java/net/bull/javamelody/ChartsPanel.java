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
package net.bull.javamelody;

import java.awt.BorderLayout;
import java.awt.Cursor;
import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.IOException;
import java.util.Map;

import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingConstants;
import javax.swing.SwingWorker;

import net.bull.javamelody.internal.model.RemoteCollector;
import net.bull.javamelody.swing.MButton;
import net.bull.javamelody.swing.MTransferableLabel;
import net.bull.javamelody.swing.Utilities;
import net.bull.javamelody.swing.util.MWaitCursor;

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
	private static final int CHART_WIDTH = 200;
	private static final int CHART_HEIGHT = 50;

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
					showException(e);
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
			final String jrobinName = entry.getKey();
			final byte[] imageData = entry.getValue();
			final ImageIcon icon = new ImageIcon(imageData);
			final JLabel label = new MTransferableLabel(icon);
			// ce name sera utilisé comme nom de fichier pour le drag and drop de l'image
			label.setName(getString(jrobinName));
			label.setHorizontalAlignment(SwingConstants.CENTER);
			label.setCursor(HAND_CURSOR);
			label.addMouseListener(new MouseAdapter() {
				@Override
				public void mouseClicked(MouseEvent e) {
					final MWaitCursor waitCursor = new MWaitCursor(centerPanel);
					try {
						showZoomedChart(jrobinName);
					} catch (final IOException ex) {
						showException(ex);
					} finally {
						waitCursor.restore();
					}
				}
			});
			centerPanel.add(label);
		}

		final JPanel graphicsPanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
		graphicsPanel.setOpaque(false);
		graphicsPanel.add(centerPanel);
		return graphicsPanel;
	}

	private JPanel createButtonsPanel() {
		final MButton detailsButton = new MButton(getString("Autres_courbes"), PLUS_ICON);
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
					showException(ex);
				}
			}
		});

		return Utilities.createButtonsPanel(detailsButton);
	}

	final void showOtherJRobinsPanel() throws IOException {
		if (otherJRobinsPanel == null) {
			final Map<String, byte[]> otherJRobins = getRemoteCollector()
					.collectOtherJRobins(CHART_WIDTH, CHART_HEIGHT);
			otherJRobinsPanel = createJRobinPanel(otherJRobins);
			otherJRobinsPanel.setVisible(false);
			add(otherJRobinsPanel, BorderLayout.SOUTH);
		}
		otherJRobinsPanel.setVisible(!otherJRobinsPanel.isVisible());
		otherJRobinsPanel.validate();
	}

	final void showZoomedChart(String jrobinName) throws IOException {
		final String graphName = jrobinName;
		final String graphLabel = getString(jrobinName);
		final ChartPanel panel = new ChartPanel(getRemoteCollector(), graphName, graphLabel);
		MainPanel.addOngletFromChild(this, panel);
	}
}
