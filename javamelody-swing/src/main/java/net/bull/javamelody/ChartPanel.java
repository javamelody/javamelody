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
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;

import javax.swing.AbstractAction;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSlider;
import javax.swing.KeyStroke;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingWorker;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import net.bull.javamelody.internal.model.RemoteCollector;
import net.bull.javamelody.swing.MButton;
import net.bull.javamelody.swing.MTransferableLabel;
import net.bull.javamelody.swing.Utilities;
import net.bull.javamelody.swing.util.MSwingUtilities;

/**
 * Panel d'un graphique zoomé.
 * @author Emeric Vernat
 */
class ChartPanel extends MelodyPanel {
	private static final long serialVersionUID = 1L;

	private static final int CHART_WIDTH = 960;
	private static final int CHART_HEIGHT = 400;
	private final String graphLabel;
	private final String graphName;
	private final MButton refreshButton;
	private ImageIcon imageIcon;
	private MTransferableLabel imageLabel;
	private int zoomValue;

	ChartPanel(RemoteCollector remoteCollector, String graphName, String graphLabel)
			throws IOException {
		this(remoteCollector, graphName, graphLabel, null);
	}

	ChartPanel(RemoteCollector remoteCollector, String graphName, String graphLabel,
			MButton refreshButton) throws IOException {
		super(remoteCollector);
		this.graphName = graphName;
		this.graphLabel = graphLabel;

		if (refreshButton == null) {
			this.refreshButton = createRefreshButton();
			this.refreshButton.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					try {
						refresh();
					} catch (final IOException ex) {
						showException(ex);
					}
				}
			});
		} else {
			this.refreshButton = refreshButton;
		}

		refresh();
	}

	final void refresh() throws IOException {
		removeAll();

		setName(graphLabel);

		final JPanel southPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
		southPanel.setOpaque(false);

		zoomValue = 0;
		final byte[] imageData = getRemoteCollector().collectJRobin(graphName, CHART_WIDTH,
				CHART_HEIGHT);
		if (imageData != null) {
			this.imageIcon = new ImageIcon(imageData);
			this.imageLabel = new MTransferableLabel(imageIcon);
			// ce name sera utilisé comme nom de fichier pour le drag and drop de l'image
			this.imageLabel.setName(graphLabel);

			final JScrollPane scrollPane = new JScrollPane(imageLabel);
			scrollPane.setHorizontalScrollBarPolicy(
					ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
			scrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
			add(scrollPane, BorderLayout.CENTER);

			southPanel.add(createSlider());
		} else {
			this.imageIcon = null;
			this.imageLabel = null;
		}
		southPanel.add(createButtonsPanel());
		add(southPanel, BorderLayout.SOUTH);
	}

	private JSlider createSlider() {
		final JSlider slider = new JSlider();
		slider.setOpaque(false);
		slider.setMinimum(10);
		slider.setMaximum(200);
		slider.setValue(100);
		slider.setLabelTable(slider.createStandardLabels(50));
		slider.setMajorTickSpacing(100);
		slider.setMinorTickSpacing(10);
		slider.setExtent(20);
		// slider.setPaintLabels(true);
		// slider.setPaintTicks(true);
		// slider.setSnapToTicks(true);
		slider.addChangeListener(new ChangeListener() {
			@Override
			public void stateChanged(ChangeEvent e) {
				final int value = slider.getValue();
				refreshZoom(value);
			}
		});
		return slider;
	}

	private JPanel createButtonsPanel() {
		if (getImageLabel() != null) {
			final MButton exportButton = new MButton(getString("Exporter") + "...");
			exportButton.setToolTipText(exportButton.getText() + " (F12)");
			exportButton.setActionCommand("export");
			exportButton.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW)
					.put(KeyStroke.getKeyStroke("F12"), "doExport");
			exportButton.getActionMap().put("doExport", new AbstractAction() {
				private static final long serialVersionUID = 1L;

				@Override
				public void actionPerformed(ActionEvent e) {
					exportButton.doClick();
				}
			});
			exportButton.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					try {
						export();
					} catch (final IOException ex) {
						showException(ex);
					}
				}
			});
			return Utilities.createButtonsPanel(refreshButton, exportButton);
		}

		// TODO ajouter boutons périodes ? ainsi que ci-dessus
		return Utilities.createButtonsPanel(refreshButton);
	}

	final void refreshZoom(final int value) {
		this.zoomValue = value;
		final int width = Math.max(1, imageIcon.getIconWidth() * value / 100);
		final int height = Math.max(1, imageIcon.getIconHeight() * value / 100);
		final ImageIcon scaledImageIcon = MSwingUtilities.getScaledInstance(imageIcon, width,
				height);
		// setIcon appelle déjà revalidate() et repaint()
		getImageLabel().setIcon(scaledImageIcon);
		// SwingWorker pour recharger le graphique dans la bonne dimension en tâche de fond
		final SwingWorker<byte[], Object> swingWorker = new SwingWorker<byte[], Object>() {
			@Override
			protected byte[] doInBackground() throws IOException, InterruptedException {
				return collectJRobin(value, width, height);
			}

			@Override
			protected void done() {
				try {
					final byte[] imageData = get();
					// si la valeur de zoom a déjà rechangé, imageData peut être null
					if (imageData != null) {
						getImageLabel().setIcon(new ImageIcon(imageData));
					}
				} catch (final Exception e) {
					showException(e);
				}
			}
		};
		swingWorker.execute();
	}

	final byte[] collectJRobin(int value, int width, int height)
			throws IOException, InterruptedException {
		// on attend 300 ms avant de voir si l'utilisateur se stabilise sur une valeur de zoom
		// et sinon inutile de charger l'image avec cette valeur de zoom
		Thread.sleep(300);
		//		if (zoomValue == value && imageLabel.getIcon().getIconWidth() != width
		//				&& imageLabel.getIcon().getIconHeight() != height) {
		if (zoomValue == value) {
			zoomValue = -1;
			return getRemoteCollector().collectJRobin(graphName, width, height);
		}
		return null;
	}

	final void export() throws IOException {
		getImageLabel().export();
	}

	final MTransferableLabel getImageLabel() {
		return imageLabel;
	}
}
