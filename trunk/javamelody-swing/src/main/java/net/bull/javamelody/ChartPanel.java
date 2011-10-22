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
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;

import javax.swing.ImageIcon;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSlider;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingWorker;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

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
	private final String graphName;
	private ImageIcon imageIcon;
	private MTransferableLabel imageLabel;
	private int zoomValue;

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
		this.imageIcon = new ImageIcon(imageData);
		this.imageLabel = new MTransferableLabel(imageIcon);
		// ce name sera utilisé comme nom de fichier pour le drag and drop de l'image
		this.imageLabel.setName(I18N.getString(graphName));

		final JScrollPane scrollPane = new JScrollPane(imageLabel);
		scrollPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		scrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
		add(scrollPane, BorderLayout.CENTER);

		final JPanel southPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
		southPanel.setOpaque(false);
		southPanel.add(createSlider());
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

		// TODO traduction
		final MButton exportButton = new MButton("Exporter...");
		exportButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				try {
					export();
				} catch (final IOException ex) {
					MSwingUtilities.showException(ex);
				}
			}
		});

		// TODO boutons périodes
		return Utilities.createButtonsPanel(refreshButton, exportButton);
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
					MSwingUtilities.showException(e);
				}
			}
		};
		swingWorker.execute();
	}

	final byte[] collectJRobin(int value, int width, int height) throws IOException,
			InterruptedException {
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
