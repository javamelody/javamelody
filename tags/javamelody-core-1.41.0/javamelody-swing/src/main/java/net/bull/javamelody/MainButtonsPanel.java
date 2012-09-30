/*
 * Copyright 2008-2012 by Emeric Vernat
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
import java.awt.Desktop;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.io.Serializable;
import java.net.URI;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.swing.AbstractAction;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.KeyStroke;

import net.bull.javamelody.swing.MButton;

/**
 * Panel des boutons principaux.
 * @author Emeric Vernat
 */
class MainButtonsPanel extends MelodyPanel {
	private static final Color BACKGROUND = new Color(186, 207, 226);
	private static final ImageIcon MONITORING_ICON = ImageIconCache.getScaledImageIcon(
			"systemmonitor.png", 16, 16);
	private static final long serialVersionUID = 1L;

	MainButtonsPanel(RemoteCollector remoteCollector, Range selectedRange, final URL monitoringUrl) {
		super(remoteCollector, new BorderLayout());
		setOpaque(true);
		setBackground(BACKGROUND);

		final JPanel centerPanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
		centerPanel.setOpaque(false);
		add(centerPanel, BorderLayout.CENTER);
		final CustomPeriodPanel customPeriodPanel = new CustomPeriodPanel(remoteCollector,
				selectedRange);
		add(customPeriodPanel, BorderLayout.SOUTH);
		final MButton refreshButton = createRefreshButton();
		final MButton pdfButton = createPdfButton();
		final MButton xmlJsonButton = createXmlJsonButton(createDefaultSerializable());
		final MButton onlineHelpButton = createOnlineHelpButton();

		final MButton monitoringButton = new MButton(I18N.getString("Monitoring"), MONITORING_ICON);
		monitoringButton.setToolTipText(I18N.getFormattedString("Monitoring_sur",
				remoteCollector.getApplication()));
		centerPanel.add(refreshButton);
		centerPanel.add(pdfButton);
		centerPanel.add(xmlJsonButton);
		centerPanel.add(onlineHelpButton);
		centerPanel.add(monitoringButton);

		addPeriodButtons(centerPanel, customPeriodPanel);

		refreshButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				actionRefresh();
			}
		});

		pdfButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				actionPdf();
			}
		});

		monitoringButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				try {
					Desktop.getDesktop().browse(new URI(monitoringUrl.toExternalForm()));
				} catch (final Exception ex) {
					showException(ex);
				}
			}
		});

		onlineHelpButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				try {
					Desktop.getDesktop().browse(
							new URI(monitoringUrl.toExternalForm() + "?resource="
									+ I18N.getString("help_url")));
				} catch (final Exception ex) {
					showException(ex);
				}
			}
		});
	}

	private void addPeriodButtons(JPanel centerPanel, final CustomPeriodPanel customPeriodPanel) {
		centerPanel.add(new JLabel("        " + I18N.getString("Choix_periode") + " : "));
		for (final Period myPeriod : Period.values()) {
			final String linkLabel = myPeriod.getLinkLabel();
			final MButton myPeriodButton = new MButton(linkLabel,
					ImageIconCache.getImageIcon(myPeriod.getIconName()));
			myPeriodButton.setToolTipText(I18N.getFormattedString("Choisir_periode", linkLabel)
					+ " (Alt-" + linkLabel.charAt(0) + ')');
			myPeriodButton.setMnemonic(linkLabel.charAt(0));
			centerPanel.add(myPeriodButton);
			myPeriodButton.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					actionChangePeriod(myPeriod.getRange());
				}
			});
		}

		final String customPeriodLinkLabel = I18N.getString("personnalisee");
		final MButton customPeriodButton = new MButton(customPeriodLinkLabel,
				ImageIconCache.getImageIcon("calendar.png"));
		customPeriodButton.setToolTipText(I18N.getFormattedString("Choisir_periode",
				customPeriodLinkLabel) + " (Alt-" + customPeriodLinkLabel.charAt(0) + ')');
		customPeriodButton.setMnemonic(customPeriodLinkLabel.charAt(0));
		centerPanel.add(customPeriodButton);

		customPeriodPanel.setVisible(false);
		customPeriodButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				customPeriodPanel.setVisible(!customPeriodPanel.isVisible());
				if (customPeriodPanel.isVisible()) {
					customPeriodPanel.requestFocusInStartField();
				}
				validate();
			}
		});
	}

	private MButton createOnlineHelpButton() {
		final MButton onlineHelpButton = new MButton(I18N.getString("Aide_en_ligne"),
				ImageIconCache.getImageIcon("action_help.png"));
		onlineHelpButton.setToolTipText(I18N.getString("Afficher_aide_en_ligne") + " (F1)");
		onlineHelpButton.setActionCommand("help");
		onlineHelpButton.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(
				KeyStroke.getKeyStroke("F1"), "doHelp");
		onlineHelpButton.getActionMap().put("doHelp", new AbstractAction() {
			private static final long serialVersionUID = 1L;

			@Override
			public void actionPerformed(ActionEvent e) {
				onlineHelpButton.doClick();
			}
		});
		return onlineHelpButton;
	}

	void actionRefresh() {
		try {
			getRemoteCollector().collectData();
			MainPanel.refreshMainTabFromChild(this);
		} catch (final IOException e) {
			showException(e);
		}
	}

	void actionChangePeriod(Range newRange) {
		MainPanel.getParentMainPanelFromChild(this).changeRange(newRange);
	}

	void actionPdf() {
		try {
			// ici on prend un comportement similaire au serveur de collecte
			// en ne mettant pas les requêtes en cours puisque de toute façon on n'a pas
			// les données dans les counters
			final boolean collectorServer = true;
			final Range range = MainPanel.getParentMainPanelFromChild(this).getSelectedRange();
			final File tempFile = createTempFileForPdf();
			final RemoteCollector remoteCollector = getRemoteCollector();
			final Collector collector = remoteCollector.getCollector();
			final List<JavaInformations> javaInformationsList = remoteCollector
					.getJavaInformationsList();
			final Map<String, byte[]> smallGraphs = remoteCollector.collectJRobins(
					PdfReport.SMALL_GRAPH_WIDTH, PdfReport.SMALL_GRAPH_HEIGHT);
			final Map<String, byte[]> smallOtherGraphs = remoteCollector.collectOtherJRobins(
					PdfReport.SMALL_GRAPH_WIDTH, PdfReport.SMALL_GRAPH_HEIGHT);
			final Map<String, byte[]> largeGraphs = remoteCollector.collectJRobins(
					PdfReport.LARGE_GRAPH_WIDTH, PdfReport.LARGE_GRAPH_HEIGHT);
			try (final OutputStream output = createFileOutputStream(tempFile)) {
				final PdfReport pdfReport = new PdfReport(collector, collectorServer,
						javaInformationsList, range, output);
				try {
					// PdfReport utilise collector.getRangeCountersToBeDisplayed(range),
					// mais les counters contiennent les bonnes données pour la période TOUT
					// et non pas celle de la variable "range"
					pdfReport.setCounterRange(Period.TOUT.getRange());
					pdfReport.preInitGraphs(smallGraphs, smallOtherGraphs, largeGraphs);
					pdfReport.toPdf();
				} finally {
					pdfReport.close();
				}
			}
			Desktop.getDesktop().open(tempFile);
		} catch (final Exception ex) {
			showException(ex);
		}
	}

	private Serializable createDefaultSerializable() {
		final RemoteCollector remoteCollector = getRemoteCollector();
		final Collector collector = remoteCollector.getCollector();
		final List<JavaInformations> javaInformationsList = remoteCollector
				.getJavaInformationsList();
		final List<Counter> counters = collector.getCounters();
		final List<Serializable> serialized = new ArrayList<>(counters.size()
				+ javaInformationsList.size());
		// on clone les counters avant de les sérialiser pour ne pas avoir de problèmes de concurrences d'accès
		for (final Counter counter : counters) {
			serialized.add(counter.clone());
		}
		serialized.addAll(javaInformationsList);
		return (Serializable) serialized;
	}
}
