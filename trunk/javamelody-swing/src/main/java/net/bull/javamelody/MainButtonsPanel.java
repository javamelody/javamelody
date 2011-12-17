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

import java.awt.Desktop;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.net.URI;
import java.net.URL;
import java.util.List;
import java.util.Map;

import javax.swing.AbstractAction;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.KeyStroke;

import net.bull.javamelody.swing.MButton;

/**
 * Panel des boutons principaux.
 * @author Emeric Vernat
 */
class MainButtonsPanel extends MelodyPanel {
	private static final ImageIcon MONITORING_ICON = ImageIconCache.getScaledImageIcon(
			"systemmonitor.png", 16, 16);
	private static final long serialVersionUID = 1L;

	MainButtonsPanel(RemoteCollector remoteCollector, final URL monitoringUrl) {
		super(remoteCollector, new FlowLayout(FlowLayout.CENTER));

		final MButton refreshButton = createRefreshButton();
		final MButton pdfButton = createPdfButton();
		final MButton onlineHelpButton = createOnlineHelpButton();

		// TODO traductions
		final MButton monitoringButton = new MButton("Monitoring", MONITORING_ICON);
		monitoringButton.setToolTipText(I18N.getFormattedString("Monitoring_sur",
				remoteCollector.getApplication()));
		add(refreshButton);
		add(pdfButton);
		add(onlineHelpButton);
		add(monitoringButton);
		add(new JLabel("        " + I18N.getString("Choix_periode") + " : "));
		for (final Period myPeriod : Period.values()) {
			final MButton myPeriodButton = new MButton(myPeriod.getLinkLabel(),
					ImageIconCache.getImageIcon(myPeriod.getIconName()));
			myPeriodButton.setToolTipText(I18N.getFormattedString("Choisir_periode",
					myPeriod.getLinkLabel()));
			add(myPeriodButton);
			myPeriodButton.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					actionChangePeriod(myPeriod.getRange());
				}
			});
		}
		// TODO ajouter bouton pour custom period

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
		final Range currentRange = MainPanel.getParentMainPanelFromChild(this).getSelectedRange();
		try {
			MainPanel.getParentMainPanelFromChild(this).setSelectedRange(newRange);

			getRemoteCollector().collectData();
			MainPanel.refreshMainTabFromChild(this);
		} catch (final IOException e) {
			showException(e);
			// si le changement de période n'a pas abouti, alors on remet l'ancienne période
			MainPanel.getParentMainPanelFromChild(this).setSelectedRange(currentRange);
		}
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
			final OutputStream output = createFileOutputStream(tempFile);
			final PdfReport pdfReport = new PdfReport(collector, collectorServer,
					javaInformationsList, range, output);
			try {
				pdfReport.preInitGraphs(smallGraphs, smallOtherGraphs, largeGraphs);
				pdfReport.toPdf();
			} finally {
				pdfReport.close();
			}
			Desktop.getDesktop().open(tempFile);
		} catch (final Exception ex) {
			showException(ex);
		}

	}
}
