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
import java.net.URI;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import javax.swing.AbstractAction;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.KeyStroke;

import net.bull.javamelody.Counter.CounterRequestContextComparator;
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

	private final boolean collectorServer;

	MainButtonsPanel(RemoteCollector remoteCollector, Range selectedRange, final URL monitoringUrl,
			boolean collectorServer) {
		super(remoteCollector, new BorderLayout());
		this.collectorServer = collectorServer;
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
		// on ne peut pas instancier defaultSerializable ici car sinon,
		// les données ne seraient plus à jour après une actualisation, donc on passe la valeur spéciale null
		final MButton xmlJsonButton = createXmlJsonButton(null);
		final MButton onlineHelpButton = createOnlineHelpButton();

		final MButton monitoringButton = new MButton(getString("Monitoring"), MONITORING_ICON);
		monitoringButton.setToolTipText(getFormattedString("Monitoring_sur",
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
				try {
					actionPdf();
				} catch (final Exception ex) {
					showException(ex);
				}
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
									+ getString("help_url")));
				} catch (final Exception ex) {
					showException(ex);
				}
			}
		});
	}

	private void addPeriodButtons(JPanel centerPanel, final CustomPeriodPanel customPeriodPanel) {
		centerPanel.add(new JLabel("        " + getString("Choix_periode") + " : "));
		for (final Period myPeriod : Period.values()) {
			final String linkLabel = myPeriod.getLinkLabel();
			final MButton myPeriodButton = new MButton(linkLabel,
					ImageIconCache.getImageIcon(myPeriod.getIconName()));
			myPeriodButton.setToolTipText(getFormattedString("Choisir_periode", linkLabel)
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

		final String customPeriodLinkLabel = getString("personnalisee");
		final MButton customPeriodButton = new MButton(customPeriodLinkLabel,
				ImageIconCache.getImageIcon("calendar.png"));
		customPeriodButton.setToolTipText(getFormattedString("Choisir_periode",
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
		final MButton onlineHelpButton = new MButton(getString("Aide_en_ligne"),
				ImageIconCache.getImageIcon("action_help.png"));
		onlineHelpButton.setToolTipText(getString("Afficher_aide_en_ligne") + " (F1)");
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
			getRemoteCollector().collectDataIncludingCurrentRequests();
			MainPanel.refreshMainTabFromChild(this);
		} catch (final IOException e) {
			showException(e);
		}
	}

	void actionChangePeriod(Range newRange) {
		MainPanel.getParentMainPanelFromChild(this).changeRange(newRange);
	}

	void actionPdf() throws IOException {
		final Range range = MainPanel.getParentMainPanelFromChild(this).getSelectedRange();
		final File tempFile = createTempFileForPdf();
		final Collector collector = getCollector();
		final List<JavaInformations> javaInformationsList = getJavaInformationsList();
		final RemoteCollector remoteCollector = getRemoteCollector();
		final Map<String, byte[]> smallGraphs = remoteCollector.collectJRobins(
				PdfCoreReport.SMALL_GRAPH_WIDTH, PdfCoreReport.SMALL_GRAPH_HEIGHT);
		final Map<String, byte[]> smallOtherGraphs = remoteCollector.collectOtherJRobins(
				PdfCoreReport.SMALL_GRAPH_WIDTH, PdfCoreReport.SMALL_GRAPH_HEIGHT);
		final Map<String, byte[]> largeGraphs = remoteCollector.collectJRobins(
				PdfCoreReport.LARGE_GRAPH_WIDTH, PdfCoreReport.LARGE_GRAPH_HEIGHT);
		try (final OutputStream output = createFileOutputStream(tempFile)) {
			final PdfReport pdfReport = new PdfReport(collector, collectorServer,
					javaInformationsList, range, output);
			try {
				// PdfReport utilise collector.getRangeCountersToBeDisplayed(range),
				// mais les counters contiennent les bonnes données pour la période TOUT
				// et non pas celle de la variable "range"
				pdfReport.setCounterRange(Period.TOUT.getRange());
				pdfReport.preInitGraphs(smallGraphs, smallOtherGraphs, largeGraphs);
				if (!collectorServer) {
					final List<CounterRequestContext> currentRequests = new ArrayList<>();
					for (final List<CounterRequestContext> requests : getRemoteCollector()
							.getCurrentRequests().values()) {
						currentRequests.addAll(requests);
					}
					Collections.sort(currentRequests, Collections
							.reverseOrder(new CounterRequestContextComparator(System
									.currentTimeMillis())));
					pdfReport.setCurrentRequests(currentRequests);
				}
				pdfReport.toPdf();
			} finally {
				pdfReport.close();
			}
		}
		Desktop.getDesktop().open(tempFile);
	}
}
