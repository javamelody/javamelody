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

import net.bull.javamelody.internal.model.Collector;
import net.bull.javamelody.internal.model.Counter.CounterRequestContextComparator;
import net.bull.javamelody.internal.model.CounterRequestContext;
import net.bull.javamelody.internal.model.JavaInformations;
import net.bull.javamelody.internal.model.Period;
import net.bull.javamelody.internal.model.Range;
import net.bull.javamelody.internal.model.RemoteCollector;
import net.bull.javamelody.internal.web.pdf.PdfCoreReport;
import net.bull.javamelody.internal.web.pdf.PdfReport;
import net.bull.javamelody.swing.MButton;

/**
 * Panel des boutons principaux.
 * @author Emeric Vernat
 */
class MainButtonsPanel extends MelodyPanel {
	private static final Color BACKGROUND = new Color(186, 207, 226);
	private static final ImageIcon MONITORING_ICON = ImageIconCache
			.getScaledImageIcon("systemmonitor.png", 16, 16);
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
		final DeploymentPeriodPanel deploymentPeriodPanel = createDeploymentPeriodPanel(
				remoteCollector, selectedRange);
		final JPanel customPeriodsPanel = new JPanel(new BorderLayout());
		customPeriodsPanel.setOpaque(false);
		if (deploymentPeriodPanel != null) {
			customPeriodsPanel.add(deploymentPeriodPanel, BorderLayout.NORTH);
		}
		customPeriodsPanel.add(customPeriodPanel, BorderLayout.SOUTH);
		add(customPeriodsPanel, BorderLayout.SOUTH);

		final MButton refreshButton = createRefreshButton();
		final MButton pdfButton = createPdfButton();
		// on ne peut pas instancier defaultSerializable ici car sinon,
		// les données ne seraient plus à jour après une actualisation, donc on passe la valeur spéciale null
		final MButton xmlJsonButton = createXmlJsonButton(null);
		final MButton onlineHelpButton = createOnlineHelpButton();

		final MButton monitoringButton = new MButton(getString("Monitoring"), MONITORING_ICON);
		monitoringButton.setToolTipText(
				getFormattedString("Monitoring_sur", remoteCollector.getApplication()));
		centerPanel.add(refreshButton);
		centerPanel.add(pdfButton);
		centerPanel.add(xmlJsonButton);
		centerPanel.add(onlineHelpButton);
		centerPanel.add(monitoringButton);

		addPeriodButtons(centerPanel, customPeriodPanel, deploymentPeriodPanel);
		addCustomPeriodsButtons(centerPanel, customPeriodPanel, deploymentPeriodPanel);

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
					Desktop.getDesktop().browse(new URI(
							monitoringUrl.toExternalForm() + "?resource=" + getString("help_url")));
				} catch (final Exception ex) {
					showException(ex);
				}
			}
		});
	}

	private DeploymentPeriodPanel createDeploymentPeriodPanel(RemoteCollector remoteCollector,
			Range selectedRange) {
		try {
			final DeploymentPeriodPanel deploymentPeriodPanel = new DeploymentPeriodPanel(
					remoteCollector, selectedRange);
			if (deploymentPeriodPanel.getWebappVersions().isEmpty()) {
				return null;
			}
			return deploymentPeriodPanel;
		} catch (final IOException e) {
			return null;
		}
	}

	private void addPeriodButtons(JPanel centerPanel, final CustomPeriodPanel customPeriodPanel,
			final DeploymentPeriodPanel deploymentPeriodPanel) {
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
					customPeriodPanel.setVisible(false);
					if (deploymentPeriodPanel != null) {
						deploymentPeriodPanel.setVisible(false);
					}
					actionChangePeriod(myPeriod.getRange());
				}
			});
		}
	}

	private void addCustomPeriodsButtons(JPanel centerPanel,
			final CustomPeriodPanel customPeriodPanel,
			final DeploymentPeriodPanel deploymentPeriodPanel) {
		final String customPeriodLinkLabel = getString("personnalisee");
		final MButton customPeriodButton = new MButton(customPeriodLinkLabel,
				ImageIconCache.getImageIcon("calendar.png"));
		customPeriodButton
				.setToolTipText(getFormattedString("Choisir_periode", customPeriodLinkLabel)
						+ " (Alt-" + customPeriodLinkLabel.charAt(0) + ')');
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
				if (deploymentPeriodPanel != null) {
					deploymentPeriodPanel.setVisible(false);
				}
				validate();
			}
		});

		if (deploymentPeriodPanel != null) {
			final String deploymentLabel = getString("par_deploiement");
			final MButton deploymentButton = new MButton(deploymentLabel,
					ImageIconCache.getImageIcon("calendar.png"));
			deploymentButton.setToolTipText(
					getFormattedString("Choisir_periode", deploymentLabel) + " (Alt-D)");
			deploymentButton.setMnemonic('D');
			centerPanel.add(deploymentButton);

			deploymentPeriodPanel.setVisible(false);
			deploymentButton.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					deploymentPeriodPanel.setVisible(!deploymentPeriodPanel.isVisible());
					if (deploymentPeriodPanel.isVisible()) {
						deploymentPeriodPanel.requestFocusInVersionField();
					}
					customPeriodPanel.setVisible(false);
					validate();
				}
			});
		}
	}

	private MButton createOnlineHelpButton() {
		final MButton onlineHelpButton = new MButton(getString("Aide_en_ligne"),
				ImageIconCache.getImageIcon("action_help.png"));
		onlineHelpButton.setToolTipText(getString("Afficher_aide_en_ligne") + " (F1)");
		onlineHelpButton.setActionCommand("help");
		onlineHelpButton.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW)
				.put(KeyStroke.getKeyStroke("F1"), "doHelp");
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
		final Map<String, byte[]> smallGraphs = remoteCollector
				.collectJRobins(PdfCoreReport.SMALL_GRAPH_WIDTH, PdfCoreReport.SMALL_GRAPH_HEIGHT);
		final Map<String, byte[]> smallOtherGraphs = remoteCollector.collectOtherJRobins(
				PdfCoreReport.SMALL_GRAPH_WIDTH, PdfCoreReport.SMALL_GRAPH_HEIGHT);
		final Map<String, byte[]> largeGraphs = remoteCollector
				.collectJRobins(PdfCoreReport.LARGE_GRAPH_WIDTH, PdfCoreReport.LARGE_GRAPH_HEIGHT);
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
					Collections.sort(currentRequests, Collections.reverseOrder(
							new CounterRequestContextComparator(System.currentTimeMillis())));
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
