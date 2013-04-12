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
import java.awt.Component;
import java.awt.Desktop;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Map;

import javax.swing.BoxLayout;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ScrollPaneConstants;

import net.bull.javamelody.swing.MButton;
import net.bull.javamelody.swing.Utilities;

/**
 * Panel de la liste des requêtes en cours pour un serveur de collecte.
 * @author Emeric Vernat
 */
class CurrentRequestsForCollectorServerPanel extends MelodyPanel {
	private static final long serialVersionUID = 1L;

	@SuppressWarnings("all")
	private Map<JavaInformations, List<CounterRequestContext>> currentRequests;

	CurrentRequestsForCollectorServerPanel(RemoteCollector remoteCollector) throws IOException {
		super(remoteCollector);

		refresh();
	}

	final void refresh() throws IOException {
		removeAll();

		setName(getString("Requetes_en_cours"));
		final JLabel titleLabel = Utilities.createParagraphTitle(getName(), "hourglass.png");
		add(titleLabel, BorderLayout.NORTH);

		try {
			this.currentRequests = getRemoteCollector().collectCurrentRequests();
			if (currentRequests.isEmpty()) {
				final JPanel labelPanel = new JPanel(new BorderLayout());
				labelPanel.setOpaque(false);
				labelPanel.add(new JLabel("   " + getString("Aucune_requete_en_cours")),
						BorderLayout.NORTH);
				add(labelPanel, BorderLayout.CENTER);
			} else {
				final JPanel centerPanel = createCenterPanel();
				final JScrollPane scrollPane = new JScrollPane(centerPanel);
				scrollPane
						.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
				scrollPane.getVerticalScrollBar().setUnitIncrement(20);
				add(scrollPane, BorderLayout.CENTER);
			}
		} finally {
			// boutons en finally pour avoir le bouton Actualiser même après une erreur
			add(createButtonsPanel(), BorderLayout.SOUTH);
		}
	}

	private JPanel createCenterPanel() {
		final JPanel panel = new JPanel();
		panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
		panel.setOpaque(false);
		for (final Map.Entry<JavaInformations, List<CounterRequestContext>> entry : currentRequests
				.entrySet()) {
			final JavaInformations javaInformations = entry.getKey();
			final List<CounterRequestContext> contexts = entry.getValue();
			final CounterRequestContextPanel contextsPanel = new CounterRequestContextPanel(
					getRemoteCollector(), contexts, javaInformations);
			contextsPanel.addCurrentRequestsCount(contexts.size());
			panel.add(contextsPanel);
		}

		for (final Component component : panel.getComponents()) {
			((JComponent) component).setAlignmentX(Component.LEFT_ALIGNMENT);
		}
		return panel;
	}

	private JPanel createButtonsPanel() {
		final MButton refreshButton = createRefreshButton();
		final MButton pdfButton = createPdfButton();

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

		pdfButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				try {
					actionPdf();
				} catch (final IOException ex) {
					showException(ex);
				}
			}
		});

		return Utilities.createButtonsPanel(refreshButton, pdfButton);
	}

	final void actionPdf() throws IOException {
		final File tempFile = createTempFileForPdf();
		final PdfOtherReport pdfOtherReport = createPdfOtherReport(tempFile);
		try {
			CounterRequestContextPanel.writeAllCurrentRequestsAsPart(pdfOtherReport,
					currentRequests, getCollector());
		} finally {
			pdfOtherReport.close();
		}
		Desktop.getDesktop().open(tempFile);
	}
}
