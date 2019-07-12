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
import java.awt.Desktop;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;

import javax.swing.JPanel;

import net.bull.javamelody.internal.model.Collector;
import net.bull.javamelody.internal.model.Counter;
import net.bull.javamelody.internal.model.Range;
import net.bull.javamelody.internal.model.RemoteCollector;
import net.bull.javamelody.internal.web.pdf.PdfOtherReport;
import net.bull.javamelody.swing.MButton;
import net.bull.javamelody.swing.Utilities;

/**
 * Panel du rapport des statistiques d'un compteur résumées par classes ou pour une classe.
 * @author Emeric Vernat
 */
class CounterSummaryPerClassPanel extends MelodyPanel {
	private static final long serialVersionUID = 1L;

	private final Counter counter;
	private final Range range;
	private final String requestId;

	CounterSummaryPerClassPanel(RemoteCollector remoteCollector, Counter counter, Range range,
			String requestId) {
		super(remoteCollector);
		assert counter != null;
		assert range != null;
		// requestId peut être null (pour les statistisques aggrégées par classes)
		this.counter = counter;
		this.range = range;
		this.requestId = requestId;

		refresh();
	}

	final void refresh() {
		removeAll();

		final String counterLabel = getString(counter.getName() + "Label");
		setName(getFormattedString("Statistiques_compteur", counterLabel) + " - "
				+ range.getLabel());
		add(Utilities.createParagraphTitle(getName(), counter.getIconName()), BorderLayout.NORTH);

		final MButton detailsButton;
		if (requestId == null) {
			detailsButton = new MButton(getString("Details"));
		} else {
			detailsButton = null;
		}

		final StatisticsPanel statisticsPanel = new StatisticsPanel(getRemoteCollector(), counter,
				range, requestId != null);
		statisticsPanel.showRequestsAggregatedOrFilteredByClassName(requestId, detailsButton);
		add(statisticsPanel, BorderLayout.CENTER);

		add(createButtonsPanel(detailsButton), BorderLayout.SOUTH);
	}

	private JPanel createButtonsPanel(MButton detailsButton) {
		final MButton refreshButton = createRefreshButton();
		refreshButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				try {
					getRemoteCollector().collectDataIncludingCurrentRequests();
					refresh();
				} catch (final IOException ex) {
					showException(ex);
				}
			}
		});
		final MButton pdfButton = createPdfButton();
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

		final JPanel buttonsPanel = Utilities.createButtonsPanel();
		if (detailsButton != null) {
			buttonsPanel.add(detailsButton);
		}
		buttonsPanel.add(refreshButton);
		buttonsPanel.add(pdfButton);
		return buttonsPanel;
	}

	final void actionPdf() throws IOException {
		final File tempFile = createTempFileForPdf();
		final PdfOtherReport pdfOtherReport = createPdfOtherReport(tempFile);
		try {
			final Collector collector = getCollector();
			pdfOtherReport.writeCounterSummaryPerClass(collector, counter, requestId, range);
		} finally {
			pdfOtherReport.close();
		}
		Desktop.getDesktop().open(tempFile);
	}
}
