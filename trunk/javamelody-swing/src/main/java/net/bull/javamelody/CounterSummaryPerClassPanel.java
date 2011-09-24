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
import java.awt.Desktop;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;

import javax.swing.JPanel;

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

		final String counterLabel = I18N.getString(counter.getName() + "Label");
		final String title = I18N.getFormattedString("Statistiques_compteur", counterLabel) + " - "
				+ range.getLabel();
		setName(title);
		add(Utilities.createParagraphTitle(title, counter.getIconName()), BorderLayout.NORTH);

		final MButton detailsButton;
		if (requestId == null) {
			detailsButton = new MButton(I18N.getString("Details"));
		} else {
			detailsButton = null;
		}

		final StatisticsPanel statisticsPanel = new StatisticsPanel(getRemoteCollector(), counter,
				range);
		statisticsPanel.showRequestsAggregatedOrFilteredByClassName(requestId, detailsButton);
		add(statisticsPanel, BorderLayout.CENTER);

		add(createButtonsPanel(detailsButton), BorderLayout.SOUTH);
	}

	private JPanel createButtonsPanel(MButton detailsButton) {
		final MButton refreshButton = new MButton(I18N.getString("Actualiser"),
				ImageIconCache.getImageIcon("action_refresh.png"));
		refreshButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				try {
					getRemoteCollector().collectData();
					refresh();
				} catch (final IOException ex) {
					showException(ex);
				}
			}
		});
		final MButton pdfButton = new MButton(I18N.getString("PDF"),
				ImageIconCache.getImageIcon("pdf.png"));
		pdfButton.setToolTipText(I18N.getString("afficher_PDF"));
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
		final OutputStream output = createFileOutputStream(tempFile);
		try {
			final PdfOtherReport pdfOtherReport = new PdfOtherReport(getRemoteCollector()
					.getApplication(), output);
			final Collector collector = getRemoteCollector().getCollector();
			pdfOtherReport.writeCounterSummaryPerClass(collector, counter, requestId, range);
		} finally {
			output.close();
		}
		Desktop.getDesktop().open(tempFile);
	}
}
