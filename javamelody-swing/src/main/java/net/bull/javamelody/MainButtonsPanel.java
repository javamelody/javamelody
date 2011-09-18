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
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.URI;
import java.net.URL;
import java.util.List;

import javax.swing.JLabel;

import net.bull.javamelody.swing.MButton;

/**
 * Panel des boutons principaux.
 * @author Emeric Vernat
 */
class MainButtonsPanel extends MelodyPanel {
	private static final long serialVersionUID = 1L;

	MainButtonsPanel(RemoteCollector remoteCollector, final URL monitoringUrl) {
		super(remoteCollector, new FlowLayout(FlowLayout.CENTER));

		final MButton refreshButton = new MButton(I18N.getString("Actualiser"),
				ImageIconCache.getImageIcon("action_refresh.png"));
		refreshButton.setToolTipText(I18N.getString("Rafraichir"));
		final MButton pdfButton = new MButton(I18N.getString("PDF"),
				ImageIconCache.getImageIcon("pdf.png"));
		pdfButton.setToolTipText(I18N.getString("afficher_PDF"));
		final MButton onlineHelpButton = new MButton(I18N.getString("Aide_en_ligne"),
				ImageIconCache.getImageIcon("action_help.png"));
		onlineHelpButton.setToolTipText(I18N.getString("Afficher_aide_en_ligne"));
		// TODO traductions
		final MButton monitoringButton = new MButton("Monitoring",
				ImageIconCache.getScaledImageIcon("systemmonitor.png", 16, 16));
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
			// TODO ajouter listener
		}

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
									+ I18N.getString("help_url")));
				} catch (final Exception ex) {
					showException(ex);
				}
			}
		});
	}

	void actionRefresh() {
		try {
			getRemoteCollector().collectData();
			MainPanel.refreshMainTabFromChild(this);
		} catch (final IOException e) {
			showException(e);
		}
	}

	void actionPdf() throws IOException {
		// ici on prend un comportement similaire au serveur de collecte
		// en ne mettant pas les requêtes en cours puisque de toute façon on n'a pas
		// les données dans les counters
		final boolean collectorServer = true;
		// TODO récupérer range sélectionné
		final Range range = Period.TOUT.getRange();
		final File tempFile = createTempFileForPdf();
		final OutputStream output = new BufferedOutputStream(new FileOutputStream(tempFile));
		try {
			final Collector collector = getRemoteCollector().getCollector();
			final List<JavaInformations> javaInformationsList = getRemoteCollector()
					.getJavaInformationsList();
			final PdfReport pdfReport = new PdfReport(collector, collectorServer,
					javaInformationsList, range, output);
			pdfReport.toPdf();
		} finally {
			output.close();
		}
		Desktop.getDesktop().open(tempFile);
	}
}
