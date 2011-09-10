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
import java.awt.Component;
import java.awt.Desktop;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.List;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTable;
import javax.swing.SwingConstants;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import net.bull.javamelody.SessionInformations.SessionAttribute;
import net.bull.javamelody.swing.MButton;
import net.bull.javamelody.swing.Utilities;
import net.bull.javamelody.swing.table.MDateTableCellRenderer;
import net.bull.javamelody.swing.table.MDefaultTableCellRenderer;
import net.bull.javamelody.swing.table.MTable;
import net.bull.javamelody.swing.table.MTableScrollPane;
import net.bull.javamelody.swing.util.MSwingUtilities;

/**
 * Panel de la liste des sessions.
 * @author Emeric Vernat
 */
class SessionInformationsPanel extends MelodyPanel {
	private static final long serialVersionUID = 1L;

	@SuppressWarnings("all")
	private List<SessionInformations> sessionsInformations;
	private MTable<SessionInformations> table;
	private MTable<SessionAttribute> attributesTable;

	private class CountryTableCellRenderer extends MDefaultTableCellRenderer {
		private static final long serialVersionUID = 1L;

		CountryTableCellRenderer() {
			super();
			setHorizontalAlignment(SwingConstants.CENTER);
		}

		@Override
		public Component getTableCellRendererComponent(JTable jtable, Object value,
				boolean isSelected, boolean hasFocus, int row, int column) {
			// icône et tooltip correspondant au pays
			if (row == -1) {
				setIcon(null);
				setToolTipText(null);
			} else {
				final MTable<SessionInformations> myTable = getTable();
				final SessionInformations sessionInformations = myTable.getList().get(
						myTable.convertRowIndexToModel(row));
				final String country = sessionInformations.getCountry();
				if (country == null) {
					setIcon(null);
				} else {
					final String fileName = "flags/" + country + ".gif";
					if (getClass().getResource(Parameters.getResourcePath(fileName)) == null) {
						setIcon(null);
					} else {
						setIcon(ImageIconCache.getImageIcon(fileName));
					}
				}
				setToolTipText(sessionInformations.getCountryDisplay());
			}
			// sans texte
			return super.getTableCellRendererComponent(jtable, null, isSelected, hasFocus, row,
					column);
		}
	}

	SessionInformationsPanel(RemoteCollector remoteCollector) throws IOException {
		super(remoteCollector, new BorderLayout());

		refresh();
	}

	final void refresh() throws IOException {
		removeAll();

		this.sessionsInformations = getRemoteCollector().collectSessionInformations(null);
		this.table = new MTable<SessionInformations>();
		this.attributesTable = new MTable<SessionAttribute>();

		setName(I18N.getString("Sessions"));
		final JLabel titleLabel = Utilities.createParagraphTitle(I18N.getString("Sessions"),
				"system-users.png");
		add(titleLabel, BorderLayout.NORTH);

		addScrollPane();

		final JPanel southPanel = new JPanel(new BorderLayout());
		southPanel.setOpaque(false);
		southPanel.add(createButtonsPanel(), BorderLayout.NORTH);
		southPanel.add(createSummaryLabel(), BorderLayout.CENTER);
		southPanel.add(createAttributesPanel(), BorderLayout.SOUTH);
		add(southPanel, BorderLayout.SOUTH);
	}

	private void addScrollPane() {
		boolean displayUser = false;
		for (final SessionInformations sessionInformations : sessionsInformations) {
			if (sessionInformations.getRemoteUser() != null) {
				displayUser = true;
				break;
			}
		}
		final MTableScrollPane<SessionInformations> tableScrollPane = new MTableScrollPane<SessionInformations>(
				table);
		table.addColumn("id", I18N.getString("Session_id"));
		table.addColumn("lastAccess", I18N.getString("Dernier_acces"));
		table.addColumn("age", I18N.getString("Age"));
		table.addColumn("expirationDate", I18N.getString("Expiration"));
		table.addColumn("attributeCount", I18N.getString("Nb_attributs"));
		table.addColumn("serializable", I18N.getString("Serialisable"));
		table.addColumn("serializedSize", I18N.getString("Taille_serialisee"));
		table.addColumn("remoteAddr", I18N.getString("Adresse_IP"));
		table.addColumn("countryDisplay", I18N.getString("Pays"));

		if (displayUser) {
			table.addColumn("remoteUser", I18N.getString("Utilisateur"));
		}

		final MDateTableCellRenderer durationTableCellRenderer = new MDateTableCellRenderer();
		durationTableCellRenderer.setDateFormat(I18N.createDurationFormat());
		table.setColumnCellRenderer("lastAccess", durationTableCellRenderer);
		table.setColumnCellRenderer("age", durationTableCellRenderer);
		final MDateTableCellRenderer dateAndTimeTableCellRenderer = new MDateTableCellRenderer();
		dateAndTimeTableCellRenderer.setDateFormat(I18N.createDateAndTimeFormat());
		table.setColumnCellRenderer("expirationDate", dateAndTimeTableCellRenderer);
		table.setColumnCellRenderer("countryDisplay", new CountryTableCellRenderer());

		table.setList(sessionsInformations);

		add(tableScrollPane, BorderLayout.CENTER);
	}

	private JPanel createAttributesPanel() {
		final JPanel attributesPanel = new JPanel(new BorderLayout());
		attributesPanel.setOpaque(false);
		final JLabel attributesLabel = new JLabel(I18N.getString("Attributs"));
		attributesLabel.setFont(attributesLabel.getFont().deriveFont(Font.BOLD));
		attributesPanel.add(attributesLabel, BorderLayout.NORTH);
		final MTableScrollPane<SessionAttribute> attributesTableScrollPane = new MTableScrollPane<SessionAttribute>(
				attributesTable);
		attributesTable.addColumn("name", I18N.getString("Nom"));
		attributesTable.addColumn("type", I18N.getString("Type"));
		attributesTable.addColumn("serializable", I18N.getString("Serialisable"));
		attributesTable.addColumn("serializedSize", I18N.getString("Taille_serialisee"));
		attributesTable.addColumn("content", I18N.getString("Contenu"));
		attributesTable.setPreferredScrollableViewportSize(new Dimension(-1, 100));
		attributesPanel.add(attributesTableScrollPane, BorderLayout.CENTER);

		table.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
			@Override
			public void valueChanged(ListSelectionEvent e) {
				if (!e.getValueIsAdjusting()) {
					refreshAttributes();
				}
			}
		});

		return attributesPanel;
	}

	private JLabel createSummaryLabel() {
		long totalSerializedSize = 0;
		int nbSerializableSessions = 0;
		for (final SessionInformations sessionInformations : sessionsInformations) {
			final int size = sessionInformations.getSerializedSize();
			if (size >= 0) {
				totalSerializedSize += size;
				nbSerializableSessions++;
			}
		}
		final long meanSerializedSize;
		if (nbSerializableSessions > 0) {
			meanSerializedSize = totalSerializedSize / nbSerializableSessions;
		} else {
			meanSerializedSize = -1;
		}
		final JLabel summaryLabel = new JLabel("<html><div align='right'>"
				+ I18N.getFormattedString("nb_sessions", sessionsInformations.size()) + "<br/>"
				+ I18N.getFormattedString("taille_moyenne_sessions", meanSerializedSize));
		summaryLabel.setHorizontalAlignment(SwingConstants.RIGHT);
		return summaryLabel;
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
		final MButton invalidateAllSessionsButton = new MButton(
				I18N.getString("invalidate_sessions"), ImageIconCache.getScaledImageIcon(
						"user-trash.png", 16, 16));
		invalidateAllSessionsButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				if (confirm(I18N.getFormattedString("confirm_invalidate_sessions"))) {
					try {
						final String message = getRemoteCollector().executeActionAndCollectData(
								Action.INVALIDATE_SESSIONS, null, null, null, null);
						showMessage(message);
						refresh();
					} catch (final IOException ex) {
						showException(ex);
					}
				}
			}
		});
		final MButton invalidateSessionButton = new MButton(I18N.getString("invalidate_session"),
				ImageIconCache.getScaledImageIcon("user-trash.png", 16, 16));
		getTable().getSelectionModel().addListSelectionListener(new ListSelectionListener() {
			@Override
			public void valueChanged(ListSelectionEvent e) {
				final SessionInformations sessionInformations = getTable().getSelectedObject();
				invalidateSessionButton.setEnabled(sessionInformations != null);
			}
		});
		invalidateSessionButton.setEnabled(getTable().getSelectedObject() != null);
		invalidateSessionButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				final SessionInformations sessionInformations = getTable().getSelectedObject();
				if (sessionInformations != null
						&& confirm(I18N.getFormattedString("confirm_invalidate_session"))) {
					try {
						final String message = getRemoteCollector().executeActionAndCollectData(
								Action.INVALIDATE_SESSION, null, sessionInformations.getId(), null,
								null);
						showMessage(message);
						refresh();
					} catch (final IOException ex) {
						showException(ex);
					}
				}
			}
		});
		final JPanel buttonsPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
		buttonsPanel.setOpaque(false);
		buttonsPanel.add(refreshButton);
		buttonsPanel.add(pdfButton);
		buttonsPanel.add(invalidateAllSessionsButton);
		buttonsPanel.add(invalidateSessionButton);
		return buttonsPanel;
	}

	final void actionPdf() throws IOException {
		final File tempFile = createTempFileForPdf();
		final OutputStream output = new BufferedOutputStream(new FileOutputStream(tempFile));
		try {
			final PdfOtherReport pdfOtherReport = new PdfOtherReport(getRemoteCollector()
					.getApplication(), output);
			pdfOtherReport.writeSessionInformations(sessionsInformations);
		} finally {
			output.close();
		}
		Desktop.getDesktop().open(tempFile);
	}

	final void refreshAttributes() {
		getAttributesTable().setList(null);

		final SessionInformations sessionInformations = getTable().getSelectedObject();
		if (sessionInformations != null) {
			try {
				final List<SessionInformations> list = getRemoteCollector()
						.collectSessionInformations(sessionInformations.getId());
				if (list.isEmpty()) {
					final String message = I18N.getFormattedString("session_invalidee",
							sessionInformations.getId());
					MSwingUtilities.showMessage(this, message);
				} else {
					getAttributesTable().setList(list.get(0).getAttributes());
				}
			} catch (final IOException ex) {
				showException(ex);
			}
		}
	}

	MTable<SessionInformations> getTable() {
		return table;
	}

	MTable<SessionAttribute> getAttributesTable() {
		return attributesTable;
	}
}
