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
import java.awt.Component;
import java.awt.Desktop;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.io.Serializable;
import java.util.List;

import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTable;
import javax.swing.SwingConstants;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import net.bull.javamelody.internal.common.I18N;
import net.bull.javamelody.internal.common.Parameters;
import net.bull.javamelody.internal.model.Action;
import net.bull.javamelody.internal.model.RemoteCollector;
import net.bull.javamelody.internal.model.SessionInformations;
import net.bull.javamelody.internal.model.SessionInformations.SessionAttribute;
import net.bull.javamelody.internal.web.html.HtmlSessionInformationsReport;
import net.bull.javamelody.internal.web.pdf.PdfOtherReport;
import net.bull.javamelody.swing.MButton;
import net.bull.javamelody.swing.Utilities;
import net.bull.javamelody.swing.table.MDateTableCellRenderer;
import net.bull.javamelody.swing.table.MDefaultTableCellRenderer;
import net.bull.javamelody.swing.table.MTable;
import net.bull.javamelody.swing.table.MTableScrollPane;

/**
 * Panel de la liste des sessions.
 * @author Emeric Vernat
 */
class SessionInformationsPanel extends MelodyPanel {
	private static final ImageIcon INVALIDATE_SESSION_ICON = ImageIconCache
			.getScaledImageIcon("user-trash.png", 16, 16);

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
				final SessionInformations sessionInformations = myTable.getList()
						.get(myTable.convertRowIndexToModel(row));
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

	private class BrowserTableCellRenderer extends MDefaultTableCellRenderer {
		private static final long serialVersionUID = 1L;

		BrowserTableCellRenderer() {
			super();
			setHorizontalAlignment(SwingConstants.CENTER);
		}

		@Override
		public Component getTableCellRendererComponent(JTable jtable, Object value,
				boolean isSelected, boolean hasFocus, int row, int column) {
			// icône et tooltip correspondant au navigateur
			if (row == -1) {
				setIcon(null);
				setToolTipText(null);
			} else {
				final MTable<SessionInformations> myTable = getTable();
				final SessionInformations sessionInformations = myTable.getList()
						.get(myTable.convertRowIndexToModel(row));
				final String browser = sessionInformations.getBrowser();
				if (browser == null) {
					setIcon(null);
				} else {
					final String browserIconName = HtmlSessionInformationsReport
							.getBrowserIconName(browser);
					final String fileName = "browsers/" + browserIconName;
					if (getClass().getResource(Parameters.getResourcePath(fileName)) == null) {
						setIcon(null);
					} else {
						setIcon(ImageIconCache.getImageIcon(fileName));
					}
				}
				setToolTipText(browser);
			}
			// sans texte
			return super.getTableCellRendererComponent(jtable, null, isSelected, hasFocus, row,
					column);
		}
	}

	private class OsTableCellRenderer extends MDefaultTableCellRenderer {
		private static final long serialVersionUID = 1L;

		OsTableCellRenderer() {
			super();
			setHorizontalAlignment(SwingConstants.CENTER);
		}

		@Override
		public Component getTableCellRendererComponent(JTable jtable, Object value,
				boolean isSelected, boolean hasFocus, int row, int column) {
			// icône et tooltip correspondant au navigateur
			if (row == -1) {
				setIcon(null);
				setToolTipText(null);
			} else {
				final MTable<SessionInformations> myTable = getTable();
				final SessionInformations sessionInformations = myTable.getList()
						.get(myTable.convertRowIndexToModel(row));
				final String os = sessionInformations.getOs();
				if (os == null) {
					setIcon(null);
				} else {
					final String osIconName = HtmlSessionInformationsReport.getOSIconName(os);
					final String fileName = "servers/" + osIconName;
					if (getClass().getResource(Parameters.getResourcePath(fileName)) == null) {
						setIcon(null);
					} else {
						setIcon(ImageIconCache.getImageIcon(fileName));
					}
				}
				setToolTipText(os);
			}
			// sans texte
			return super.getTableCellRendererComponent(jtable, null, isSelected, hasFocus, row,
					column);
		}
	}

	SessionInformationsPanel(RemoteCollector remoteCollector) throws IOException {
		super(remoteCollector);

		refresh();
	}

	final void refresh() throws IOException {
		removeAll();

		this.sessionsInformations = getRemoteCollector().collectSessionInformations(null);
		this.attributesTable = new MTable<>();

		setName(getString("Sessions"));
		final JLabel titleLabel = Utilities.createParagraphTitle(getName(), "system-users.png");
		add(titleLabel, BorderLayout.NORTH);

		final MTableScrollPane<SessionInformations> scrollPane = createScrollPane();
		this.table = scrollPane.getTable();

		table.setList(sessionsInformations);

		add(scrollPane, BorderLayout.CENTER);

		final JPanel southPanel = new JPanel(new BorderLayout());
		southPanel.setOpaque(false);
		southPanel.add(createButtonsPanel(), BorderLayout.NORTH);
		southPanel.add(createSummaryLabel(), BorderLayout.CENTER);
		southPanel.add(createAttributesPanel(), BorderLayout.SOUTH);
		add(southPanel, BorderLayout.SOUTH);
	}

	private MTableScrollPane<SessionInformations> createScrollPane() {
		boolean displayUser = false;
		for (final SessionInformations sessionInformations : sessionsInformations) {
			if (sessionInformations.getRemoteUser() != null) {
				displayUser = true;
				break;
			}
		}
		final MTableScrollPane<SessionInformations> tableScrollPane = new MTableScrollPane<>();
		final MTable<SessionInformations> myTable = tableScrollPane.getTable();
		myTable.addColumn("id", getString("Session_id"));
		myTable.addColumn("lastAccess", getString("Dernier_acces"));
		myTable.addColumn("age", getString("Age"));
		myTable.addColumn("expirationDate", getString("Expiration"));
		myTable.addColumn("attributeCount", getString("Nb_attributs"));
		myTable.addColumn("serializable", getString("Serialisable"));
		myTable.addColumn("serializedSize", getString("Taille_serialisee"));
		myTable.addColumn("remoteAddr", getString("Adresse_IP"));
		myTable.addColumn("countryDisplay", getString("Pays"));
		myTable.addColumn("browser", getString("Navigateur"));
		myTable.addColumn("os", getString("OS"));

		if (displayUser) {
			myTable.addColumn("remoteUser", getString("Utilisateur"));
		}

		final MDateTableCellRenderer durationTableCellRenderer = new MDateTableCellRenderer();
		durationTableCellRenderer.setDateFormat(I18N.createDurationFormat());
		myTable.setColumnCellRenderer("lastAccess", durationTableCellRenderer);
		myTable.setColumnCellRenderer("age", durationTableCellRenderer);
		final MDateTableCellRenderer dateAndTimeTableCellRenderer = new MDateTableCellRenderer();
		dateAndTimeTableCellRenderer.setDateFormat(I18N.createDateAndTimeFormat());
		myTable.setColumnCellRenderer("expirationDate", dateAndTimeTableCellRenderer);
		myTable.setColumnCellRenderer("countryDisplay", new CountryTableCellRenderer());
		myTable.setColumnCellRenderer("browser", new BrowserTableCellRenderer());
		myTable.setColumnCellRenderer("os", new OsTableCellRenderer());

		return tableScrollPane;
	}

	private JPanel createAttributesPanel() {
		final JPanel attributesPanel = new JPanel(new BorderLayout());
		attributesPanel.setOpaque(false);
		final JLabel attributesLabel = new JLabel(getString("Attributs"));
		attributesLabel.setFont(attributesLabel.getFont().deriveFont(Font.BOLD));
		attributesPanel.add(attributesLabel, BorderLayout.NORTH);
		final MTableScrollPane<SessionAttribute> attributesTableScrollPane = new MTableScrollPane<>(
				attributesTable);
		attributesTable.addColumn("name", getString("Nom"));
		attributesTable.addColumn("type", getString("Type"));
		attributesTable.addColumn("serializable", getString("Serialisable"));
		attributesTable.addColumn("serializedSize", getString("Taille_serialisee"));
		attributesTable.addColumn("content", getString("Contenu"));
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
				+ getFormattedString("nb_sessions", sessionsInformations.size()) + "<br/>"
				+ getFormattedString("taille_moyenne_sessions", meanSerializedSize));
		summaryLabel.setHorizontalAlignment(SwingConstants.RIGHT);
		return summaryLabel;
	}

	private JPanel createButtonsPanel() {
		final MButton refreshButton = createRefreshButton();
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
		final MButton xmlJsonButton = createXmlJsonButton((Serializable) sessionsInformations);
		final MButton invalidateAllSessionsButton = createInvalidateAllSessionsButton();
		final MButton invalidateSessionButton = createInvalidateSessionButton();
		getTable().getSelectionModel().addListSelectionListener(new ListSelectionListener() {
			@Override
			public void valueChanged(ListSelectionEvent e) {
				final SessionInformations sessionInformations = getTable().getSelectedObject();
				invalidateSessionButton.setEnabled(sessionInformations != null);
			}
		});
		invalidateSessionButton.setEnabled(getTable().getSelectedObject() != null);
		return Utilities.createButtonsPanel(refreshButton, pdfButton, xmlJsonButton,
				invalidateAllSessionsButton, invalidateSessionButton);
	}

	private MButton createInvalidateAllSessionsButton() {
		final MButton invalidateAllSessionsButton = new MButton(getString("invalidate_sessions"),
				INVALIDATE_SESSION_ICON);
		invalidateAllSessionsButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				if (confirm(getFormattedString("confirm_invalidate_sessions"))) {
					actionInvalidateAllSessions();
				}
			}
		});
		return invalidateAllSessionsButton;
	}

	private MButton createInvalidateSessionButton() {
		final MButton invalidateSessionButton = new MButton(getString("invalidate_session"),
				INVALIDATE_SESSION_ICON);
		invalidateSessionButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				final SessionInformations sessionInformations = getTable().getSelectedObject();
				if (sessionInformations != null
						&& confirm(getFormattedString("confirm_invalidate_session"))) {
					actionInvalidateSession(sessionInformations);
				}
			}
		});
		return invalidateSessionButton;
	}

	final void actionInvalidateAllSessions() {
		try {
			final String message = getRemoteCollector().executeActionAndCollectData(
					Action.INVALIDATE_SESSIONS, null, null, null, null, null);
			showMessage(message);
			refresh();
		} catch (final IOException ex) {
			showException(ex);
		}
	}

	final void actionInvalidateSession(final SessionInformations sessionInformations) {
		try {
			final String message = getRemoteCollector().executeActionAndCollectData(
					Action.INVALIDATE_SESSION, null, sessionInformations.getId(), null, null, null);
			showMessage(message);
			refresh();
		} catch (final IOException ex) {
			showException(ex);
		}
	}

	final void actionPdf() throws IOException {
		final File tempFile = createTempFileForPdf();
		final PdfOtherReport pdfOtherReport = createPdfOtherReport(tempFile);
		try {
			pdfOtherReport.writeSessionInformations(sessionsInformations);
		} finally {
			pdfOtherReport.close();
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
					final String message = getFormattedString("session_invalidee",
							sessionInformations.getId());
					showMessage(message);
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
