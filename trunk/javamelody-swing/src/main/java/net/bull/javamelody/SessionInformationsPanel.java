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
import java.io.IOException;
import java.util.List;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTable;
import javax.swing.SwingConstants;

import net.bull.javamelody.table.MDateTableCellRenderer;
import net.bull.javamelody.table.MDefaultTableCellRenderer;
import net.bull.javamelody.table.MTable;
import net.bull.javamelody.table.MTableScrollPane;

/**
 * Panel de la liste des sessions.
 * @author Emeric Vernat
 */
class SessionInformationsPanel extends JPanel {
	private static final long serialVersionUID = 1L;

	@SuppressWarnings("all")
	private final List<SessionInformations> sessionsInformations;
	private final MTable<SessionInformations> table;

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
		super(new BorderLayout());
		assert remoteCollector != null;
		this.sessionsInformations = remoteCollector.collectSessionInformations(null);
		this.table = new MTable<SessionInformations>();

		setOpaque(false);

		// TODO
		//		writeBackAndRefreshLinks();
		//		writeln("<br/>");
		//
		//		if (sessionsInformations.isEmpty()) {
		//			writeln("#Aucune_session#");
		//			return;
		//		}

		final JLabel titleLabel = Utilities.createParagraphTitle(I18N.getString("Sessions"),
				"system-users.png");
		add(titleLabel, BorderLayout.NORTH);

		addScrollPane();
		//	TODO	write("<th class='noPrint'>#Invalider#</th>");
		// TODO invalider toutes les sessions

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
		add(summaryLabel, BorderLayout.SOUTH);
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

	MTable<SessionInformations> getTable() {
		return table;
	}
}
