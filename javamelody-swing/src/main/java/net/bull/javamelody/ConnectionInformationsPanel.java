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
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTable;
import javax.swing.SwingConstants;

import net.bull.javamelody.swing.MButton;
import net.bull.javamelody.swing.Utilities;
import net.bull.javamelody.swing.table.MDateTableCellRenderer;
import net.bull.javamelody.swing.table.MDefaultTableCellRenderer;
import net.bull.javamelody.swing.table.MTable;
import net.bull.javamelody.swing.table.MTableScrollPane;

/**
 * Panel de la liste des connexions jdbc.
 * @author Emeric Vernat
 */
class ConnectionInformationsPanel extends MelodyPanel {
	private static final long serialVersionUID = 1L;

	@SuppressWarnings("all")
	private List<ConnectionInformations> connectionInformationsList;
	private boolean stackTraceEnabled;
	@SuppressWarnings("all")
	private Map<ConnectionInformations, ThreadInformations> threadInformationsByConnectionInformations;
	private MTable<ConnectionInformations> table;

	private class OpeningDateTableCellRenderer extends MDateTableCellRenderer {
		private static final long serialVersionUID = 1L;

		OpeningDateTableCellRenderer() {
			super();
			setDateFormat(I18N.createDateAndTimeFormat());
		}

		@Override
		public Component getTableCellRendererComponent(JTable jtable, Object value,
				boolean isSelected, boolean hasFocus, int row, int column) {
			// tooltip selon la stack-trace
			if (row == -1) {
				setToolTipText(null);
			} else {
				final MTable<ConnectionInformations> myTable = getTable();
				final ConnectionInformations connectionInformations = myTable.getList().get(
						myTable.convertRowIndexToModel(row));
				final String description = getDateFormat().format(
						connectionInformations.getOpeningDate());
				final List<StackTraceElement> stackTrace = connectionInformations
						.getOpeningStackTrace();
				setToolTipText(ThreadInformationsPanel.convertStackTraceToHtml(description,
						stackTrace));
			}
			// et texte selon la valeur (nom du thread)
			return super.getTableCellRendererComponent(jtable, value, isSelected, hasFocus, row,
					column);
		}
	}

	private class ThreadTableCellRenderer extends MDefaultTableCellRenderer {
		private static final long serialVersionUID = 1L;

		ThreadTableCellRenderer() {
			super();
		}

		@Override
		public Component getTableCellRendererComponent(JTable jtable, Object value,
				boolean isSelected, boolean hasFocus, int row, int column) {
			// tooltip selon la stack-trace
			final String text;
			if (row == -1) {
				setToolTipText(null);
				text = null;
			} else {
				final MTable<ConnectionInformations> myTable = getTable();
				final ConnectionInformations connectionInformations = myTable.getList().get(
						myTable.convertRowIndexToModel(row));
				final ThreadInformations threadInformations = getThreadInformationsByConnectionInformations(connectionInformations);
				if (threadInformations != null) {
					text = threadInformations.getName();
					setToolTipText(ThreadInformationsPanel.convertStackTraceToHtml(
							threadInformations.getName(), threadInformations.getStackTrace()));
				} else {
					setToolTipText(null);
					text = null;
				}
			}
			// et texte selon la valeur (nom du thread)
			return super.getTableCellRendererComponent(jtable, text, isSelected, hasFocus, row,
					column);
		}
	}

	ConnectionInformationsPanel(RemoteCollector remoteCollector) throws IOException {
		super(remoteCollector);

		refresh();
	}

	final void refresh() throws IOException {
		removeAll();

		final List<List<ConnectionInformations>> allConnectionInformations = getRemoteCollector()
				.collectConnectionInformations();
		// collecte aussi les data pour avoir des stack-traces de threads à jour
		getRemoteCollector().collectData();

		final List<JavaInformations> javaInformationsList = getRemoteCollector()
				.getJavaInformationsList();
		for (final JavaInformations javaInformations : javaInformationsList) {
			if (javaInformations.isStackTraceEnabled()) {
				stackTraceEnabled = true;
				break;
			}
		}

		// on mélange toutes les connexions des éventuels différents serveurs ensemble mais c'est pas bien grave
		this.connectionInformationsList = new ArrayList<ConnectionInformations>();
		this.threadInformationsByConnectionInformations = new HashMap<ConnectionInformations, ThreadInformations>();
		int i = 0;
		for (final List<ConnectionInformations> connections : allConnectionInformations) {
			connectionInformationsList.addAll(connections);

			// on recherche les infos courantes sur le thread ayant ouvert la connexion
			// (attention : l'id du thread n'est pas forcément unique s'il y a plusieurs JVM)
			final JavaInformations javaInformations = javaInformationsList.get(i);
			for (final ConnectionInformations connectionInformations : connections) {
				for (final ThreadInformations threadInformations : javaInformations
						.getThreadInformationsList()) {
					if (connectionInformations.getThreadId() == threadInformations.getId()) {
						threadInformationsByConnectionInformations.put(connectionInformations,
								threadInformations);
					}
				}
			}
			i++;
		}

		setName(I18N.getString("Connexions_jdbc_ouvertes"));
		final JLabel titleLabel = Utilities.createParagraphTitle(
				I18N.getString("Connexions_jdbc_ouvertes"), "db.png");
		add(titleLabel, BorderLayout.NORTH);

		final JLabel introLabel = new JLabel(' ' + I18N.getString("connexions_intro"));
		final MTableScrollPane<ConnectionInformations> scrollPane = createScrollPane();
		this.table = scrollPane.getTable();
		table.setList(connectionInformationsList);

		final JPanel centerPanel = new JPanel(new BorderLayout());
		centerPanel.setOpaque(false);
		centerPanel.add(introLabel, BorderLayout.NORTH);
		centerPanel.add(scrollPane, BorderLayout.CENTER);
		add(centerPanel, BorderLayout.CENTER);

		final JLabel nbConnectionsLabel = new JLabel(I18N.getFormattedString(
				"nb_connexions_ouvertes", connectionInformationsList.size()));
		nbConnectionsLabel.setHorizontalAlignment(SwingConstants.RIGHT);
		final JPanel southPanel = new JPanel(new BorderLayout());
		southPanel.setOpaque(false);
		southPanel.add(createButtonsPanel(), BorderLayout.NORTH);
		southPanel.add(nbConnectionsLabel, BorderLayout.CENTER);
		add(southPanel, BorderLayout.SOUTH);
	}

	private MTableScrollPane<ConnectionInformations> createScrollPane() {
		final MTableScrollPane<ConnectionInformations> tableScrollPane = new MTableScrollPane<ConnectionInformations>();
		final MTable<ConnectionInformations> myTable = tableScrollPane.getTable();
		myTable.addColumn("openingDate", I18N.getString("Date_et_stack_trace_ouverture"));
		if (stackTraceEnabled) {
			myTable.addColumn("threadId", I18N.getString("Thread_et_stack_trace_actuelle"));
		} else {
			myTable.addColumn("threadId", I18N.getString("Thread"));
		}
		myTable.setColumnCellRenderer("openingDate", new OpeningDateTableCellRenderer());
		myTable.setColumnCellRenderer("threadId", new ThreadTableCellRenderer());

		myTable.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseClicked(MouseEvent e) {
				if (e.getClickCount() == 2) {
					final ConnectionInformations connectionInformations = getTable()
							.getSelectedObject();
					final ThreadInformations threadInformations = getThreadInformationsByConnectionInformations(connectionInformations);
					showStackTraceInPopup(connectionInformations, threadInformations);
				}
			}
		});

		return tableScrollPane;
	}

	final void showStackTraceInPopup(ConnectionInformations connectionInformations,
			ThreadInformations threadInformations) {
		final StringBuilder sb = new StringBuilder();
		sb.append(I18N.getString("Date_et_stack_trace_ouverture"));
		sb.append(": ");
		sb.append(I18N.createDateAndTimeFormat().format(connectionInformations.getOpeningDate()));
		sb.append('\n');
		for (final StackTraceElement stackTraceElement : connectionInformations
				.getOpeningStackTrace()) {
			sb.append(stackTraceElement);
			sb.append('\n');
		}
		sb.append('\n');
		if (threadInformations != null && stackTraceEnabled) {
			sb.append(I18N.getString("Thread_et_stack_trace_actuelle"));
			sb.append(": ");
			final List<StackTraceElement> stackTrace = threadInformations.getStackTrace();
			if (stackTrace != null && !stackTrace.isEmpty()) {
				// même si stackTraceEnabled, ce thread n'a pas forcément de stack-trace
				sb.append(threadInformations.getName());
				sb.append('\n');
				for (final StackTraceElement stackTraceElement : stackTrace) {
					sb.append(stackTraceElement);
					sb.append('\n');
				}
				final String title = threadInformations.getName();
				final String text = sb.toString();
				Utilities.showTextInPopup(this, title, text);
			}
		}
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
		return Utilities.createButtonsPanel(refreshButton);
	}

	final MTable<ConnectionInformations> getTable() {
		return table;
	}

	final ThreadInformations getThreadInformationsByConnectionInformations(
			ConnectionInformations connectionInformations) {
		return threadInformationsByConnectionInformations.get(connectionInformations);
	}
}
