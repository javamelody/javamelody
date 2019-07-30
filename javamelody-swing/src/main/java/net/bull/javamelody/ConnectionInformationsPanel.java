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
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import net.bull.javamelody.internal.common.I18N;
import net.bull.javamelody.internal.model.ConnectionInformations;
import net.bull.javamelody.internal.model.JavaInformations;
import net.bull.javamelody.internal.model.RemoteCollector;
import net.bull.javamelody.internal.model.ThreadInformations;
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
				final ConnectionInformations connectionInformations = myTable.getList()
						.get(myTable.convertRowIndexToModel(row));
				final String description = getDateFormat()
						.format(connectionInformations.getOpeningDate());
				final List<StackTraceElement> stackTrace = connectionInformations
						.getOpeningStackTrace();
				setToolTipText(
						ThreadInformationsPanel.convertStackTraceToHtml(description, stackTrace));
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
				final ConnectionInformations connectionInformations = myTable.getList()
						.get(myTable.convertRowIndexToModel(row));
				final ThreadInformations threadInformations = getThreadInformationsByConnectionInformations(
						connectionInformations);
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
		getRemoteCollector().collectDataIncludingCurrentRequests();

		final List<JavaInformations> javaInformationsList = getJavaInformationsList();
		for (final JavaInformations javaInformations : javaInformationsList) {
			if (javaInformations.isStackTraceEnabled()) {
				stackTraceEnabled = true;
				break;
			}
		}

		// on mélange toutes les connexions des éventuels différents serveurs ensemble mais c'est pas bien grave
		this.connectionInformationsList = new ArrayList<>();
		this.threadInformationsByConnectionInformations = new HashMap<>();
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

		setName(getString("Connexions_jdbc_ouvertes"));
		final JLabel titleLabel = Utilities.createParagraphTitle(getName(), "db.png");
		add(titleLabel, BorderLayout.NORTH);

		final JLabel introLabel = new JLabel(' ' + getString("connexions_intro"));
		final MTableScrollPane<ConnectionInformations> scrollPane = createScrollPane();
		this.table = scrollPane.getTable();
		table.setList(connectionInformationsList);

		final JPanel centerPanel = new JPanel(new BorderLayout());
		centerPanel.setOpaque(false);
		centerPanel.add(introLabel, BorderLayout.NORTH);
		centerPanel.add(scrollPane, BorderLayout.CENTER);
		add(centerPanel, BorderLayout.CENTER);

		final JLabel nbConnectionsLabel = new JLabel(
				getFormattedString("nb_connexions_ouvertes", connectionInformationsList.size()));
		nbConnectionsLabel.setHorizontalAlignment(SwingConstants.RIGHT);
		final JPanel southPanel = new JPanel(new BorderLayout());
		southPanel.setOpaque(false);
		southPanel.add(createButtonsPanel(), BorderLayout.NORTH);
		southPanel.add(nbConnectionsLabel, BorderLayout.CENTER);
		add(southPanel, BorderLayout.SOUTH);
	}

	private MTableScrollPane<ConnectionInformations> createScrollPane() {
		final MTableScrollPane<ConnectionInformations> tableScrollPane = new MTableScrollPane<>();
		final MTable<ConnectionInformations> myTable = tableScrollPane.getTable();
		myTable.addColumn("openingDate", getString("Date_et_stack_trace_ouverture"));
		if (stackTraceEnabled) {
			myTable.addColumn("threadId", getString("Thread_et_stack_trace_actuelle"));
		} else {
			myTable.addColumn("threadId", getString("Thread"));
		}
		myTable.setColumnCellRenderer("openingDate", new OpeningDateTableCellRenderer());
		myTable.setColumnCellRenderer("threadId", new ThreadTableCellRenderer());

		return tableScrollPane;
	}

	final void showStackTraceInPopup(ConnectionInformations connectionInformations,
			ThreadInformations threadInformations) {
		final StringBuilder sb = new StringBuilder();
		sb.append(getString("Date_et_stack_trace_ouverture"));
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
			sb.append(getString("Thread_et_stack_trace_actuelle"));
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
		final MButton openButton = new MButton(getString("Ouvrir"),
				ImageIconCache.getImageIcon("action_open.png"));
		final MButton refreshButton = createRefreshButton();

		openButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				final ConnectionInformations connectionInformations = getTable()
						.getSelectedObject();
				final ThreadInformations threadInformations = getThreadInformationsByConnectionInformations(
						connectionInformations);
				showStackTraceInPopup(connectionInformations, threadInformations);
			}
		});
		table.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseClicked(MouseEvent e) {
				if (e.getClickCount() == 2) {
					openButton.doClick();
				}
			}
		});
		table.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
			@Override
			public void valueChanged(ListSelectionEvent e) {
				final ConnectionInformations connectionInformations = getTable()
						.getSelectedObject();
				openButton.setEnabled(connectionInformations != null);
			}
		});
		openButton.setEnabled(false);

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
		return Utilities.createButtonsPanel(openButton, refreshButton);
	}

	final MTable<ConnectionInformations> getTable() {
		return table;
	}

	final ThreadInformations getThreadInformationsByConnectionInformations(
			ConnectionInformations connectionInformations) {
		return threadInformationsByConnectionInformations.get(connectionInformations);
	}
}
