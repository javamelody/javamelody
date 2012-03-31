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
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.IOException;
import java.util.List;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTable;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import net.bull.javamelody.swing.MButton;
import net.bull.javamelody.swing.Utilities;
import net.bull.javamelody.swing.table.MDefaultTableCellRenderer;
import net.bull.javamelody.swing.table.MTable;
import net.bull.javamelody.swing.table.MTableScrollPane;

/**
 * Panel des threads.
 * @author Emeric Vernat
 */
class ThreadInformationsPanel extends MelodyPanel {
	private static final long serialVersionUID = 1L;

	@SuppressWarnings("all")
	private final List<ThreadInformations> threadInformationsList;
	private final boolean stackTraceEnabled;
	private final boolean cpuTimeEnabled;
	private final MTable<ThreadInformations> table;

	private class NameTableCellRenderer extends MDefaultTableCellRenderer {
		private static final long serialVersionUID = 1L;

		NameTableCellRenderer() {
			super();
		}

		@Override
		public Component getTableCellRendererComponent(JTable jtable, Object value,
				boolean isSelected, boolean hasFocus, int row, int column) {
			// tooltip selon la stack-trace
			if (row == -1) {
				setToolTipText(null);
			} else {
				final MTable<ThreadInformations> myTable = getTable();
				final ThreadInformations threadInformations = myTable.getList().get(
						myTable.convertRowIndexToModel(row));
				setToolTipText(convertStackTraceToHtml(threadInformations.getName(),
						threadInformations.getStackTrace()));
			}
			// et texte selon la valeur (nom du thread)
			return super.getTableCellRendererComponent(jtable, value, isSelected, hasFocus, row,
					column);
		}
	}

	private class StateTableCellRenderer extends MDefaultTableCellRenderer {
		private static final long serialVersionUID = 1L;

		StateTableCellRenderer() {
			super();
		}

		@Override
		public Component getTableCellRendererComponent(JTable jtable, Object value,
				boolean isSelected, boolean hasFocus, int row, int column) {
			// icône correspondant à l'état
			if (row == -1) {
				setIcon(null);
			} else {
				final MTable<ThreadInformations> myTable = getTable();
				final ThreadInformations threadInformations = myTable.getList().get(
						myTable.convertRowIndexToModel(row));
				setIcon(ImageIconCache.getImageIcon("bullets/"
						+ HtmlThreadInformationsReport.getStateIcon(threadInformations)));
			}
			// et texte selon la valeur (libellé de l'état)
			return super.getTableCellRendererComponent(jtable, value, isSelected, hasFocus, row,
					column);
		}
	}

	ThreadInformationsPanel(RemoteCollector remoteCollector,
			List<ThreadInformations> threadInformationsList, boolean stackTraceEnabled) {
		super(remoteCollector);
		assert threadInformationsList != null;
		this.threadInformationsList = threadInformationsList;
		this.stackTraceEnabled = stackTraceEnabled;
		this.cpuTimeEnabled = !threadInformationsList.isEmpty()
				&& threadInformationsList.get(0).getCpuTimeMillis() != -1;

		final MTableScrollPane<ThreadInformations> scrollPane = createScrollPane();
		this.table = scrollPane.getTable();
		table.setList(threadInformationsList);
		Utilities.adjustTableHeight(table);

		add(scrollPane, BorderLayout.NORTH);

		final JLabel label = new JLabel(' ' + I18N.getString("Temps_threads"));
		add(label, BorderLayout.WEST);

		final JPanel buttonsPanel = createButtonsPanel();
		add(buttonsPanel, BorderLayout.EAST);
	}

	private MTableScrollPane<ThreadInformations> createScrollPane() {
		final MTableScrollPane<ThreadInformations> tableScrollPane = new MTableScrollPane<ThreadInformations>();
		final MTable<ThreadInformations> myTable = tableScrollPane.getTable();
		myTable.addColumn("name", I18N.getString("Thread"));
		myTable.addColumn("daemon", I18N.getString("Demon"));
		myTable.addColumn("priority", I18N.getString("Priorite"));
		myTable.addColumn("state", I18N.getString("Etat"));
		if (stackTraceEnabled) {
			myTable.addColumn("executedMethod", I18N.getString("Methode_executee"));
		}
		if (cpuTimeEnabled) {
			myTable.addColumn("cpuTimeMillis", I18N.getString("Temps_cpu"));
			myTable.addColumn("userTimeMillis", I18N.getString("Temps_user"));
		}
		myTable.setColumnCellRenderer("state", new StateTableCellRenderer());

		myTable.setColumnCellRenderer("name", new NameTableCellRenderer());

		myTable.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseClicked(MouseEvent e) {
				if (e.getClickCount() == 2) {
					final ThreadInformations threadInformations = getTable().getSelectedObject();
					showStackTraceInPopup(threadInformations);
				}
			}
		});

		return tableScrollPane;
	}

	private JPanel createButtonsPanel() {
		final JPanel buttonsPanel = Utilities.createButtonsPanel();

		if (Parameters.isSystemActionsEnabled()) {
			final MButton killThreadButton = new MButton(I18N.getString("Tuer"),
					ImageIconCache.getImageIcon("stop.png"));
			getTable().getSelectionModel().addListSelectionListener(new ListSelectionListener() {
				@Override
				public void valueChanged(ListSelectionEvent e) {
					final ThreadInformations threadInformations = getTable().getSelectedObject();
					killThreadButton.setEnabled(threadInformations != null);
					if (threadInformations != null) {
						killThreadButton.setToolTipText(I18N.getFormattedString("kill_thread",
								threadInformations.getName()));
					} else {
						killThreadButton.setToolTipText(null);
					}
				}
			});
			killThreadButton.setEnabled(getTable().getSelectedObject() != null);
			killThreadButton.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					final ThreadInformations threadInformations = getTable().getSelectedObject();
					if (threadInformations != null
							&& confirm(I18N.getFormattedString("confirm_kill_thread",
									threadInformations.getName()))) {
						actionKillThread(threadInformations);
					}
				}
			});

			buttonsPanel.add(killThreadButton);
		}

		if (stackTraceEnabled) {
			final MButton dumpThreadsButton = new MButton(I18N.getString("Dump_threads_en_texte"),
					ImageIconCache.getImageIcon("text.png"));
			dumpThreadsButton.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					actionDumpThreads();
				}
			});
			buttonsPanel.add(dumpThreadsButton);
		}

		return buttonsPanel;
	}

	final void actionKillThread(final ThreadInformations threadInformations) {
		try {
			final String message = getRemoteCollector().executeActionAndCollectData(
					Action.KILL_THREAD, null, null, threadInformations.getGlobalThreadId(), null);
			showMessage(message);
			MainPanel.refreshMainTabFromChild(this);
		} catch (final IOException ex) {
			showException(ex);
		}
	}

	final void actionDumpThreads() {
		try {
			final ThreadsDumpPanel panel = new ThreadsDumpPanel(getRemoteCollector(),
					threadInformationsList);
			MainPanel.addOngletFromChild(this, panel);
		} catch (final IOException ex) {
			showException(ex);
		}
	}

	final void showStackTraceInPopup(ThreadInformations threadInformations) {
		final List<StackTraceElement> stackTrace = threadInformations.getStackTrace();
		if (stackTrace != null && !stackTrace.isEmpty()) {
			// même si stackTraceEnabled, ce thread n'a pas forcément de stack-trace
			final StringBuilder sb = new StringBuilder();
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

	static String convertStackTraceToHtml(String description, List<StackTraceElement> stackTrace) {
		if (stackTrace != null && !stackTrace.isEmpty()) {
			// même si stackTraceEnabled, ce thread n'a pas forcément de stack-trace
			final StringBuilder sb = new StringBuilder();
			sb.append("<html>");
			sb.append(description);
			sb.append("<br/>");
			for (final StackTraceElement stackTraceElement : stackTrace) {
				sb.append(stackTraceElement);
				sb.append("<br/>");
			}
			return sb.toString();
		}
		return null;
	}

	MTable<ThreadInformations> getTable() {
		return table;
	}
}
