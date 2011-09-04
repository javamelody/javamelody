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
import java.awt.Color;
import java.awt.Component;
import java.awt.FlowLayout;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.IOException;
import java.util.List;

import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextArea;
import javax.swing.SwingUtilities;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import net.bull.javamelody.table.MDefaultTableCellRenderer;
import net.bull.javamelody.table.MTable;
import net.bull.javamelody.table.MTableScrollPane;
import net.bull.javamelody.util.MSwingUtilities;

/**
 * Panel des threads.
 * @author Emeric Vernat
 */
class ThreadInformationsPanel extends JPanel {
	private static final long serialVersionUID = 1L;

	@SuppressWarnings("all")
	private final RemoteCollector remoteCollector;
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
				final List<StackTraceElement> stackTrace = threadInformations.getStackTrace();
				if (stackTrace != null && !stackTrace.isEmpty()) {
					// même si stackTraceEnabled, ce thread n'a pas forcément de stack-trace
					final StringBuilder sb = new StringBuilder();
					sb.append("<html>");
					sb.append(threadInformations.getName());
					sb.append("<br/>");
					for (final StackTraceElement stackTraceElement : stackTrace) {
						sb.append(stackTraceElement);
						sb.append("<br/>");
					}
					setToolTipText(sb.toString());
				} else {
					setToolTipText(null);
				}
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
		super(new BorderLayout());
		assert remoteCollector != null;
		assert threadInformationsList != null;
		this.remoteCollector = remoteCollector;
		this.threadInformationsList = threadInformationsList;
		this.stackTraceEnabled = stackTraceEnabled;
		this.cpuTimeEnabled = !threadInformationsList.isEmpty()
				&& threadInformationsList.get(0).getCpuTimeMillis() != -1;
		this.table = new MTable<ThreadInformations>();

		setOpaque(false);

		addScrollPane();

		final JLabel label = new JLabel(' ' + I18N.getString("Temps_threads"));
		add(label, BorderLayout.WEST);

		if (Parameters.isSystemActionsEnabled()) {
			addButton();
		}
	}

	private void addScrollPane() {
		final MTableScrollPane<ThreadInformations> tableScrollPane = new MTableScrollPane<ThreadInformations>(
				table);
		table.addColumn("name", I18N.getString("Thread"));
		table.addColumn("daemon", I18N.getString("Demon"));
		table.addColumn("priority", I18N.getString("Priorite"));
		table.addColumn("state", I18N.getString("Etat"));
		if (stackTraceEnabled) {
			table.addColumn("executedMethod", I18N.getString("Methode_executee"));
		}
		if (cpuTimeEnabled) {
			table.addColumn("cpuTimeMillis", I18N.getString("Temps_cpu"));
			table.addColumn("userTimeMillis", I18N.getString("Temps_user"));
		}
		final MTable<ThreadInformations> myTable = table;
		table.setColumnCellRenderer("state", new StateTableCellRenderer());

		table.setColumnCellRenderer("name", new NameTableCellRenderer());

		table.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseClicked(MouseEvent e) {
				if (e.getClickCount() == 2) {
					final ThreadInformations threadInformations = myTable.getSelectedObject();
					showStackTraceInPopup(threadInformations);
				}
			}
		});

		table.setList(threadInformationsList);
		Utilities.adjustTableHeight(table);

		add(tableScrollPane, BorderLayout.NORTH);
	}

	private void addButton() {
		final MButton killThreadButton = new MButton(I18N.getString("Tuer"),
				ImageIconCache.getImageIcon("stop.png"));
		final MTable<ThreadInformations> myTable = table;
		myTable.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
			@Override
			public void valueChanged(ListSelectionEvent e) {
				final ThreadInformations threadInformations = myTable.getSelectedObject();
				killThreadButton.setEnabled(threadInformations != null);
				if (threadInformations != null) {
					killThreadButton.setToolTipText(I18N.getFormattedString("kill_thread",
							threadInformations.getName()));
				} else {
					killThreadButton.setToolTipText(null);
				}
			}
		});
		killThreadButton.setEnabled(myTable.getSelectedObject() != null);
		killThreadButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				final ThreadInformations threadInformations = myTable.getSelectedObject();
				if (threadInformations != null
						&& confirm(I18N.getFormattedString("confirm_kill_thread",
								threadInformations.getName()))) {
					try {
						// TODO refresh
						final String message = getRemoteCollector().executeActionAndCollectData(
								Action.KILL_THREAD, null, null,
								threadInformations.getGlobalThreadId(), null);
						showMessage(message);
					} catch (final IOException ex) {
						MSwingUtilities.showException(ex);
					}
				}
			}
		});
		final JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
		buttonPanel.setOpaque(false);
		buttonPanel.add(killThreadButton);
		add(buttonPanel, BorderLayout.EAST);
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
			showTextInPopup(this, title, text);
		}
	}

	static void showTextInPopup(Component component, String title, String text) {
		final JTextArea textArea = new JTextArea();
		textArea.setText(text);
		textArea.setEditable(false);
		// background nécessaire avec la plupart des look and feels dont Nimbus,
		// sinon il reste blanc malgré editable false
		textArea.setBackground(Color.decode("#E7E7E7"));
		final JScrollPane scrollPane = new JScrollPane(textArea);
		final JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT, 0, 5));
		buttonPanel.setOpaque(false);
		// TODO traduction
		final MButton clipBoardButton = new MButton("Copier dans presse-papiers");
		clipBoardButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				textArea.selectAll();
				textArea.copy();
				textArea.setCaretPosition(0);
			}
		});
		buttonPanel.add(clipBoardButton);
		final Window window = SwingUtilities.getWindowAncestor(component);
		final JDialog dialog = new JDialog((JFrame) window, title, true);
		final JPanel contentPane = new JPanel(new BorderLayout());
		contentPane.add(scrollPane, BorderLayout.CENTER);
		contentPane.add(buttonPanel, BorderLayout.SOUTH);
		dialog.setContentPane(contentPane);
		dialog.pack();
		dialog.setLocationRelativeTo(window);
		dialog.setVisible(true);
	}

	MTable<ThreadInformations> getTable() {
		return table;
	}

	final boolean confirm(String message) {
		return MSwingUtilities.showConfirmation(this, message);
	}

	final void showMessage(final String message) {
		MSwingUtilities.showMessage(this, message);
	}

	RemoteCollector getRemoteCollector() {
		return remoteCollector;
	}
}
