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
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
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

	private final transient List<ThreadInformations> threadInformationsList;
	private final boolean stackTraceEnabled;
	private final boolean cpuTimeEnabled;
	private final MTable<ThreadInformations> table;

	ThreadInformationsPanel(List<ThreadInformations> threadInformationsList,
			boolean stackTraceEnabled) {
		super(new BorderLayout());
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
		final MDefaultTableCellRenderer stateTableCellRenderer = new MDefaultTableCellRenderer() {
			private static final long serialVersionUID = 1L;

			@Override
			public Component getTableCellRendererComponent(JTable jtable, Object value,
					boolean isSelected, boolean hasFocus, int row, int column) {
				// icône correspondant à l'état
				if (row == -1) {
					setIcon(null);
				} else {
					final ThreadInformations threadInformations = myTable.getList().get(row);
					setIcon(ImageIconCache.getImageIcon("bullets/"
							+ HtmlThreadInformationsReport.getStateIcon(threadInformations)));
				}
				// et texte selon la valeur (libellé de l'état)
				return super.getTableCellRendererComponent(jtable, value, isSelected, hasFocus,
						row, column);
			}
		};
		table.setColumnCellRenderer("state", stateTableCellRenderer);

		final MDefaultTableCellRenderer nameTableCellRenderer = new MDefaultTableCellRenderer() {
			private static final long serialVersionUID = 1L;

			@Override
			public Component getTableCellRendererComponent(JTable jtable, Object value,
					boolean isSelected, boolean hasFocus, int row, int column) {
				// tooltip selon la stack-trace
				if (row == -1) {
					setToolTipText(null);
				} else {
					final ThreadInformations threadInformations = myTable.getList().get(row);
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
				return super.getTableCellRendererComponent(jtable, value, isSelected, hasFocus,
						row, column);
			}
		};
		table.setColumnCellRenderer("name", nameTableCellRenderer);

		table.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseClicked(MouseEvent e) {
				if (e.getClickCount() == 2) {
					final ThreadInformations threadInformations = myTable.getSelectedObject();
					showStackTraceInPopup(threadInformations);
				}
			}
		});

		add(tableScrollPane, BorderLayout.NORTH);

		table.setList(threadInformationsList);
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
					// TODO	
				}
			}
		});
		final JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
		buttonPanel.setOpaque(false);
		buttonPanel.add(killThreadButton);
		add(buttonPanel, BorderLayout.EAST);
	}

	final boolean confirm(String message) {
		return MSwingUtilities.showConfirmation(this, message);
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
			final JTextArea textArea = new JTextArea();
			textArea.setText(sb.toString());
			textArea.setEditable(false);
			// background nécessaire avec la plupart des look and feels dont Nimbus,
			// sinon il reste blanc malgré editable false
			textArea.setBackground(Color.decode("#E7E7E7"));
			final JScrollPane scrollPane = new JScrollPane(textArea);
			final JDialog dialog = new JDialog((JFrame) SwingUtilities.getWindowAncestor(table),
					threadInformations.getName(), true);
			final JPanel contentPane = new JPanel(new BorderLayout());
			contentPane.add(scrollPane, BorderLayout.CENTER);
			dialog.setContentPane(contentPane);
			dialog.pack();
			dialog.setLocationRelativeTo(table);
			dialog.setVisible(true);
		}
	}
}
