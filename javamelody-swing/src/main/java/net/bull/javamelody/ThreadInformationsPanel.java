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
import java.awt.Color;
import java.awt.Component;
import java.awt.Desktop;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.File;
import java.io.IOException;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTable;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import net.bull.javamelody.internal.common.Parameters;
import net.bull.javamelody.internal.model.Action;
import net.bull.javamelody.internal.model.JavaInformations;
import net.bull.javamelody.internal.model.RemoteCollector;
import net.bull.javamelody.internal.model.ThreadInformations;
import net.bull.javamelody.internal.web.html.HtmlThreadInformationsReport;
import net.bull.javamelody.internal.web.pdf.PdfOtherReport;
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

	private final JavaInformations javaInformations;
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
				final ThreadInformations threadInformations = myTable.getList()
						.get(myTable.convertRowIndexToModel(row));
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
				final ThreadInformations threadInformations = myTable.getList()
						.get(myTable.convertRowIndexToModel(row));
				setIcon(ImageIconCache.getImageIcon("bullets/"
						+ HtmlThreadInformationsReport.getStateIcon(threadInformations)));
			}
			// et texte selon la valeur (libellé de l'état)
			return super.getTableCellRendererComponent(jtable, value, isSelected, hasFocus, row,
					column);
		}
	}

	ThreadInformationsPanel(RemoteCollector remoteCollector, JavaInformations javaInformations) {
		super(remoteCollector);
		assert javaInformations != null;
		this.javaInformations = javaInformations;
		this.threadInformationsList = javaInformations.getThreadInformationsList();
		this.stackTraceEnabled = javaInformations.isStackTraceEnabled();
		this.cpuTimeEnabled = !threadInformationsList.isEmpty()
				&& threadInformationsList.get(0).getCpuTimeMillis() != -1;

		final MTableScrollPane<ThreadInformations> scrollPane = createScrollPane();
		this.table = scrollPane.getTable();
		table.setList(threadInformationsList);
		Utilities.adjustTableHeight(table);

		add(scrollPane, BorderLayout.NORTH);

		final JLabel label = new JLabel(' ' + getString("Temps_threads"));
		// panel to align vertically the label in the threads as new tab
		final JPanel labelPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
		labelPanel.setOpaque(false);
		labelPanel.add(label);
		add(labelPanel, BorderLayout.WEST);

		final JPanel buttonsPanel = createButtonsPanel();
		add(buttonsPanel, BorderLayout.EAST);
	}

	private MTableScrollPane<ThreadInformations> createScrollPane() {
		final MTableScrollPane<ThreadInformations> tableScrollPane = new MTableScrollPane<>();
		final MTable<ThreadInformations> myTable = tableScrollPane.getTable();
		myTable.addColumn("name", getString("Thread"));
		myTable.addColumn("daemon", getString("Demon"));
		myTable.addColumn("priority", getString("Priorite"));
		myTable.addColumn("state", getString("Etat"));
		if (stackTraceEnabled) {
			myTable.addColumn("executedMethod", getString("Methode_executee"));
		}
		if (cpuTimeEnabled) {
			myTable.addColumn("cpuTimeMillis", getString("Temps_cpu"));
			myTable.addColumn("userTimeMillis", getString("Temps_user"));
		}
		myTable.setColumnCellRenderer("state", new StateTableCellRenderer());

		myTable.setColumnCellRenderer("name", new NameTableCellRenderer());

		return tableScrollPane;
	}

	private JPanel createButtonsPanel() {
		final JPanel buttonsPanel = Utilities.createButtonsPanel();

		final MButton openButton = new MButton(getString("Ouvrir"),
				ImageIconCache.getImageIcon("action_open.png"));
		buttonsPanel.add(openButton);
		openButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				final ThreadInformations threadInformations = getTable().getSelectedObject();
				showStackTraceInPopup(threadInformations);
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
				final ThreadInformations threadInformations = getTable().getSelectedObject();
				openButton.setEnabled(threadInformations != null);
			}
		});
		openButton.setEnabled(false);

		if (Parameters.isSystemActionsEnabled()) {
			addInterruptAndKillThreadButtons(buttonsPanel);
		}

		if (stackTraceEnabled) {
			final MButton dumpThreadsButton = new MButton(getString("Dump_threads_en_texte"),
					ImageIconCache.getImageIcon("text.png"));
			dumpThreadsButton.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					actionDumpThreads();
				}
			});
			buttonsPanel.add(dumpThreadsButton);
		}
		final MButton openInNewTabButton = new MButton(getString("Voir_dans_une_nouvelle_page"),
				ImageIconCache.getScaledImageIcon("threads.png", 16, 16));
		openInNewTabButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				final JPanel panel = createPanelForNewTab();
				MainPanel.addOngletFromChild(ThreadInformationsPanel.this, panel);
			}
		});
		buttonsPanel.add(openInNewTabButton);

		return buttonsPanel;
	}

	private void addInterruptAndKillThreadButtons(JPanel buttonsPanel) {
		final MButton sendThreadInterruptButton = new MButton(getString("Interrupt"),
				ImageIconCache.getImageIcon("action_interrupt.png"));
		final MButton killThreadButton = new MButton(getString("Tuer"),
				ImageIconCache.getImageIcon("stop.png"));
		getTable().getSelectionModel().addListSelectionListener(new ListSelectionListener() {
			@Override
			public void valueChanged(ListSelectionEvent e) {
				final ThreadInformations threadInformations = getTable().getSelectedObject();
				sendThreadInterruptButton.setEnabled(threadInformations != null);
				killThreadButton.setEnabled(threadInformations != null);
				if (threadInformations != null) {
					sendThreadInterruptButton.setToolTipText(getFormattedString(
							"send_thread_interrupt", threadInformations.getName()));
					killThreadButton.setToolTipText(
							getFormattedString("kill_thread", threadInformations.getName()));
				} else {
					sendThreadInterruptButton.setToolTipText(null);
					killThreadButton.setToolTipText(null);
				}
			}
		});
		sendThreadInterruptButton.setEnabled(getTable().getSelectedObject() != null);
		killThreadButton.setEnabled(getTable().getSelectedObject() != null);
		sendThreadInterruptButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				final ThreadInformations threadInformations = getTable().getSelectedObject();
				if (threadInformations != null
						&& confirm(getFormattedString("confirm_send_thread_interrupt",
								threadInformations.getName()))) {
					actionSendThreadInterrupt(threadInformations);
				}
			}
		});
		killThreadButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				final ThreadInformations threadInformations = getTable().getSelectedObject();
				if (threadInformations != null && confirm(
						getFormattedString("confirm_kill_thread", threadInformations.getName()))) {
					actionKillThread(threadInformations);
				}
			}
		});

		buttonsPanel.add(sendThreadInterruptButton);
		buttonsPanel.add(killThreadButton);
	}

	final void actionSendThreadInterrupt(ThreadInformations threadInformations) {
		try {
			final String message = getRemoteCollector().executeActionAndCollectData(
					Action.SEND_THREAD_INTERRUPT, null, null,
					threadInformations.getGlobalThreadId(), null, null);
			showMessage(message);
			MainPanel.refreshMainTabFromChild(this);
		} catch (final IOException ex) {
			showException(ex);
		}
	}

	final void actionKillThread(ThreadInformations threadInformations) {
		try {
			final String message = getRemoteCollector().executeActionAndCollectData(
					Action.KILL_THREAD, null, null, threadInformations.getGlobalThreadId(), null,
					null);
			showMessage(message);
			MainPanel.refreshMainTabFromChild(this);
		} catch (final IOException ex) {
			showException(ex);
		}
	}

	final void actionDumpThreads() {
		try {
			final ThreadsDumpPanel panel = new ThreadsDumpPanel(getRemoteCollector(),
					javaInformations);
			MainPanel.addOngletFromChild(this, panel);
		} catch (final IOException ex) {
			showException(ex);
		}
	}

	JPanel createPanelForNewTab() {
		final JPanel panel = new JPanel();
		panel.setName(getString("Threads"));
		panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));

		final MButton refreshButton = createRefreshButton();
		refreshButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				try {
					getRemoteCollector().collectDataIncludingCurrentRequests();
					panel.removeAll();
					final JPanel newPanel = createPanelForNewTab();
					newPanel.setOpaque(false);
					panel.add(newPanel);
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
		final Serializable serializable;
		if (getJavaInformationsList().size() == 1) {
			serializable = new ArrayList<>(
					getJavaInformationsList().get(0).getThreadInformationsList());
		} else {
			final List<List<ThreadInformations>> list = new ArrayList<>();
			for (final JavaInformations javaInfos : getJavaInformationsList()) {
				list.add(new ArrayList<>(javaInfos.getThreadInformationsList()));
			}
			serializable = (Serializable) list;
		}
		final MButton xmlJsonButton = createXmlJsonButton(serializable);
		final JPanel mainButtonsPanel = Utilities.createButtonsPanel(refreshButton, pdfButton,
				xmlJsonButton);
		mainButtonsPanel.setLayout(new FlowLayout(FlowLayout.LEFT));

		panel.add(mainButtonsPanel);
		panel.add(Utilities.createParagraphTitle(getString("Threads"), "threads.png"));
		for (final JavaInformations javaInfos : getJavaInformationsList()) {
			final ThreadInformationsPanel threadInformationsPanel = new ThreadInformationsPanel(
					getRemoteCollector(), javaInfos);
			panel.add(threadInformationsPanel.createThreadsAsPartPanel());
		}
		for (final Component component : panel.getComponents()) {
			((JComponent) component).setAlignmentX(Component.LEFT_ALIGNMENT);
		}
		return panel;
	}

	final void actionPdf() throws IOException {
		final File tempFile = createTempFileForPdf();
		final PdfOtherReport pdfOtherReport = createPdfOtherReport(tempFile);
		try {
			pdfOtherReport.writeThreads(getJavaInformationsList());
		} finally {
			pdfOtherReport.close();
		}
		Desktop.getDesktop().open(tempFile);
	}

	final void showStackTraceInPopup(ThreadInformations threadInformations) {
		final StringBuilder sb = new StringBuilder();
		sb.append(threadInformations.getName());
		final List<StackTraceElement> stackTrace = threadInformations.getStackTrace();
		if (stackTrace != null && !stackTrace.isEmpty()) {
			// même si stackTraceEnabled, ce thread n'a pas forcément de stack-trace
			sb.append('\n');
			for (final StackTraceElement stackTraceElement : stackTrace) {
				sb.append(stackTraceElement);
				sb.append('\n');
			}
		}
		final String title = threadInformations.getName();
		final String text = sb.toString();
		Utilities.showTextInPopup(this, title, text);
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

	JLabel createSummaryLabel() {
		return new JLabel("<html><b>"
				+ getFormattedString("Threads_sur", javaInformations.getHost()) + ": </b>"
				+ getFormattedString("thread_count", javaInformations.getThreadCount(),
						javaInformations.getPeakThreadCount(),
						javaInformations.getTotalStartedThreadCount()));
	}

	JLabel createThreadDeadlocksLabel() {
		boolean deadlockFound = false;
		final StringBuilder sb = new StringBuilder();
		sb.append("  ");
		sb.append(getString("Threads_deadlocks"));
		String separator = " ";
		for (final ThreadInformations thread : threadInformationsList) {
			if (thread.isDeadlocked()) {
				deadlockFound = true;
				sb.append(separator);
				sb.append(thread.getName());
				separator = ", ";
			}
		}
		if (deadlockFound) {
			final JLabel label = new JLabel(sb.toString());
			label.setForeground(Color.RED);
			label.setFont(label.getFont().deriveFont(Font.BOLD));
			// séparateur avec composants au-dessus
			label.setBorder(BorderFactory.createEmptyBorder(10, 0, 0, 0));
			return label;
		}
		return null;
	}

	JPanel createThreadsAsPartPanel() {
		final JPanel panel = new JPanel();
		panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
		panel.setOpaque(false);
		panel.add(createSummaryLabel());
		panel.add(this);
		final JLabel deadlockLabel = createThreadDeadlocksLabel();
		if (deadlockLabel != null) {
			// au moins un thread est en deadlock
			panel.add(deadlockLabel);
		}
		for (final Component component : panel.getComponents()) {
			((JComponent) component).setAlignmentX(Component.LEFT_ALIGNMENT);
		}
		return panel;
	}

	MTable<ThreadInformations> getTable() {
		return table;
	}
}
