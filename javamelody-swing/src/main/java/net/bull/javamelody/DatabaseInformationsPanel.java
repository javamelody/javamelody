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
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.io.File;
import java.io.IOException;

import javax.swing.DefaultListCellRenderer;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.SwingConstants;
import javax.swing.table.TableColumn;

import net.bull.javamelody.internal.model.DatabaseInformations;
import net.bull.javamelody.internal.model.RemoteCollector;
import net.bull.javamelody.internal.web.pdf.PdfOtherReport;
import net.bull.javamelody.swing.MButton;
import net.bull.javamelody.swing.Utilities;
import net.bull.javamelody.swing.table.MMultiLineTableCellRenderer;
import net.bull.javamelody.swing.table.MTable;
import net.bull.javamelody.swing.table.MTableModel;
import net.bull.javamelody.swing.table.MTableScrollPane;

/**
 * Panel du rapport base de données.
 * @author Emeric Vernat
 */
class DatabaseInformationsPanel extends MelodyPanel {
	private static final long serialVersionUID = 1L;

	@SuppressWarnings("all")
	private DatabaseInformations databaseInformations;

	DatabaseInformationsPanel(RemoteCollector remoteCollector) throws IOException {
		super(remoteCollector);

		refresh(0);
	}

	final void refresh(int requestIndex) throws IOException {
		removeAll();

		this.databaseInformations = getRemoteCollector().collectDatabaseInformations(requestIndex);

		setName(getString("database"));
		final String selectedRequestName = databaseInformations.getSelectedRequestName();
		final JLabel titleLabel = Utilities.createParagraphTitle(
				getString("database") + " : " + getString(selectedRequestName), "db.png");
		add(titleLabel, BorderLayout.NORTH);

		final MTableScrollPane<DatabaseInformations> scrollPane = createScrollPane();

		add(scrollPane, BorderLayout.CENTER);

		add(createButtonsPanel(), BorderLayout.SOUTH);
	}

	private MTableScrollPane<DatabaseInformations> createScrollPane() {
		final MTableScrollPane<DatabaseInformations> tableScrollPane = new MTableScrollPane<>();
		final MTable<DatabaseInformations> myTable = tableScrollPane.getTable();
		final String[][] values = databaseInformations.getResult();
		myTable.setModel(new MTableModel<String[]>(myTable) {
			private static final long serialVersionUID = 1L;

			@Override
			public Object getValueAt(int rowIndex, int columnIndex) {
				return values[rowIndex + 1][columnIndex];
			}

			@Override
			public int getRowCount() {
				return values.length - 1;
			}
		});

		for (final String header : values[0]) {
			final TableColumn tableColumn = new TableColumn(myTable.getColumnCount());
			tableColumn.setIdentifier(header);
			if (header.indexOf('\n') != -1) {
				tableColumn.setHeaderValue(header.substring(0, header.indexOf('\n')));
				// on ne peut pas mettre <html> et <br/> car les exports ne seraient pas bons
				// tableColumn.setHeaderValue("<html>" + header.replace("\n", "<br/>"));
			} else {
				tableColumn.setHeaderValue(header);
			}
			tableColumn.setCellRenderer(new MMultiLineTableCellRenderer() {
				private static final long serialVersionUID = 1L;

				@Override
				public void setValue(Object value) {
					super.setValue(value);
					if (value != null && isNumber((String) value)) {
						setHorizontalAlignment(SwingConstants.RIGHT);
					} else {
						setHorizontalAlignment(SwingConstants.LEFT);
					}
				}
			});
			myTable.addColumn(tableColumn);
		}

		myTable.adjustColumnWidths();

		return tableScrollPane;
	}

	static boolean isNumber(String text) {
		final int length = text.length();
		for (int i = 0; i < length; i++) {
			final char c = text.charAt(i);
			if (!Character.isDigit(c) && c != '.') {
				return false;
			}
		}
		return true;
	}

	// SuppressWarnings pour jdk 1.7 en conservant compatibilité avec javac 1.6
	@SuppressWarnings({ "unchecked", "rawtypes" })
	private JPanel createButtonsPanel() {
		final JComboBox requestComboBox = new JComboBox(
				databaseInformations.getRequestNames().toArray());
		requestComboBox.setFont(requestComboBox.getFont().deriveFont(Font.BOLD));
		requestComboBox.setSelectedIndex(databaseInformations.getSelectedRequestIndex());
		requestComboBox.setRenderer(new DefaultListCellRenderer() {
			private static final long serialVersionUID = 1L;

			@Override
			public Component getListCellRendererComponent(JList list, Object value, int index,
					boolean isSelected, boolean cellHasFocus) {
				final String requestKey = (String) value;
				final String label = getString(requestKey);
				return super.getListCellRendererComponent(list, label, index, isSelected,
						cellHasFocus);
			}
		});
		requestComboBox.addItemListener(new ItemListener() {
			@Override
			public void itemStateChanged(ItemEvent e) {
				if (e.getStateChange() == ItemEvent.SELECTED) {
					final int requestIndex = requestComboBox.getSelectedIndex();
					try {
						refresh(requestIndex);
					} catch (final IOException ex) {
						showException(ex);
					}
				}
			}
		});

		final MButton refreshButton = createRefreshButton();
		refreshButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				try {
					refresh(getDatabaseInformations().getSelectedRequestIndex());
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
		final MButton xmlJsonButton = createXmlJsonButton(databaseInformations);

		return Utilities.createButtonsPanel(requestComboBox, new JLabel("             "),
				refreshButton, pdfButton, xmlJsonButton);
	}

	final void actionPdf() throws IOException {
		final File tempFile = createTempFileForPdf();
		final PdfOtherReport pdfOtherReport = createPdfOtherReport(tempFile);
		try {
			pdfOtherReport.writeDatabaseInformations(databaseInformations);
		} finally {
			pdfOtherReport.close();
		}
		Desktop.getDesktop().open(tempFile);
	}

	DatabaseInformations getDatabaseInformations() {
		return databaseInformations;
	}
}
