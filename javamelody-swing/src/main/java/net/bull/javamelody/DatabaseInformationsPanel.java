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
import java.awt.Desktop;
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

		setName(I18N.getString("database"));
		final String selectedRequestName = databaseInformations.getSelectedRequestName();
		final JLabel titleLabel = Utilities.createParagraphTitle(I18N.getString("database") + " : "
				+ I18N.getString(selectedRequestName), "db.png");
		add(titleLabel, BorderLayout.NORTH);

		final MTableScrollPane<DatabaseInformations> scrollPane = createScrollPane();

		add(scrollPane, BorderLayout.CENTER);

		add(createButtonsPanel(), BorderLayout.SOUTH);
	}

	private MTableScrollPane<DatabaseInformations> createScrollPane() {
		final MTableScrollPane<DatabaseInformations> tableScrollPane = new MTableScrollPane<DatabaseInformations>();
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
		final JComboBox requestComboBox = new JComboBox(databaseInformations.getRequestNames()
				.toArray());
		requestComboBox.setSelectedIndex(databaseInformations.getSelectedRequestIndex());
		requestComboBox.setRenderer(new DefaultListCellRenderer() {
			private static final long serialVersionUID = 1L;

			@Override
			public Component getListCellRendererComponent(JList list, Object value, int index,
					boolean isSelected, boolean cellHasFocus) {
				final String requestKey = (String) value;
				final String label = I18N.getString(requestKey);
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

		return Utilities.createButtonsPanel(requestComboBox, refreshButton, pdfButton);
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
