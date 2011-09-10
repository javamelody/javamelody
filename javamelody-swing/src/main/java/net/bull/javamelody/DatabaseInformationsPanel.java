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
import java.awt.Desktop;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;

import javax.swing.Box;
import javax.swing.DefaultListCellRenderer;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;

import net.bull.javamelody.swing.MButton;
import net.bull.javamelody.swing.Utilities;
import net.bull.javamelody.swing.table.MTable;
import net.bull.javamelody.swing.table.MTableScrollPane;

/**
 * Panel du rapport base de données.
 * @author Emeric Vernat
 */
class DatabaseInformationsPanel extends MelodyPanel {
	private static final long serialVersionUID = 1L;

	@SuppressWarnings("all")
	private DatabaseInformations databaseInformations;
	private MTable<DatabaseInformations> table;

	DatabaseInformationsPanel(RemoteCollector remoteCollector) throws IOException {
		super(remoteCollector, new BorderLayout());

		refresh(0);
	}

	final void refresh(int requestIndex) throws IOException {
		removeAll();

		this.databaseInformations = getRemoteCollector().collectDatabaseInformations(requestIndex);
		this.table = new MTable<DatabaseInformations>();

		setName(I18N.getString("database"));
		final String selectedRequestName = databaseInformations.getSelectedRequestName();
		final JLabel titleLabel = Utilities.createParagraphTitle(I18N.getString("database") + " : "
				+ I18N.getString(selectedRequestName), "db.png");
		add(titleLabel, BorderLayout.NORTH);

		addScrollPane();

		add(createButtonsPanel(), BorderLayout.SOUTH);
	}

	private void addScrollPane() {
		final MTableScrollPane<DatabaseInformations> tableScrollPane = new MTableScrollPane<DatabaseInformations>(
				table);
		//		table.addColumn("name", I18N.getString("Thread"));
		// TODO

		add(tableScrollPane, BorderLayout.CENTER);
	}

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

		final MButton refreshButton = new MButton(I18N.getString("Actualiser"),
				ImageIconCache.getImageIcon("action_refresh.png"));
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
		final MButton pdfButton = new MButton(I18N.getString("PDF"),
				ImageIconCache.getImageIcon("pdf.png"));
		pdfButton.setToolTipText(I18N.getString("afficher_PDF"));
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

		final JPanel buttonsPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
		buttonsPanel.setOpaque(false);
		buttonsPanel.add(requestComboBox);
		buttonsPanel.add(Box.createHorizontalGlue());
		buttonsPanel.add(refreshButton);
		buttonsPanel.add(pdfButton);
		return buttonsPanel;
	}

	final void actionPdf() throws IOException {
		final File tempFile = createTempFileForPdf();
		final OutputStream output = new BufferedOutputStream(new FileOutputStream(tempFile));
		try {
			final PdfOtherReport pdfOtherReport = new PdfOtherReport(getRemoteCollector()
					.getApplication(), output);
			pdfOtherReport.writeDatabaseInformations(databaseInformations);
		} finally {
			output.close();
		}
		Desktop.getDesktop().open(tempFile);
	}

	DatabaseInformations getDatabaseInformations() {
		return databaseInformations;
	}
}
