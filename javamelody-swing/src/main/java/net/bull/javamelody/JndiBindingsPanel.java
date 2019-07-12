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
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.File;
import java.io.IOException;
import java.io.Serializable;
import java.util.ArrayDeque;
import java.util.Deque;
import java.util.List;

import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTable;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import net.bull.javamelody.internal.model.JndiBinding;
import net.bull.javamelody.internal.model.RemoteCollector;
import net.bull.javamelody.internal.web.pdf.PdfOtherReport;
import net.bull.javamelody.swing.MButton;
import net.bull.javamelody.swing.Utilities;
import net.bull.javamelody.swing.table.MDefaultTableCellRenderer;
import net.bull.javamelody.swing.table.MTable;
import net.bull.javamelody.swing.table.MTableScrollPane;

/**
 * Panel de l'arbre JNDI.
 * @author Emeric Vernat
 */
class JndiBindingsPanel extends MelodyPanel {
	static final ImageIcon FOLDER_ICON = ImageIconCache.getImageIcon("folder.png");

	private static final long serialVersionUID = 1L;

	@SuppressWarnings("all")
	private List<JndiBinding> jndiBindings;

	private String path;

	private final Deque<String> previousPaths = new ArrayDeque<>();

	private MTable<JndiBinding> table;

	private class NameTableCellRenderer extends MDefaultTableCellRenderer {
		private static final long serialVersionUID = 1L;

		NameTableCellRenderer() {
			super();
		}

		@Override
		public Component getTableCellRendererComponent(JTable jtable, Object value,
				boolean isSelected, boolean hasFocus, int row, int column) {
			if (row == -1) {
				setIcon(null);
			} else {
				final MTable<JndiBinding> myTable = getTable();
				final JndiBinding jndiBinding = myTable.getList()
						.get(myTable.convertRowIndexToModel(row));
				if (jndiBinding.getContextPath() != null) {
					setIcon(FOLDER_ICON);
				} else {
					setIcon(null);
				}
			}
			return super.getTableCellRendererComponent(jtable, value, isSelected, hasFocus, row,
					column);
		}
	}

	JndiBindingsPanel(RemoteCollector remoteCollector) throws IOException {
		super(remoteCollector);

		refresh(); // NOPMD
	}

	final void refresh() throws IOException {
		removeAll();

		this.jndiBindings = getRemoteCollector().collectJndiBindings(path);

		if (path == null || path.length() == 0) {
			setName(getString("Arbre_JNDI"));
		} else {
			setName(getFormattedString("Arbre_JNDI_pour_contexte", path));
		}

		final JLabel titleLabel = Utilities.createParagraphTitle(getName(), "jndi.png");
		add(titleLabel, BorderLayout.NORTH);

		final MTableScrollPane<JndiBinding> scrollPane = createScrollPane();
		this.table = scrollPane.getTable();
		table.setList(jndiBindings);

		add(scrollPane, BorderLayout.CENTER);

		add(createButtonsPanel(), BorderLayout.SOUTH);
	}

	private MTableScrollPane<JndiBinding> createScrollPane() {
		final MTableScrollPane<JndiBinding> tableScrollPane = new MTableScrollPane<>();
		final MTable<JndiBinding> myTable = tableScrollPane.getTable();
		myTable.addColumn("name", getString("Nom"));
		myTable.addColumn("className", getString("Type"));
		myTable.addColumn("value", getString("Value"));

		myTable.setColumnCellRenderer("name", new NameTableCellRenderer());

		return tableScrollPane;
	}

	private JPanel createButtonsPanel() {
		final MButton openButton = new MButton(getString("Ouvrir"),
				ImageIconCache.getImageIcon("action_open.png"));
		final MButton backButton = new MButton(getString("Retour"),
				ImageIconCache.getImageIcon("action_back.png"));

		openButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				final JndiBinding binding = getTable().getSelectedObject();
				try {
					openContext(binding);
				} catch (final IOException ex) {
					showException(ex);
				}
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
				final JndiBinding binding = getTable().getSelectedObject();
				openButton.setEnabled(binding != null && binding.getContextPath() != null);
			}
		});
		openButton.setEnabled(false);

		backButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				try {
					goBack();
					backButton.setEnabled(getPath() != null);
				} catch (final IOException e1) {
					showException(e1);
				}
			}
		});
		backButton.setEnabled(getPath() != null);

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
		final MButton xmlJsonButton = createXmlJsonButton((Serializable) jndiBindings);

		final MButton refreshButton = createRefreshButton();
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

		return Utilities.createButtonsPanel(backButton, openButton, refreshButton, pdfButton,
				xmlJsonButton);
	}

	void goBack() throws IOException {
		path = previousPaths.pollLast();
		refresh();
	}

	void openContext(JndiBinding binding) throws IOException {
		if (path != null) {
			previousPaths.add(path);
		}
		path = binding.getContextPath();
		refresh();
	}

	final void actionPdf() throws IOException {
		final File tempFile = createTempFileForPdf();
		final PdfOtherReport pdfOtherReport = createPdfOtherReport(tempFile);
		try {
			pdfOtherReport.writeJndi(jndiBindings, path != null ? path : "");
		} finally {
			pdfOtherReport.close();
		}
		Desktop.getDesktop().open(tempFile);
	}

	MTable<JndiBinding> getTable() {
		return table;
	}

	String getPath() {
		return path;
	}
}
