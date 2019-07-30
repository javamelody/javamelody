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
import java.io.File;
import java.io.IOException;
import java.text.DecimalFormat;
import java.util.List;

import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTable;
import javax.swing.table.TableColumn;

import net.bull.javamelody.internal.common.I18N;
import net.bull.javamelody.internal.model.Action;
import net.bull.javamelody.internal.model.HeapHistogram;
import net.bull.javamelody.internal.model.HeapHistogram.ClassInfo;
import net.bull.javamelody.internal.model.RemoteCollector;
import net.bull.javamelody.internal.web.pdf.PdfOtherReport;
import net.bull.javamelody.swing.MButton;
import net.bull.javamelody.swing.MTabbedPane;
import net.bull.javamelody.swing.Utilities;
import net.bull.javamelody.swing.table.MIntegerTableCellRenderer;
import net.bull.javamelody.swing.table.MTable;
import net.bull.javamelody.swing.table.MTableScrollPane;

/**
 * Panel de l'histogramme mémoire.
 * @author Emeric Vernat
 */
class HeapInformationsPanel extends MelodyPanel {
	private static final long serialVersionUID = 1L;

	private static final ImageIcon GC_ICON = ImageIconCache.getScaledImageIcon("broom.png", 16, 16);

	private static final ImageIcon HEAP_DUMP_ICON = ImageIconCache
			.getScaledImageIcon("heapdump.png", 16, 16);

	@SuppressWarnings("all")
	private HeapHistogram heapHistogram;

	private static final class BytesTableCellRenderer extends MIntegerTableCellRenderer {
		private static final long serialVersionUID = 1L;

		BytesTableCellRenderer() {
			super();
		}

		@Override
		public void setValue(final Object value) {
			super.setValue((Long) value / 1024);
		}
	}

	private static final class PctTailleTableCellRenderer extends MIntegerTableCellRenderer {
		private static final long serialVersionUID = 1L;

		private final long totalBytes;

		PctTailleTableCellRenderer(long totalBytes) {
			super();
			this.totalBytes = totalBytes;
		}

		@SuppressWarnings("unchecked")
		@Override
		public Component getTableCellRendererComponent(JTable jtable, Object value,
				boolean isSelected, boolean hasFocus, int row, int column) {
			final Long pctTaille;
			if (row == -1) {
				pctTaille = null;
			} else if (jtable instanceof MTable) {
				final MTable<ClassInfo> myTable = (MTable<ClassInfo>) jtable;
				final ClassInfo classInfo = myTable.getList()
						.get(myTable.convertRowIndexToModel(row));
				pctTaille = classInfo.getBytes() * 100 / totalBytes;
			} else {
				pctTaille = -1L;
			}
			return super.getTableCellRendererComponent(jtable, pctTaille, isSelected, hasFocus, row,
					column);
		}
	}

	private static final class PctInstancesTableCellRenderer extends MIntegerTableCellRenderer {
		private static final long serialVersionUID = 1L;

		private final long totalInstances;

		PctInstancesTableCellRenderer(long totalInstances) {
			super();
			this.totalInstances = totalInstances;
		}

		@SuppressWarnings("unchecked")
		@Override
		public Component getTableCellRendererComponent(JTable jtable, Object value,
				boolean isSelected, boolean hasFocus, int row, int column) {
			final Long pctInstances;
			if (row == -1) {
				pctInstances = null;
			} else if (jtable instanceof MTable) {
				final MTable<ClassInfo> myTable = (MTable<ClassInfo>) jtable;
				final ClassInfo classInfo = myTable.getList()
						.get(myTable.convertRowIndexToModel(row));
				pctInstances = classInfo.getInstancesCount() * 100 / totalInstances;
			} else {
				pctInstances = -1L;
			}
			return super.getTableCellRendererComponent(jtable, pctInstances, isSelected, hasFocus,
					row, column);
		}
	}

	HeapInformationsPanel(RemoteCollector remoteCollector) throws IOException {
		super(remoteCollector);

		refresh();
	}

	final void refresh() throws IOException {
		removeAll();

		this.heapHistogram = getRemoteCollector().collectHeapHistogram();

		setName(getFormattedString("heap_histo_du",
				I18N.createDateAndTimeFormat().format(heapHistogram.getTime())));
		final JLabel titleLabel = Utilities.createParagraphTitle(getName(), "memory.png");
		add(titleLabel, BorderLayout.NORTH);

		final JPanel heapTabPanel = createTabPanel(heapHistogram.getHeapHistogram(),
				heapHistogram.getTotalHeapInstances(), heapHistogram.getTotalHeapBytes(), true);

		final List<ClassInfo> permGenHistogram = heapHistogram.getPermGenHistogram();
		if (!permGenHistogram.isEmpty()) {
			final JPanel permGenTabPanel = createTabPanel(heapHistogram.getPermGenHistogram(),
					heapHistogram.getTotalPermGenInstances(), heapHistogram.getTotalPermGenBytes(),
					false);

			final MTabbedPane tabbedPane = new MTabbedPane();
			tabbedPane.add(getString("Heap"), heapTabPanel);
			tabbedPane.add(getString("PermGen"), permGenTabPanel);
			add(tabbedPane, BorderLayout.CENTER);
		} else {
			add(heapTabPanel, BorderLayout.CENTER);
		}

		add(createButtonsPanel(), BorderLayout.SOUTH);
	}

	private JPanel createTabPanel(List<ClassInfo> classInfos, long totalInstances, long totalBytes,
			boolean heap) {
		final boolean sourceDisplayed = heap && heapHistogram.isSourceDisplayed();
		final MTableScrollPane<ClassInfo> scrollPane = createScrollPane(totalInstances, totalBytes,
				sourceDisplayed);
		final MTable<ClassInfo> table = scrollPane.getTable();
		table.setList(classInfos);

		final JLabel label = createSummaryLabel(classInfos.size(), totalInstances, totalBytes);

		final JPanel panel = new JPanel(new BorderLayout());
		panel.setOpaque(false);
		panel.add(scrollPane, BorderLayout.CENTER);
		panel.add(label, BorderLayout.NORTH);
		return panel;
	}

	private JLabel createSummaryLabel(int size, long totalInstances, long totalBytes) {
		final DecimalFormat integerFormat = I18N.createIntegerFormat();
		final String text = ' ' + getString("Classes") + ": " + integerFormat.format(size) + ",   "
				+ getString("Instances") + ": " + integerFormat.format(totalInstances) + ",   "
				+ getString("Kilo-Octets") + ": " + integerFormat.format(totalBytes / 1024);
		return new JLabel(text);
	}

	private MTableScrollPane<HeapHistogram.ClassInfo> createScrollPane(long totalInstances,
			long totalBytes, boolean sourceDisplayed) {
		final MTableScrollPane<HeapHistogram.ClassInfo> tableScrollPane = new MTableScrollPane<>();
		final MTable<ClassInfo> myTable = tableScrollPane.getTable();

		myTable.addColumn("name", getString("Classe"));
		myTable.addColumn("bytes", getString("Taille"));
		final TableColumn pctTailleColumn = new TableColumn(myTable.getColumnCount());
		pctTailleColumn.setIdentifier(myTable.getColumnCount());
		pctTailleColumn.setHeaderValue(getString("pct_taille"));
		myTable.addColumn(pctTailleColumn);
		myTable.addColumn("instancesCount", getString("Instances"));
		final TableColumn pctInstancesColumn = new TableColumn(myTable.getColumnCount());
		pctInstancesColumn.setIdentifier(myTable.getColumnCount());
		pctInstancesColumn.setHeaderValue(getString("pct_instances"));
		myTable.addColumn(pctInstancesColumn);
		if (sourceDisplayed) {
			myTable.addColumn("source", getString("Source"));
		}

		myTable.setColumnCellRenderer("bytes", new BytesTableCellRenderer());
		pctTailleColumn.setCellRenderer(new PctTailleTableCellRenderer(totalBytes));
		pctInstancesColumn.setCellRenderer(new PctInstancesTableCellRenderer(totalInstances));

		return tableScrollPane;
	}

	private JPanel createButtonsPanel() {
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

		final MButton gcButton = new MButton(getString("ramasse_miette"), GC_ICON);
		gcButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				if (confirm(getString("confirm_ramasse_miette"))) {
					executeAction(Action.GC);
				}
			}
		});

		final MButton heapDumpButton = new MButton(getString("heap_dump"), HEAP_DUMP_ICON);
		heapDumpButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				if (confirm(getString("confirm_heap_dump"))) {
					executeAction(Action.HEAP_DUMP);
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
		final MButton xmlJsonButton = createXmlJsonButton(heapHistogram);

		return Utilities.createButtonsPanel(refreshButton, pdfButton, xmlJsonButton, gcButton,
				heapDumpButton);
	}

	final void actionPdf() throws IOException {
		final File tempFile = createTempFileForPdf();
		final PdfOtherReport pdfOtherReport = createPdfOtherReport(tempFile);
		try {
			pdfOtherReport.writeHeapHistogram(heapHistogram);
		} finally {
			pdfOtherReport.close();
		}
		Desktop.getDesktop().open(tempFile);
	}

	final void executeAction(Action action) {
		try {
			final String message = getRemoteCollector().executeActionAndCollectData(action, null,
					null, null, null, null);
			showMessage(message);
			refresh();
			MainPanel.refreshMainTabFromChild(this);
		} catch (final IOException ex) {
			showException(ex);
		}
	}
}
