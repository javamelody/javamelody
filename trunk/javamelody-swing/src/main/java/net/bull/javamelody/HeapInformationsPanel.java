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
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.text.DecimalFormat;
import java.text.FieldPosition;
import java.text.NumberFormat;
import java.util.List;

import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTable;
import javax.swing.table.TableColumn;

import net.bull.javamelody.HeapHistogram.ClassInfo;
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

	private static final ImageIcon HEAP_DUMP_ICON = ImageIconCache.getScaledImageIcon(
			"heapdump.png", 16, 16);

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

	private static final class BytesDeltaTableCellRenderer extends MIntegerTableCellRenderer {
		private static final long serialVersionUID = 1L;

		BytesDeltaTableCellRenderer() {
			super();

			final NumberFormat defaultNumberFormat = getNumberFormat();
			setNumberFormat(new DecimalFormat() {
				private static final long serialVersionUID = 1L;

				@Override
				public StringBuffer format(long number, StringBuffer toAppendTo, FieldPosition pos) {
					final StringBuffer sb = defaultNumberFormat.format(number, toAppendTo, pos);
					if (number > 0) {
						sb.insert(0, "+");
					}
					return sb;
				}
			});
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
				final ClassInfo classInfo = myTable.getList().get(
						myTable.convertRowIndexToModel(row));
				pctTaille = classInfo.getBytes() * 100 / totalBytes;
			} else {
				pctTaille = -1L;
			}
			return super.getTableCellRendererComponent(jtable, pctTaille, isSelected, hasFocus,
					row, column);
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
				final ClassInfo classInfo = myTable.getList().get(
						myTable.convertRowIndexToModel(row));
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

		setName(I18N.getFormattedString("heap_histo_du",
				I18N.createDateAndTimeFormat().format(heapHistogram.getTime())));
		final JLabel titleLabel = Utilities.createParagraphTitle(
				I18N.getFormattedString("heap_histo_du",
						I18N.createDateAndTimeFormat().format(heapHistogram.getTime())),
				"memory.png");
		add(titleLabel, BorderLayout.NORTH);

		final JPanel heapTabPanel = createTabPanel(heapHistogram.getHeapHistogram(),
				heapHistogram.getTotalHeapInstances(), heapHistogram.getTotalHeapBytes(), true);

		final List<ClassInfo> permGenHistogram = heapHistogram.getPermGenHistogram();
		if (!permGenHistogram.isEmpty()) {
			final JPanel permGenTabPanel = createTabPanel(heapHistogram.getPermGenHistogram(),
					heapHistogram.getTotalPermGenInstances(), heapHistogram.getTotalPermGenBytes(),
					false);

			final MTabbedPane tabbedPane = new MTabbedPane();
			tabbedPane.add(I18N.getString("Heap"), heapTabPanel);
			tabbedPane.add(I18N.getString("PermGen"), permGenTabPanel);
			add(tabbedPane, BorderLayout.CENTER);
		} else {
			add(heapTabPanel, BorderLayout.CENTER);
		}

		add(createButtonsPanel(), BorderLayout.SOUTH);
	}

	private JPanel createTabPanel(List<ClassInfo> classInfos, long totalInstances, long totalBytes,
			boolean heap) {
		final boolean deltaDisplayed = heap && heapHistogram.isDeltaDisplayed();
		final boolean sourceDisplayed = heap && heapHistogram.isSourceDisplayed();
		final MTableScrollPane<ClassInfo> scrollPane = createScrollPane(totalInstances, totalBytes,
				deltaDisplayed, sourceDisplayed);
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
		final String text = ' ' + I18N.getString("Classes") + ": " + integerFormat.format(size)
				+ ",   " + I18N.getString("Instances") + ": "
				+ integerFormat.format(totalInstances) + ",   " + I18N.getString("Kilo-Octets")
				+ ": " + integerFormat.format(totalBytes / 1024);
		return new JLabel(text);
	}

	private MTableScrollPane<HeapHistogram.ClassInfo> createScrollPane(long totalInstances,
			long totalBytes, boolean deltaDisplayed, boolean sourceDisplayed) {
		final MTableScrollPane<HeapHistogram.ClassInfo> tableScrollPane = new MTableScrollPane<HeapHistogram.ClassInfo>();
		final MTable<ClassInfo> myTable = tableScrollPane.getTable();

		myTable.addColumn("name", I18N.getString("Classe"));
		myTable.addColumn("bytes", I18N.getString("Taille"));
		final TableColumn pctTailleColumn = new TableColumn(myTable.getColumnCount());
		pctTailleColumn.setIdentifier(myTable.getColumnCount());
		pctTailleColumn.setHeaderValue(I18N.getString("pct_taille"));
		myTable.addColumn(pctTailleColumn);
		if (deltaDisplayed) {
			myTable.addColumn("bytesDelta", I18N.getString("Delta"));
			myTable.setColumnCellRenderer("bytesDelta", new BytesDeltaTableCellRenderer());
		}
		myTable.addColumn("instancesCount", I18N.getString("Instances"));
		final TableColumn pctInstancesColumn = new TableColumn(myTable.getColumnCount());
		pctInstancesColumn.setIdentifier(myTable.getColumnCount());
		pctInstancesColumn.setHeaderValue(I18N.getString("pct_instances"));
		myTable.addColumn(pctInstancesColumn);
		if (sourceDisplayed) {
			myTable.addColumn("source", I18N.getString("Source"));
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

		final MButton gcButton = new MButton(I18N.getString("ramasse_miette"), GC_ICON);
		gcButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				if (confirm(I18N.getString("confirm_ramasse_miette"))) {
					executeAction(Action.GC);
				}
			}
		});

		final MButton heapDumpButton = new MButton(I18N.getString("heap_dump"), HEAP_DUMP_ICON);
		heapDumpButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				if (confirm(I18N.getString("confirm_heap_dump"))) {
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

		return Utilities.createButtonsPanel(refreshButton, pdfButton, gcButton, heapDumpButton);
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
					null, null, null);
			showMessage(message);
			refresh();
			MainPanel.refreshMainTabFromChild(this);
		} catch (final IOException ex) {
			showException(ex);
		}
	}
}
