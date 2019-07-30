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
import java.io.Serializable;
import java.util.List;

import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTable;

import net.bull.javamelody.internal.model.Action;
import net.bull.javamelody.internal.model.RemoteCollector;
import net.bull.javamelody.internal.model.SamplingProfiler.SampledMethod;
import net.bull.javamelody.internal.web.pdf.PdfOtherReport;
import net.bull.javamelody.swing.MButton;
import net.bull.javamelody.swing.Utilities;
import net.bull.javamelody.swing.table.MDefaultTableCellRenderer;
import net.bull.javamelody.swing.table.MDoubleTableCellRenderer;
import net.bull.javamelody.swing.table.MTable;
import net.bull.javamelody.swing.table.MTableScrollPane;

/**
 * Panel des hotspots du sampling.
 * @author Emeric Vernat
 */
class HotspotsPanel extends MelodyPanel {
	private static final ImageIcon CLEAR_HOTSPOTS_ICON = ImageIconCache
			.getScaledImageIcon("user-trash.png", 16, 16);

	private static final long serialVersionUID = 1L;

	@SuppressWarnings("all")
	private List<SampledMethod> hotspots;

	private long totalCount;

	private MTable<SampledMethod> table;

	private class MethodTableCellRenderer extends MDefaultTableCellRenderer {
		private static final long serialVersionUID = 1L;

		MethodTableCellRenderer() {
			super();
		}

		@Override
		public Component getTableCellRendererComponent(JTable jtable, Object value,
				boolean isSelected, boolean hasFocus, int row, int column) {
			final String text;
			if (row == -1) {
				text = null;
			} else {
				final MTable<SampledMethod> myTable = getTable();
				final SampledMethod method = myTable.getList()
						.get(myTable.convertRowIndexToModel(row));
				text = "<html>" + method.getClassName() + ".<b>" + method.getMethodName() + "</b>";
			}
			// et texte selon la méthode
			return super.getTableCellRendererComponent(jtable, text, isSelected, hasFocus, row,
					column);
		}
	}

	private class CountTableCellRenderer extends MDoubleTableCellRenderer {
		private static final long serialVersionUID = 1L;

		CountTableCellRenderer() {
			super();
		}

		@Override
		public void setValue(final Object value) {
			final Long count = (Long) value;
			super.setValue(100d * count / getTotalCount());
		}
	}

	HotspotsPanel(RemoteCollector remoteCollector) throws IOException {
		super(remoteCollector);

		refresh();
	}

	final void refresh() throws IOException {
		removeAll();

		this.hotspots = getRemoteCollector().collectHotspots();

		long total = 0;
		for (final SampledMethod hotspot : hotspots) {
			total += hotspot.getCount();
		}
		this.totalCount = total;

		setName(getString("hotspots"));

		final JLabel titleLabel = Utilities.createParagraphTitle(getName(), "clock.png");
		add(titleLabel, BorderLayout.NORTH);

		final MTableScrollPane<SampledMethod> scrollPane = createScrollPane();
		this.table = scrollPane.getTable();
		table.setList(hotspots);

		add(scrollPane, BorderLayout.CENTER);

		add(createButtonsPanel(), BorderLayout.SOUTH);
	}

	private MTableScrollPane<SampledMethod> createScrollPane() {
		final MTableScrollPane<SampledMethod> tableScrollPane = new MTableScrollPane<>();
		final MTable<SampledMethod> myTable = tableScrollPane.getTable();
		myTable.addColumn("methodName", getString("Methode_executee"));
		myTable.addColumn("count", getString("percent_time"));

		myTable.setColumnCellRenderer("methodName", new MethodTableCellRenderer());
		myTable.setColumnCellRenderer("count", new CountTableCellRenderer());

		return tableScrollPane;
	}

	private JPanel createButtonsPanel() {
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
		final MButton xmlJsonButton = createXmlJsonButton((Serializable) hotspots);
		final MButton clearHotspotsButton = createClearHotspotsButton();

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

		return Utilities.createButtonsPanel(refreshButton, pdfButton, xmlJsonButton,
				clearHotspotsButton);
	}

	private MButton createClearHotspotsButton() {
		final MButton clearHotspotsButton = new MButton(getString("clear_hotspots"),
				CLEAR_HOTSPOTS_ICON);
		clearHotspotsButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				if (confirm(getFormattedString("confirm_clear_hotspots"))) {
					actionClearHotspots();
				}
			}
		});
		return clearHotspotsButton;
	}

	final void actionClearHotspots() {
		try {
			final String message = getRemoteCollector().executeActionAndCollectData(
					Action.CLEAR_HOTSPOTS, null, null, null, null, null);
			showMessage(message);
			refresh();
		} catch (final IOException ex) {
			showException(ex);
		}
	}

	final void actionPdf() throws IOException {
		final File tempFile = createTempFileForPdf();
		final PdfOtherReport pdfOtherReport = createPdfOtherReport(tempFile);
		try {
			pdfOtherReport.writeHotspots(hotspots);
		} finally {
			pdfOtherReport.close();
		}
		Desktop.getDesktop().open(tempFile);
	}

	MTable<SampledMethod> getTable() {
		return table;
	}

	long getTotalCount() {
		return totalCount;
	}
}
