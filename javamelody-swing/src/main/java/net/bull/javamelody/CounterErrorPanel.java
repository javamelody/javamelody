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
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.text.DateFormat;
import java.util.List;

import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.SwingUtilities;

import com.lowagie.text.Font;

import net.bull.javamelody.internal.common.I18N;
import net.bull.javamelody.internal.model.Counter;
import net.bull.javamelody.internal.model.CounterError;
import net.bull.javamelody.internal.model.RemoteCollector;
import net.bull.javamelody.internal.web.html.HtmlCounterErrorReport;
import net.bull.javamelody.swing.Utilities;
import net.bull.javamelody.swing.table.MDateTableCellRenderer;
import net.bull.javamelody.swing.table.MMultiLineTableCellRenderer;
import net.bull.javamelody.swing.table.MTable;
import net.bull.javamelody.swing.table.MTableScrollPane;

/**
 * Panel des erreurs http et dans les logs ou dans les jobs.
 * @author Emeric Vernat
 */
class CounterErrorPanel extends MelodyPanel {
	private static final long serialVersionUID = 1L;

	private final Counter counter;
	private final MTable<CounterError> table;

	private final class MessageWithStackTraceTableCellRenderer extends MMultiLineTableCellRenderer {
		private static final long serialVersionUID = 1L;

		MessageWithStackTraceTableCellRenderer() {
			super();
		}

		@Override
		public Component getTableCellRendererComponent(JTable jtable, Object value,
				boolean isSelected, boolean hasFocus, int row, int column) {
			// texte selon la valeur (message de l'erreur)
			super.getTableCellRendererComponent(jtable, value, isSelected, hasFocus, row, column);
			// et tooltip selon la stackTrace
			// (l'appel à setToolTipText doit être après getTableCellRendererComponent
			// car MMultiLineTableCellRenderer change le toolTipText dans setValue)
			if (row == -1) {
				setToolTipText(null);
			} else {
				final MTable<CounterError> myTable = getTable();
				final CounterError counterError = myTable.getList()
						.get(myTable.convertRowIndexToModel(row));
				final String stackTrace = counterError.getStackTrace();
				if (stackTrace == null) {
					setToolTipText(null);
				} else {
					setToolTipText("<html>" + stackTrace.replace("[See nested", "\n[See nested")
							.replaceAll("\n", "<br/>"));
				}
			}
			return this;
		}
	}

	CounterErrorPanel(RemoteCollector remoteCollector, Counter counter) {
		super(remoteCollector);

		assert counter != null;
		assert counter.isErrorCounter();
		this.counter = counter;

		if (counter.getErrorsCount() == 0) {
			this.table = null;
			final JLabel noErrorsLabel = createNoErrorsLabel();
			add(noErrorsLabel, BorderLayout.CENTER);
		} else {
			final MTableScrollPane<CounterError> scrollPane = createScrollPane();
			this.table = scrollPane.getTable();
			final List<CounterError> errors = counter.getErrors();
			table.setList(errors);
			Utilities.adjustTableHeight(table);

			add(scrollPane, BorderLayout.CENTER);
		}

	}

	private MTableScrollPane<CounterError> createScrollPane() {
		final MTableScrollPane<CounterError> tableScrollPane = new MTableScrollPane<>();
		final MTable<CounterError> myTable = tableScrollPane.getTable();
		final List<CounterError> errors = counter.getErrors();
		final boolean displayUser = HtmlCounterErrorReport.shouldDisplayUser(errors);
		final boolean displayHttpRequest = HtmlCounterErrorReport.shouldDisplayHttpRequest(errors);
		if (errors.size() >= Counter.MAX_ERRORS_COUNT) {
			final JLabel warnLabel = new JLabel(' '
					+ getFormattedString("Dernieres_erreurs_seulement", Counter.MAX_ERRORS_COUNT));
			warnLabel.setFont(warnLabel.getFont().deriveFont(Font.BOLD));
			warnLabel.setForeground(Color.RED);
			add(warnLabel, BorderLayout.NORTH);
		}

		myTable.addColumn("date", getString("Date"));
		if (displayHttpRequest) {
			myTable.addColumn("httpRequest", getString("Requete"));
		}
		if (displayUser) {
			myTable.addColumn("remoteUser", getString("Utilisateur"));
		}
		myTable.addColumn("message", getString("Erreur"));

		final MDateTableCellRenderer dateTableCellRenderer = new MDateTableCellRenderer();
		dateTableCellRenderer.setDateFormat(DateFormat.getDateTimeInstance(DateFormat.SHORT,
				DateFormat.MEDIUM, I18N.getCurrentLocale()));
		myTable.setColumnCellRenderer("date", dateTableCellRenderer);
		myTable.setColumnCellRenderer("message", new MessageWithStackTraceTableCellRenderer());
		// invokeLater nécessaire pour que les dimensions et l'affichage soient corrects
		// avec le MessageWithStackTraceTableCellRenderer extends MMultiLineTableCellRenderer
		// (tester par exemple l'erreur de compilation dans la page jsp avec tomcat)
		SwingUtilities.invokeLater(new Runnable() {
			@Override
			public void run() {
				Utilities.adjustTableHeight(myTable);
			}
		});

		myTable.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseClicked(MouseEvent e) {
				if (e.getClickCount() == 2) {
					final CounterError counterError = getTable().getSelectedObject();
					if (counterError.getStackTrace() != null) {
						Utilities.showTextInPopup(CounterErrorPanel.this, counterError.getMessage(),
								counterError.getStackTrace());
					}
				}
			}
		});

		return tableScrollPane;
	}

	private JLabel createNoErrorsLabel() {
		return new JLabel(' ' + getString("Aucune_erreur"));
	}

	MTable<CounterError> getTable() {
		return table;
	}
}
