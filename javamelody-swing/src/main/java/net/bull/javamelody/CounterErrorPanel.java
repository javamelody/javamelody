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
import java.awt.Dimension;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.text.DateFormat;
import java.util.List;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTable;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingUtilities;

import net.bull.javamelody.table.MDateTableCellRenderer;
import net.bull.javamelody.table.MDefaultTableCellRenderer;
import net.bull.javamelody.table.MTable;
import net.bull.javamelody.table.MTableScrollPane;

import com.lowagie.text.Font;

/**
 * Panel des erreurs http et dans les logs ou dans les jobs.
 * @author Emeric Vernat
 */
class CounterErrorPanel extends JPanel {
	private static final long serialVersionUID = 1L;

	private final Counter counter;
	private final MTable<CounterError> table;

	private final class MessageWithStackTraceTableCellRenderer extends MDefaultTableCellRenderer {
		private static final long serialVersionUID = 1L;

		MessageWithStackTraceTableCellRenderer() {
			super();
		}

		@Override
		public Component getTableCellRendererComponent(JTable jtable, Object value,
				boolean isSelected, boolean hasFocus, int row, int column) {
			// tooltip selon la stackTrace
			if (row == -1) {
				setToolTipText(null);
			} else {
				final MTable<CounterError> myTable = getTable();
				final CounterError counterError = myTable.getList().get(
						myTable.convertRowIndexToModel(row));
				final String stackTrace = counterError.getStackTrace();
				if (stackTrace == null) {
					setToolTipText(null);
				} else {
					setToolTipText("<html>"
							+ stackTrace.replace("[See nested", "\n[See nested").replaceAll("\n",
									"<br/>"));
				}
			}
			// et texte selon la valeur (message de l'erreur)
			return super.getTableCellRendererComponent(jtable, value, isSelected, hasFocus, row,
					column);
		}
	}

	CounterErrorPanel(Counter counter) {
		super(new BorderLayout());

		setOpaque(false);

		assert counter != null;
		assert counter.isErrorCounter();
		this.counter = counter;

		if (counter.getErrorsCount() == 0) {
			this.table = null;
			addNoErrors();
		} else {
			this.table = new MTable<CounterError>();
			addScrollPane();
		}

	}

	private void addScrollPane() {
		final List<CounterError> errors = counter.getErrors();
		final boolean displayUser = HtmlCounterErrorReport.shouldDisplayUser(errors);
		final boolean displayHttpRequest = HtmlCounterErrorReport.shouldDisplayHttpRequest(errors);
		if (errors.size() >= Counter.MAX_ERRORS_COUNT) {
			final JLabel warnLabel = new JLabel(' ' + I18N.getFormattedString(
					"Dernieres_erreurs_seulement", Counter.MAX_ERRORS_COUNT));
			warnLabel.setFont(warnLabel.getFont().deriveFont(Font.BOLD));
			warnLabel.setForeground(Color.RED);
			add(warnLabel, BorderLayout.NORTH);
		}

		table.addColumn("date", I18N.getString("Date"));
		if (displayHttpRequest) {
			table.addColumn("httpRequest", I18N.getString("Requete"));
		}
		if (displayUser) {
			table.addColumn("remoteUser", I18N.getString("Utilisateur"));
		}
		table.addColumn("message", I18N.getString("Erreur"));

		final MDateTableCellRenderer dateTableCellRenderer = new MDateTableCellRenderer();
		dateTableCellRenderer.setDateFormat(DateFormat.getDateTimeInstance(DateFormat.SHORT,
				DateFormat.MEDIUM, I18N.getCurrentLocale()));
		table.setColumnCellRenderer("date", dateTableCellRenderer);
		table.setColumnCellRenderer("message", new MessageWithStackTraceTableCellRenderer());
		table.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseClicked(MouseEvent e) {
				if (e.getClickCount() == 2) {
					final CounterError counterError = getTable().getSelectedObject();
					if (counterError.getStackTrace() != null) {
						ThreadInformationsPanel.showTextInPopup(CounterErrorPanel.this,
								counterError.getMessage(), counterError.getStackTrace());
					}
				}
			}
		});

		table.setList(errors);

		table.setPreferredScrollableViewportSize(new Dimension(-1, table.getPreferredSize().height));
		final MTableScrollPane<CounterError> tableScrollPane = new MTableScrollPane<CounterError>(
				table);
		add(tableScrollPane, BorderLayout.CENTER);
		SwingUtilities.invokeLater(new Runnable() {
			@Override
			public void run() {
				tableScrollPane
						.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER);
			}
		});
	}

	private void addNoErrors() {
		add(new JLabel(' ' + I18N.getString("Aucune_erreur")), BorderLayout.CENTER);
	}

	MTable<CounterError> getTable() {
		return table;
	}
}
