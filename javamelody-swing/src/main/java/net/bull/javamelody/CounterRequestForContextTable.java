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

import java.awt.Color;
import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.text.DecimalFormat;

import javax.swing.BorderFactory;
import javax.swing.JTable;
import javax.swing.border.MatteBorder;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;

import net.bull.javamelody.internal.common.I18N;
import net.bull.javamelody.internal.model.Counter;
import net.bull.javamelody.internal.model.CounterRequest;
import net.bull.javamelody.internal.model.CounterRequestAggregation;
import net.bull.javamelody.internal.model.CounterRequestContext;
import net.bull.javamelody.internal.model.CounterRequestContextData;
import net.bull.javamelody.internal.model.RemoteCollector;
import net.bull.javamelody.internal.model.ThreadInformations;
import net.bull.javamelody.swing.MButton;
import net.bull.javamelody.swing.table.MDefaultTableCellRenderer;

/**
 * Tableau de requêtes avec les colonnes des requêtes en cours.
 * @author Emeric Vernat
 */
class CounterRequestForContextTable extends CounterRequestTable {
	private static final long serialVersionUID = 1L;

	private CounterRequestContextData data;

	private abstract class CounterRequestTableCellRenderer extends MDefaultTableCellRenderer {
		private static final long serialVersionUID = 1L;
		private final MatteBorder contextBorder = BorderFactory.createMatteBorder(1, 0, 0, 0,
				Color.GRAY);

		CounterRequestTableCellRenderer() {
			super();
		}

		protected CounterRequest getCounterRequest(int row) {
			final int modelRow = convertRowIndexToModel(row);
			return getData().getAllRequests().get(modelRow);
		}

		protected CounterRequestContext getCounterRequestContext(int row) {
			final int modelRow = convertRowIndexToModel(row);
			return getData().getAllContexts().get(modelRow);
		}

		int getParentContextLevel(int row) {
			CounterRequestContext counterRequestContext = getCounterRequestContext(row);
			int level = 0;
			while (counterRequestContext.getParentContext() != null) {
				level++;
				counterRequestContext = counterRequestContext.getParentContext();
			}
			return level;
		}

		@Override
		public Component getTableCellRendererComponent(JTable jtable, Object value,
				boolean isSelected, boolean hasFocus, int row, int column) {
			final Component result = super.getTableCellRendererComponent(jtable, value, isSelected,
					hasFocus, row, column);
			if (row != 0 && getParentContextLevel(row) == 0) {
				setBorder(BorderFactory.createCompoundBorder(contextBorder, getBorder()));
			}
			return result;
		}
	}

	private final class ThreadTableCellRenderer extends CounterRequestTableCellRenderer {
		private static final long serialVersionUID = 1L;

		ThreadTableCellRenderer() {
			super();
		}

		@Override
		public Component getTableCellRendererComponent(JTable jtable, Object value,
				boolean isSelected, boolean hasFocus, int row, int column) {
			final CounterRequestContext counterRequestContext = getCounterRequestContext(row);
			final ThreadInformations threadInformations = getData()
					.getThreadInformationsByCounterRequestContext(counterRequestContext);
			final String threadName;
			if (threadInformations == null) {
				threadName = null; // un décalage n'a pas permis de récupérer le thread de ce context
				setToolTipText(null);
			} else {
				threadName = threadInformations.getName();
				setToolTipText(ThreadInformationsPanel.convertStackTraceToHtml(
						threadInformations.getName(), threadInformations.getStackTrace()));
			}
			return super.getTableCellRendererComponent(jtable, threadName, isSelected, hasFocus,
					row, column);
		}
	}

	private final class ExecutedMethodTableCellRenderer extends CounterRequestTableCellRenderer {
		private static final long serialVersionUID = 1L;

		ExecutedMethodTableCellRenderer() {
			super();
		}

		@Override
		public Component getTableCellRendererComponent(JTable jtable, Object value,
				boolean isSelected, boolean hasFocus, int row, int column) {
			final CounterRequestContext counterRequestContext = getCounterRequestContext(row);
			final ThreadInformations threadInformations = getData()
					.getThreadInformationsByCounterRequestContext(counterRequestContext);
			final String executedMethod;
			if (threadInformations == null) {
				executedMethod = null; // un décalage n'a pas permis de récupérer le thread de ce context
			} else {
				executedMethod = threadInformations.getExecutedMethod();
			}

			return super.getTableCellRendererComponent(jtable, executedMethod, isSelected, hasFocus,
					row, column);
		}
	}

	private final class RemoteUserTableCellRenderer extends CounterRequestTableCellRenderer {
		private static final long serialVersionUID = 1L;

		RemoteUserTableCellRenderer() {
			super();
		}

		@Override
		public Component getTableCellRendererComponent(JTable jtable, Object value,
				boolean isSelected, boolean hasFocus, int row, int column) {
			final CounterRequestContext counterRequestContext = getCounterRequestContext(row);
			final String remoteUser = counterRequestContext.getRemoteUser();
			return super.getTableCellRendererComponent(jtable, remoteUser, isSelected, hasFocus,
					row, column);
		}
	}

	private final class NameTableCellRenderer extends CounterRequestTableCellRenderer {
		private static final long serialVersionUID = 1L;

		NameTableCellRenderer() {
			super();
		}

		@Override
		public Component getTableCellRendererComponent(JTable jtable, Object value,
				boolean isSelected, boolean hasFocus, int row, int column) {
			final CounterRequestContext context = getCounterRequestContext(row);
			final int margin = 10 * getParentContextLevel(row);
			final Counter counter = context.getParentCounter();
			setIcon(CounterRequestAbstractPanel.getCounterIcon(counter, margin));
			return super.getTableCellRendererComponent(jtable, value, isSelected, hasFocus, row,
					column);
		}
	}

	private class IntegerTableCellRenderer extends CounterRequestTableCellRenderer {
		private static final long serialVersionUID = 1L;

		private final Integer minusOne = Integer.valueOf(-1);
		private final DecimalFormat integerFormat = I18N.createIntegerFormat();

		IntegerTableCellRenderer() {
			super();
			setHorizontalAlignment(RIGHT);
		}

		@Override
		public void setValue(Object value) {
			final String text;
			if (value != null && !minusOne.equals(value)) {
				text = integerFormat.format(value);
			} else {
				text = null;
			}
			super.setValue(text);
		}
	}

	private final class DurationTableCellRenderer extends IntegerTableCellRenderer {
		private static final long serialVersionUID = 1L;

		DurationTableCellRenderer() {
			super();
		}

		@Override
		public Component getTableCellRendererComponent(JTable jtable, Object value,
				boolean isSelected, boolean hasFocus, int row, int column) {
			final CounterRequestContext context = getCounterRequestContext(row);
			final int margin = 10 * getParentContextLevel(row);
			final Counter counter = context.getParentCounter();
			setIcon(CounterRequestAbstractPanel.getCounterIcon(counter, margin));
			final CounterRequestContext counterRequestContext = getCounterRequestContext(row);
			final int duration = counterRequestContext
					.getDuration(counterRequestContext.getParentCounter().getStartDate().getTime());
			final CounterRequestAggregation aggregation = getData()
					.getAggregationForCounter(counter);
			final Component result = super.getTableCellRendererComponent(jtable, duration,
					isSelected, hasFocus, row, column);
			StatisticsTablePanel.setStyleBasedOnThresholds(this, duration, aggregation);
			return result;
		}
	}

	private final class CpuTableCellRenderer extends IntegerTableCellRenderer {
		private static final long serialVersionUID = 1L;

		CpuTableCellRenderer() {
			super();
		}

		@Override
		public Component getTableCellRendererComponent(JTable jtable, Object value,
				boolean isSelected, boolean hasFocus, int row, int column) {
			final Integer cpu;
			final CounterRequest counterRequest = getCounterRequest(row);
			if (counterRequest != null && counterRequest.getCpuTimeMean() >= 0) {
				final CounterRequestContext counterRequestContext = getCounterRequestContext(row);
				cpu = counterRequestContext.getCpuTime();
			} else {
				cpu = null;
			}
			return super.getTableCellRendererComponent(jtable, cpu, isSelected, hasFocus, row,
					column);
		}
	}

	private final class TotalChildHitsTableCellRenderer extends IntegerTableCellRenderer {
		private static final long serialVersionUID = 1L;

		TotalChildHitsTableCellRenderer() {
			super();
		}

		@Override
		public Component getTableCellRendererComponent(JTable jtable, Object value,
				boolean isSelected, boolean hasFocus, int row, int column) {
			final Integer totalChildHits;
			final CounterRequestContext context = getCounterRequestContext(row);
			if (context.getParentCounter().getChildCounterName() == null) {
				// si le compteur parent du contexte n'a pas de compteur fils
				// (comme le compteur sql et au contraire du compteur http),
				// alors la valeur de childHits ou childDurations n'a pas de sens
				// (comme les hits sql fils pour une requête sql)
				totalChildHits = -1;
			} else {
				totalChildHits = context.getTotalChildHits();
			}
			return super.getTableCellRendererComponent(jtable, totalChildHits, isSelected, hasFocus,
					row, column);
		}
	}

	private final class ChildHitsMeanTableCellRenderer extends IntegerTableCellRenderer {
		private static final long serialVersionUID = 1L;

		ChildHitsMeanTableCellRenderer() {
			super();
		}

		@Override
		public Component getTableCellRendererComponent(JTable jtable, Object value,
				boolean isSelected, boolean hasFocus, int row, int column) {
			final Integer childHitsMean;
			final CounterRequestContext context = getCounterRequestContext(row);
			if (context.getParentCounter().getChildCounterName() == null) {
				// si le compteur parent du contexte n'a pas de compteur fils
				// (comme le compteur sql et au contraire du compteur http),
				// alors la valeur de childHits ou childDurations n'a pas de sens
				// (comme les hits sql fils pour une requête sql)
				childHitsMean = -1;
			} else {
				final CounterRequest counterRequest = getCounterRequest(row);
				if (counterRequest == null) {
					childHitsMean = null;
				} else {
					childHitsMean = counterRequest.getChildHitsMean();
				}
			}
			return super.getTableCellRendererComponent(jtable, childHitsMean, isSelected, hasFocus,
					row, column);
		}
	}

	private final class TotalChildDurationsSumTableCellRenderer extends IntegerTableCellRenderer {
		private static final long serialVersionUID = 1L;

		TotalChildDurationsSumTableCellRenderer() {
			super();
		}

		@Override
		public Component getTableCellRendererComponent(JTable jtable, Object value,
				boolean isSelected, boolean hasFocus, int row, int column) {
			final Integer totalChildDurationsSum;
			final CounterRequestContext context = getCounterRequestContext(row);
			if (context.getParentCounter().getChildCounterName() == null) {
				// si le compteur parent du contexte n'a pas de compteur fils
				// (comme le compteur sql et au contraire du compteur http),
				// alors la valeur de childHits ou childDurations n'a pas de sens
				// (comme les hits sql fils pour une requête sql)
				totalChildDurationsSum = -1;
			} else {
				totalChildDurationsSum = context.getTotalChildDurationsSum();
			}
			return super.getTableCellRendererComponent(jtable, totalChildDurationsSum, isSelected,
					hasFocus, row, column);
		}
	}

	private final class ChildsDurationsMeanTableCellRenderer extends IntegerTableCellRenderer {
		private static final long serialVersionUID = 1L;

		ChildsDurationsMeanTableCellRenderer() {
			super();
		}

		@Override
		public Component getTableCellRendererComponent(JTable jtable, Object value,
				boolean isSelected, boolean hasFocus, int row, int column) {
			final Integer childDurationsMean;
			final CounterRequestContext context = getCounterRequestContext(row);
			if (context.getParentCounter().getChildCounterName() == null) {
				// si le compteur parent du contexte n'a pas de compteur fils
				// (comme le compteur sql et au contraire du compteur http),
				// alors la valeur de childHits ou childDurations n'a pas de sens
				// (comme les hits sql fils pour une requête sql)
				childDurationsMean = -1;
			} else {
				final CounterRequest counterRequest = getCounterRequest(row);
				if (counterRequest == null) {
					childDurationsMean = null;
				} else {
					childDurationsMean = counterRequest.getChildDurationsMean();
				}
			}
			return super.getTableCellRendererComponent(jtable, childDurationsMean, isSelected,
					hasFocus, row, column);
		}
	}

	CounterRequestForContextTable(RemoteCollector remoteCollector) {
		super(remoteCollector);
	}

	void init(CounterRequestContextData newData) {
		this.data = newData;
		addColumns();
		setList(data.getAllRequests());
	}

	CounterRequestContextData getData() {
		return data;
	}

	private void addColumns() {
		final boolean remoteUserDisplayed = getData().isRemoteUserDisplayed();
		final boolean childHitsDisplayed = getData().isChildHitsDisplayed();
		final boolean stackTraceEnabled = getData().isStackTraceEnabled();
		addCustomColumn(getString("Thread"), new ThreadTableCellRenderer());
		if (remoteUserDisplayed) {
			addCustomColumn(getString("Utilisateur"), new RemoteUserTableCellRenderer());
		}
		addColumn("name", getString("Requete"));
		setColumnCellRenderer("name", new NameTableCellRenderer());

		addCustomColumn(getString("Duree_ecoulee"), new DurationTableCellRenderer());

		addColumn("mean", getString("Temps_moyen"));
		setColumnCellRenderer("mean", new IntegerTableCellRenderer());

		addCustomColumn(getString("Temps_cpu"), new CpuTableCellRenderer());

		addColumn("cpuTimeMean", getString("Temps_cpu_moyen"));
		setColumnCellRenderer("cpuTimeMean", new IntegerTableCellRenderer());
		// rq : tous ces contextes viennent du même compteur donc peu importe lequel des parentCounter
		if (childHitsDisplayed) {
			final String childCounterName = getData().getRootContexts().get(0).getParentCounter()
					.getChildCounterName();
			addCustomColumn(getFormattedString("hits_fils", childCounterName),
					new TotalChildHitsTableCellRenderer());

			addCustomColumn(getFormattedString("hits_fils_moyens", childCounterName),
					new ChildHitsMeanTableCellRenderer());

			addCustomColumn(getFormattedString("temps_fils", childCounterName),
					new TotalChildDurationsSumTableCellRenderer());

			addCustomColumn(getFormattedString("temps_fils_moyen", childCounterName),
					new ChildsDurationsMeanTableCellRenderer());
		}
		if (stackTraceEnabled) {
			addCustomColumn(getString("Methode_executee"), new ExecutedMethodTableCellRenderer());
		}
	}

	private void addCustomColumn(String headerValue, TableCellRenderer tableCellRenderer) {
		final TableColumn tableColumn = new TableColumn(getColumnCount());
		tableColumn.setIdentifier(getColumnCount());
		tableColumn.setHeaderValue(headerValue);
		tableColumn.setCellRenderer(tableCellRenderer);
		addColumn(tableColumn);
	}

	MButton createKillThreadButton(final CounterRequestContextPanel contextPanel) {
		final MButton killThreadButton = new MButton(getString("Tuer"),
				ImageIconCache.getImageIcon("stop.png"));
		getSelectionModel().addListSelectionListener(new ListSelectionListener() {
			@Override
			public void valueChanged(ListSelectionEvent e) {
				final ThreadInformations threadInformations = getSelectedThreadInformations();
				killThreadButton.setEnabled(threadInformations != null);
				if (threadInformations != null) {
					killThreadButton.setToolTipText(
							getFormattedString("kill_thread", threadInformations.getName()));
				} else {
					killThreadButton.setToolTipText(null);
				}
			}
		});
		killThreadButton.setEnabled(getSelectedThreadInformations() != null);
		killThreadButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				final ThreadInformations threadInformations = getSelectedThreadInformations();
				contextPanel.actionKillThread(threadInformations);
			}
		});
		return killThreadButton;
	}

	ThreadInformations getSelectedThreadInformations() {
		final int selectedRow = getSelectedRow();
		if (selectedRow >= 0) {
			final int modelRow = convertRowIndexToModel(selectedRow);
			final CounterRequestContext counterRequestContext = getData().getAllContexts()
					.get(modelRow);
			return getData().getThreadInformationsByCounterRequestContext(counterRequestContext);
		}
		return null;
	}

	/**
	 * Retourne une traduction dans la locale courante.
	 * @param key clé d'un libellé dans les fichiers de traduction
	 * @return String
	 */
	static String getString(String key) {
		return I18N.getString(key);
	}

	/**
	 * Retourne une traduction dans la locale courante et insère les arguments aux positions {i}.
	 * @param key clé d'un libellé dans les fichiers de traduction
	 * @param arguments Valeur à inclure dans le résultat
	 * @return String
	 */
	static String getFormattedString(String key, Object... arguments) {
		return I18N.getFormattedString(key, arguments);
	}
}
