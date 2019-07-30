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
import java.awt.Font;
import java.util.List;

import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.SwingConstants;

import net.bull.javamelody.internal.model.Counter;
import net.bull.javamelody.internal.model.CounterRequest;
import net.bull.javamelody.internal.model.CounterRequestAggregation;
import net.bull.javamelody.internal.model.RemoteCollector;
import net.bull.javamelody.swing.table.MDefaultTableCellRenderer;
import net.bull.javamelody.swing.table.MIntegerTableCellRenderer;
import net.bull.javamelody.swing.table.MTable;
import net.bull.javamelody.swing.table.MTableScrollPane;

/**
 * Panel des statistiques.
 * @author Emeric Vernat
 */
class StatisticsTablePanel extends MelodyPanel {
	static final Color DARKER_GREEN = Color.GREEN.darker();
	static final Font LABEL_BOLD_FONT = new JLabel().getFont().deriveFont(Font.BOLD);
	static final Font LABEL_PLAIN_FONT = new JLabel().getFont().deriveFont(Font.PLAIN);

	private static final long serialVersionUID = 1L;

	@SuppressWarnings("all")
	private final CounterRequestAggregation counterRequestAggregation;
	private final Counter counter;
	private final MTable<CounterRequest> table;

	private static final class ResponseSizeMeanTableCellRenderer extends MIntegerTableCellRenderer {
		private static final long serialVersionUID = 1L;

		ResponseSizeMeanTableCellRenderer() {
			super();
		}

		@Override
		public void setValue(final Object value) {
			super.setValue((Integer) value / 1024);
		}
	}

	private class MeanTableCellRenderer extends MIntegerTableCellRenderer {
		private static final long serialVersionUID = 1L;

		MeanTableCellRenderer() {
			super();
		}

		@Override
		public void setValue(Object value) {
			final Integer mean = (Integer) value;
			final CounterRequestAggregation myCounterRequestAggregation = getCounterRequestAggregation();
			setStyleBasedOnThresholds(this, mean, myCounterRequestAggregation);
			super.setValue(mean);
		}
	}

	private class DurationPercentageTableCellRenderer extends MDefaultTableCellRenderer {
		private static final long serialVersionUID = 1L;

		DurationPercentageTableCellRenderer() {
			super();
			setHorizontalAlignment(SwingConstants.RIGHT);
		}

		@Override
		public void setValue(final Object value) {
			final CounterRequest globalRequest = getCounterRequestAggregation().getGlobalRequest();
			if (globalRequest.getDurationsSum() == 0) {
				setText("0");
			} else {
				setText(String.valueOf(100 * (Long) value / globalRequest.getDurationsSum()));
			}
		}
	}

	private class CpuPercentageTableCellRenderer extends MDefaultTableCellRenderer {
		private static final long serialVersionUID = 1L;

		CpuPercentageTableCellRenderer() {
			super();
			setHorizontalAlignment(SwingConstants.RIGHT);
		}

		@Override
		public void setValue(final Object value) {
			final CounterRequest globalRequest = getCounterRequestAggregation().getGlobalRequest();
			if (globalRequest.getCpuTimeSum() == 0) {
				setText("0");
			} else {
				setText(String.valueOf(100 * (Long) value / globalRequest.getCpuTimeSum()));
			}
		}
	}

	StatisticsTablePanel(RemoteCollector remoteCollector, Counter counter,
			CounterRequestAggregation counterRequestAggregation, boolean includeGraph) {
		super(remoteCollector);

		assert counter != null;
		assert counterRequestAggregation != null;
		this.counter = counter;
		this.counterRequestAggregation = counterRequestAggregation;

		if (includeGraph) {
			this.table = new CounterRequestTable(remoteCollector);
		} else {
			this.table = new MTable<>();
		}

		addColumns();

		final MTableScrollPane<CounterRequest> tableScrollPane = new MTableScrollPane<>(table);
		add(tableScrollPane, BorderLayout.CENTER);
	}

	private void addColumns() {
		final String nameColumnHeader;
		if (isJobCounter()) {
			nameColumnHeader = getString("Job");
		} else if (isErrorCounter()) {
			nameColumnHeader = getString("Erreur");
		} else {
			nameColumnHeader = getString("Requete");
		}
		table.addColumn("name", nameColumnHeader);
		// MMultiLineTableCellRenderer n'est pas défini ici pour la colonne "name"
		// car les dimensions de certains scrollPane ne seraient pas corrects

		final MIntegerTableCellRenderer meanCellRenderer = new MeanTableCellRenderer();
		if (counterRequestAggregation.isTimesDisplayed()) {
			table.addColumn("durationsSum", getString("temps_cumule"));
			table.addColumn("hits", getString("Hits"));
			table.addColumn("mean", getString("Temps_moyen"));
			table.addColumn("maximum", getString("Temps_max"));
			table.addColumn("standardDeviation", getString("Ecart_type"));
			table.setColumnCellRenderer("durationsSum", new DurationPercentageTableCellRenderer());
			table.setColumnCellRenderer("mean", meanCellRenderer);
		} else {
			table.addColumn("hits", getString("Hits"));
		}
		if (counterRequestAggregation.isCpuTimesDisplayed()) {
			table.addColumn("cpuTimeSum", getString("temps_cpu_cumule"));
			table.addColumn("cpuTimeMean", getString("Temps_cpu_moyen"));
			table.setColumnCellRenderer("cpuTimeSum", new CpuPercentageTableCellRenderer());
			table.setColumnCellRenderer("cpuTimeMean", meanCellRenderer);
		}
		if (counterRequestAggregation.isAllocatedKBytesDisplayed()) {
			table.addColumn("allocatedKBytesMean", getString("Ko_alloues_moyens"));
		}
		if (!isErrorAndNotJobCounter()) {
			table.addColumn("systemErrorPercentage", getString("erreur_systeme"));
		}
		if (counterRequestAggregation.isResponseSizeDisplayed()) {
			table.addColumn("responseSizeMean", getString("Taille_moyenne"));
			table.setColumnCellRenderer("responseSizeMean",
					new ResponseSizeMeanTableCellRenderer());
		}
		if (counterRequestAggregation.isChildHitsDisplayed()) {
			table.addColumn("childHitsMean",
					getFormattedString("hits_fils_moyens", counter.getChildCounterName()));
			table.addColumn("childDurationsMean",
					getFormattedString("temps_fils_moyen", counter.getChildCounterName()));
		}
	}

	void setList(List<CounterRequest> requests) {
		table.setList(requests);
	}

	MTable<CounterRequest> getTable() {
		return table;
	}

	CounterRequestAggregation getCounterRequestAggregation() {
		return counterRequestAggregation;
	}

	private boolean isErrorCounter() {
		return counter.isErrorCounter();
	}

	private boolean isJobCounter() {
		return counter.isJobCounter();
	}

	private boolean isErrorAndNotJobCounter() {
		return isErrorCounter() && !isJobCounter();
	}

	static void setStyleBasedOnThresholds(JComponent target, Integer duration,
			CounterRequestAggregation aggregation) {
		if (duration < aggregation.getWarningThreshold() || duration == 0) {
			// si cette moyenne est < à la moyenne globale + 1 écart-type (paramétrable), c'est bien
			// (si severeThreshold ou warningThreshold sont à 0 et mean à 0, c'est "info" et non "severe")
			target.setForeground(DARKER_GREEN);
			target.setFont(LABEL_PLAIN_FONT);
		} else if (duration < aggregation.getSevereThreshold()) {
			// sinon, si cette moyenne est < à la moyenne globale + 2 écart-types (paramétrable),
			// attention à cette requête qui est plus longue que les autres
			target.setForeground(Color.ORANGE);
			target.setFont(LABEL_BOLD_FONT);
		} else {
			// sinon, (cette moyenne est > à la moyenne globale + 2 écart-types),
			// cette requête est très longue par rapport aux autres ;
			// il peut être opportun de l'optimiser si possible
			target.setForeground(Color.RED);
			target.setFont(LABEL_BOLD_FONT);
		}
	}
}
