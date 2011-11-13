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
import java.awt.Font;
import java.util.List;

import javax.swing.JLabel;
import javax.swing.SwingConstants;

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
			if (mean < myCounterRequestAggregation.getWarningThreshold() || mean == 0) {
				// si cette moyenne est < à la moyenne globale + 1 écart-type (paramétrable), c'est bien
				// (si severeThreshold ou warningThreshold sont à 0 et mean à 0, c'est "info" et non "severe")
				setForeground(DARKER_GREEN);
				setFont(LABEL_PLAIN_FONT);
			} else if (mean < myCounterRequestAggregation.getSevereThreshold()) {
				// sinon, si cette moyenne est < à la moyenne globale + 2 écart-types (paramétrable),
				// attention à cette requête qui est plus longue que les autres
				setForeground(Color.ORANGE);
				setFont(LABEL_BOLD_FONT);
			} else {
				// sinon, (cette moyenne est > à la moyenne globale + 2 écart-types),
				// cette requête est très longue par rapport aux autres ;
				// il peut être opportun de l'optimiser si possible
				setForeground(Color.RED);
				setFont(LABEL_BOLD_FONT);
			}
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
			CounterRequestAggregation counterRequestAggregation) {
		super(remoteCollector);

		assert counter != null;
		assert counterRequestAggregation != null;
		this.counter = counter;
		this.counterRequestAggregation = counterRequestAggregation;

		if (CounterRequestTable.isRequestGraphDisplayed(counter)) {
			this.table = new CounterRequestTable(remoteCollector);
		} else {
			this.table = new MTable<CounterRequest>();
		}

		addColumns();

		final MTableScrollPane<CounterRequest> tableScrollPane = new MTableScrollPane<CounterRequest>(
				table);
		add(tableScrollPane, BorderLayout.CENTER);
	}

	private void addColumns() {
		final String nameColumnHeader;
		if (isJobCounter()) {
			nameColumnHeader = I18N.getString("Job");
		} else if (isErrorCounter()) {
			nameColumnHeader = I18N.getString("Erreur");
		} else {
			nameColumnHeader = I18N.getString("Requete");
		}
		table.addColumn("name", nameColumnHeader);
		// MMultiLineTableCellRenderer n'est pas défini ici pour la colonne "name"
		// car les dimensions de certains scrollPane ne seraient pas corrects

		final MIntegerTableCellRenderer meanCellRenderer = new MeanTableCellRenderer();
		if (counterRequestAggregation.isTimesDisplayed()) {
			table.addColumn("durationsSum", I18N.getString("temps_cumule"));
			table.addColumn("hits", I18N.getString("Hits"));
			table.addColumn("mean", I18N.getString("Temps_moyen"));
			table.addColumn("maximum", I18N.getString("Temps_max"));
			table.addColumn("standardDeviation", I18N.getString("Ecart_type"));
			table.setColumnCellRenderer("durationsSum", new DurationPercentageTableCellRenderer());
			table.setColumnCellRenderer("mean", meanCellRenderer);
		} else {
			table.addColumn("hits", I18N.getString("Hits"));
		}
		if (counterRequestAggregation.isCpuTimesDisplayed()) {
			table.addColumn("cpuTimeSum", I18N.getString("temps_cpu_cumule"));
			table.addColumn("cpuTimeMean", I18N.getString("Temps_cpu_moyen"));
			table.setColumnCellRenderer("cpuTimeSum", new CpuPercentageTableCellRenderer());
			table.setColumnCellRenderer("cpuTimeMean", meanCellRenderer);
		}
		if (!isErrorAndNotJobCounter()) {
			table.addColumn("systemErrorPercentage", I18N.getString("erreur_systeme"));
		}
		if (counterRequestAggregation.isResponseSizeDisplayed()) {
			table.addColumn("responseSizeMean", I18N.getString("Taille_moyenne"));
			table.setColumnCellRenderer("responseSizeMean", new ResponseSizeMeanTableCellRenderer());
		}
		if (counterRequestAggregation.isChildHitsDisplayed()) {
			table.addColumn("childHitsMean",
					I18N.getFormattedString("hits_fils_moyens", counter.getChildCounterName()));
			table.addColumn("childDurationsMean",
					I18N.getFormattedString("temps_fils_moyen", counter.getChildCounterName()));
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
}
