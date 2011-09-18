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
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import java.text.DecimalFormat;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingConstants;

import net.bull.javamelody.swing.MButton;
import net.bull.javamelody.swing.Utilities;
import net.bull.javamelody.swing.table.MDefaultTableCellRenderer;
import net.bull.javamelody.swing.table.MIntegerTableCellRenderer;
import net.bull.javamelody.swing.table.MTable;
import net.bull.javamelody.swing.table.MTableScrollPane;

/**
 * Panel des statistiques.
 * @author Emeric Vernat
 */
class StatisticsPanel extends MelodyPanel { // NOPMD
	static final ImageIcon PLUS_ICON = ImageIconCache.getImageIcon("bullets/plus.png");
	static final ImageIcon MINUS_ICON = ImageIconCache.getImageIcon("bullets/minus.png");

	private static final long serialVersionUID = 1L;

	@SuppressWarnings("all")
	private final CounterRequestAggregation counterRequestAggregation;
	private final Counter counter;
	private final Range range;
	private final MTable<CounterRequest> table;
	private final JPanel mainPanel;
	private StatisticsPanel detailsPanel;
	private JPanel lastErrorsPanel;

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
				setForeground(Color.GREEN.darker());
				setFont(getFont().deriveFont(Font.PLAIN));
			} else if (mean < myCounterRequestAggregation.getSevereThreshold()) {
				// sinon, si cette moyenne est < à la moyenne globale + 2 écart-types (paramétrable),
				// attention à cette requête qui est plus longue que les autres
				setForeground(Color.ORANGE);
				setFont(getFont().deriveFont(Font.BOLD));
			} else {
				// sinon, (cette moyenne est > à la moyenne globale + 2 écart-types),
				// cette requête est très longue par rapport aux autres ;
				// il peut être opportun de l'optimiser si possible
				setForeground(Color.RED);
				setFont(getFont().deriveFont(Font.BOLD));
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

	StatisticsPanel(RemoteCollector remoteCollector, Counter counter, Range range) {
		this(remoteCollector, counter, range, null);
	}

	private StatisticsPanel(RemoteCollector remoteCollector, Counter counter, Range range,
			CounterRequestAggregation counterRequestAggregation) {
		super(remoteCollector, new BorderLayout());

		assert counter != null;
		assert range != null;
		this.counter = counter;
		this.range = range;

		if (counter.getRequestsCount() == 0) {
			this.table = null;
			this.counterRequestAggregation = null;
		} else {
			this.table = new MTable<CounterRequest>();
			if (counterRequestAggregation == null) {
				this.counterRequestAggregation = new CounterRequestAggregation(counter);
			} else {
				this.counterRequestAggregation = counterRequestAggregation;
			}
			addColumns();
		}
		this.mainPanel = new JPanel(new BorderLayout());
		this.mainPanel.setOpaque(false);
		add(this.mainPanel, BorderLayout.NORTH);

		// TODO
		//		// 3. détails par requêtes (non visible par défaut)
		//		writeln("<div id='details" + counterName + "' style='display: none;'>");
		//		writeRequests(counterName, counter.getChildCounterName(), requests,
		//				isRequestGraphDisplayed(counter), true, false);
		//		writeln("</div>");
	}

	private void addColumns() {
		table.addColumn("name", I18N.getString("Requete"));
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

	public void showGlobalRequests() {
		if (counter.getRequestsCount() == 0) {
			addNoRequests();
		} else {
			List<CounterRequest> requests = counterRequestAggregation.getRequests();
			final CounterRequest globalRequest = counterRequestAggregation.getGlobalRequest();
			if (isErrorAndNotJobCounter()) {
				// il y a au moins une "request" d'erreur puisque la liste n'est pas vide
				assert !requests.isEmpty();
				requests = Collections.singletonList(requests.get(0));
			} else {
				requests = Arrays.asList(globalRequest,
						counterRequestAggregation.getWarningRequest(),
						counterRequestAggregation.getSevereRequest());
			}
			table.setList(requests);

			addScrollPane();
			addRequestsSizeAndButtons();
		}
	}

	void showDetailRequests() {
		if (detailsPanel == null) {
			detailsPanel = new StatisticsPanel(getRemoteCollector(), counter, range,
					counterRequestAggregation);
			detailsPanel.setVisible(false);
			final List<CounterRequest> requests = counterRequestAggregation.getRequests();
			detailsPanel.table.setList(requests);
			detailsPanel.addScrollPane();

			add(detailsPanel, BorderLayout.CENTER);
		}
		detailsPanel.setVisible(!detailsPanel.isVisible());
		detailsPanel.validate();
	}

	void showLastErrors() {
		if (lastErrorsPanel == null) {
			lastErrorsPanel = new CounterErrorPanel(counter);
			lastErrorsPanel.setVisible(false);

			add(lastErrorsPanel, BorderLayout.SOUTH);
		}
		lastErrorsPanel.setVisible(!lastErrorsPanel.isVisible());
		lastErrorsPanel.validate();
	}

	private void addNoRequests() {
		final String key;
		if (isJobCounter()) {
			key = "Aucun_job";
		} else if (isErrorCounter()) {
			key = "Aucune_erreur";
		} else {
			key = "Aucune_requete";
		}
		mainPanel.add(new JLabel(' ' + I18N.getString(key)), BorderLayout.CENTER);
	}

	private void addScrollPane() {
		Utilities.adjustTableHeight(table);
		final MTableScrollPane<CounterRequest> tableScrollPane = new MTableScrollPane<CounterRequest>(
				table);
		mainPanel.add(tableScrollPane, BorderLayout.NORTH);
	}

	private void addRequestsSizeAndButtons() {
		final CounterRequest globalRequest = this.counterRequestAggregation.getGlobalRequest();
		final long end;
		if (range.getEndDate() != null) {
			// l'utilisateur a choisi une période personnalisée de date à date,
			// donc la fin est peut-être avant la date du jour
			end = Math.min(range.getEndDate().getTime(), System.currentTimeMillis());
		} else {
			end = System.currentTimeMillis();
		}
		// delta ni négatif ni à 0
		final long deltaMillis = Math.max(end - counter.getStartDate().getTime(), 1);
		final long hitsParMinute = 60 * 1000 * globalRequest.getHits() / deltaMillis;
		// Rq : si serveur utilisé de 8h à 20h (soit 12h) on peut multiplier par 2 ces hits par minute indiqués
		// pour avoir une moyenne sur les heures d'activité sans la nuit
		final String nbKey;
		if (isJobCounter()) {
			nbKey = "nb_jobs";
		} else if (isErrorCounter()) {
			nbKey = "nb_erreurs";
		} else {
			nbKey = "nb_requetes";
		}
		final DecimalFormat integerFormat = I18N.createIntegerFormat();
		final String text = I18N.getFormattedString(nbKey, integerFormat.format(hitsParMinute),
				integerFormat.format(counterRequestAggregation.getRequests().size()));

		final JPanel eastPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
		eastPanel.setOpaque(false);
		eastPanel.add(new JLabel(text));

		if (counter.isBusinessFacadeCounter()) {
			// TODO
			//			writeln("<a href='?part=counterSummaryPerClass&amp;counter=" + counterName
			//					+ "' class='noPrint'>#Resume_par_classe#</a>");
			//			if (PDF_ENABLED) {
			//				writeln(separator);
			//				writeln("<a href='?part=runtimeDependencies&amp;format=pdf&amp;counter="
			//						+ counterName + "' class='noPrint'>#Dependances#</a>");
			//			}
		}

		eastPanel.add(createDetailsButton());

		if (isErrorCounter()) {
			eastPanel.add(createLastErrorsButton());
		}
		if (range.getPeriod() == Period.TOUT) {
			eastPanel.add(createClearCounterButton());
		}

		mainPanel.add(eastPanel, BorderLayout.EAST);
	}

	private MButton createDetailsButton() {
		final MButton detailsButton = new MButton(I18N.getString("Details"), PLUS_ICON);
		detailsButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				showDetailRequests();
				if (detailsButton.getIcon() == PLUS_ICON) {
					detailsButton.setIcon(MINUS_ICON);
				} else {
					detailsButton.setIcon(PLUS_ICON);
				}
			}
		});
		return detailsButton;
	}

	private MButton createLastErrorsButton() {
		final MButton lastErrorsButton = new MButton(I18N.getString("Dernieres_erreurs"), PLUS_ICON);
		lastErrorsButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				showLastErrors();
				if (lastErrorsButton.getIcon() == PLUS_ICON) {
					lastErrorsButton.setIcon(MINUS_ICON);
				} else {
					lastErrorsButton.setIcon(PLUS_ICON);
				}
			}
		});
		return lastErrorsButton;
	}

	private MButton createClearCounterButton() {
		final MButton clearCounterButton = new MButton(I18N.getString("Reinitialiser"));
		clearCounterButton
				.setToolTipText(I18N.getFormattedString("Vider_stats", counter.getName()));
		final Counter myCounter = counter;
		clearCounterButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				if (confirm(I18N.getFormattedString("confirm_vider_stats", myCounter.getName()))) {
					try {
						// TODO refresh
						final String message = getRemoteCollector().executeActionAndCollectData(
								Action.CLEAR_COUNTER, myCounter.getName(), null, null, null);
						showMessage(message);
					} catch (final IOException ex) {
						showException(ex);
					}
				}
			}
		});
		return clearCounterButton;
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
