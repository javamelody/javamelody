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
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.text.DecimalFormat;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;

import net.bull.javamelody.table.MDefaultTableCellRenderer;
import net.bull.javamelody.table.MIntegerTableCellRenderer;
import net.bull.javamelody.table.MTable;
import net.bull.javamelody.table.MTableScrollPane;

/**
 * Panel des statistiques.
 * @author Emeric Vernat
 */
class StatisticsPanel extends JPanel { // NOPMD
	static final ImageIcon PLUS_ICON = ImageIconCache.getImageIcon("bullets/plus.png");
	static final ImageIcon MINUS_ICON = ImageIconCache.getImageIcon("bullets/minus.png");

	private static final long serialVersionUID = 1L;

	final transient CounterRequestAggregation counterRequestAggregation;
	private final Counter counter;
	private final Range range;
	private final MTable<CounterRequest> table;
	private StatisticsPanel detailsPanel;

	StatisticsPanel(Counter counter, Range range) {
		this(counter, range, null);
	}

	private StatisticsPanel(Counter counter, Range range,
			CounterRequestAggregation counterRequestAggregation) {
		super(new BorderLayout());

		setOpaque(false);

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

		//		// 2. débit et liens
		//		writeSizeAndLinks(requests, counterName, globalRequest);
		//
		//		// 3. détails par requêtes (non visible par défaut)
		//		writeln("<div id='details" + counterName + "' style='display: none;'>");
		//		writeRequests(counterName, counter.getChildCounterName(), requests,
		//				isRequestGraphDisplayed(counter), true, false);
		//		writeln("</div>");
		//
		//		// 4. logs (non visible par défaut)
		//		if (isErrorCounter()) {
		//			writeln("<div id='logs" + counterName + "' style='display: none;'>");
		//			new HtmlCounterErrorReport(counter, writer).toHtml();
		//			writeln("</div>");
		//		}

	}

	private void addColumns() {
		final CounterRequest globalRequest = counterRequestAggregation.getGlobalRequest();
		table.addColumn("name", I18N.getString("Requete"));
		final MIntegerTableCellRenderer meanCellRenderer = new MIntegerTableCellRenderer() {
			private static final long serialVersionUID = 1L;

			@Override
			public void setValue(Object value) {
				final Integer mean = (Integer) value;
				if (mean < counterRequestAggregation.getWarningThreshold() || mean == 0) {
					// si cette moyenne est < à la moyenne globale + 1 écart-type (paramétrable), c'est bien
					// (si severeThreshold ou warningThreshold sont à 0 et mean à 0, c'est "info" et non "severe")
					setForeground(Color.GREEN.darker());
					setFont(getFont().deriveFont(Font.PLAIN));
				} else if (mean < counterRequestAggregation.getSevereThreshold()) {
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
		};
		if (counterRequestAggregation.isTimesDisplayed()) {
			table.addColumn("durationsSum", I18N.getString("temps_cumule"));
			table.addColumn("hits", I18N.getString("Hits"));
			table.addColumn("mean", I18N.getString("Temps_moyen"));
			table.addColumn("maximum", I18N.getString("Temps_max"));
			table.addColumn("standardDeviation", I18N.getString("Ecart_type"));
			final MDefaultTableCellRenderer durationPercentageCellRenderer = new MDefaultTableCellRenderer() {
				private static final long serialVersionUID = 1L;

				@Override
				public void setValue(final Object value) {
					if (globalRequest.getDurationsSum() == 0) {
						setText("0");
					} else {
						setText(String
								.valueOf(100 * (Long) value / globalRequest.getDurationsSum()));
					}
				}
			};
			durationPercentageCellRenderer.setHorizontalAlignment(SwingConstants.RIGHT);
			table.setColumnCellRenderer("durationsSum", durationPercentageCellRenderer);
			table.setColumnCellRenderer("mean", meanCellRenderer);
		} else {
			table.addColumn("hits", I18N.getString("Hits"));
		}
		if (counterRequestAggregation.isCpuTimesDisplayed()) {
			table.addColumn("cpuTimeSum", I18N.getString("temps_cpu_cumule"));
			table.addColumn("cpuTimeMean", I18N.getString("Temps_cpu_moyen"));
			final MDefaultTableCellRenderer cpuPercentageCellRenderer = new MDefaultTableCellRenderer() {
				private static final long serialVersionUID = 1L;

				@Override
				public void setValue(final Object value) {
					if (globalRequest.getCpuTimeSum() == 0) {
						setText("0");
					} else {
						setText(String.valueOf(100 * (Long) value / globalRequest.getCpuTimeSum()));
					}
				}
			};
			cpuPercentageCellRenderer.setHorizontalAlignment(SwingConstants.RIGHT);
			table.setColumnCellRenderer("cpuTimeSum", cpuPercentageCellRenderer);
			table.setColumnCellRenderer("cpuTimeMean", meanCellRenderer);
		}
		if (!isErrorAndNotJobCounter()) {
			table.addColumn("systemErrorPercentage", I18N.getString("erreur_systeme"));
		}
		if (counterRequestAggregation.isResponseSizeDisplayed()) {
			table.addColumn("responseSizeMean", I18N.getString("Taille_moyenne"));
			final MIntegerTableCellRenderer responseSizeMeanCellRenderer = new MIntegerTableCellRenderer() {
				private static final long serialVersionUID = 1L;

				@Override
				public void setValue(final Object value) {
					super.setValue((Integer) value / 1024);
				}
			};
			table.setColumnCellRenderer("responseSizeMean", responseSizeMeanCellRenderer);
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

	public void showDetailRequests() {
		if (detailsPanel == null) {
			detailsPanel = new StatisticsPanel(counter, range, counterRequestAggregation);
			detailsPanel.setVisible(false);
			final List<CounterRequest> requests = counterRequestAggregation.getRequests();
			detailsPanel.table.setList(requests);
			detailsPanel.addScrollPane();
			detailsPanel.add(new JLabel(" "), BorderLayout.SOUTH);

			add(detailsPanel, BorderLayout.SOUTH);
		}
		detailsPanel.setVisible(!detailsPanel.isVisible());
		detailsPanel.validate();
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
		add(new JLabel(' ' + I18N.getString(key)), BorderLayout.CENTER);
		add(new JLabel(" "), BorderLayout.SOUTH);
	}

	private void addScrollPane() {
		table.setPreferredScrollableViewportSize(new Dimension(-1, table.getPreferredSize().height));
		final MTableScrollPane<CounterRequest> tableScrollPane = new MTableScrollPane<CounterRequest>(
				table);
		add(tableScrollPane, BorderLayout.NORTH);
		SwingUtilities.invokeLater(new Runnable() {
			@Override
			public void run() {
				tableScrollPane
						.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER);
			}
		});
	}

	private void addRequestsSizeAndButtons() {
		final CounterRequest globalRequest = this.counterRequestAggregation.getGlobalRequest();
		// delta ni négatif ni à 0
		final long deltaMillis = Math.max(System.currentTimeMillis()
				- counter.getStartDate().getTime(), 1);
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
		eastPanel.add(detailsButton);
		add(eastPanel, BorderLayout.EAST);
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
