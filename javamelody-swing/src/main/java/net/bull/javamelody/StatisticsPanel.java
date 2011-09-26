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
import java.awt.Desktop;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.text.DecimalFormat;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import net.bull.javamelody.swing.MButton;
import net.bull.javamelody.swing.Utilities;
import net.bull.javamelody.swing.table.MTable;

/**
 * Panel des statistiques.
 * @author Emeric Vernat
 */
class StatisticsPanel extends MelodyPanel {
	static final ImageIcon PLUS_ICON = ImageIconCache.getImageIcon("bullets/plus.png");
	static final ImageIcon MINUS_ICON = ImageIconCache.getImageIcon("bullets/minus.png");

	private static final long serialVersionUID = 1L;

	@SuppressWarnings("all")
	private final CounterRequestAggregation counterRequestAggregation;
	private final Counter counter;
	private final Range range;
	private final StatisticsTablePanel tablePanel;
	private final JPanel mainPanel;
	private StatisticsPanel detailsPanel;
	private JPanel lastErrorsPanel;

	StatisticsPanel(RemoteCollector remoteCollector, Counter counter, Range range) {
		this(remoteCollector, counter, range, null);
	}

	private StatisticsPanel(RemoteCollector remoteCollector, Counter counter, Range range,
			CounterRequestAggregation counterRequestAggregation) {
		super(remoteCollector);

		assert counter != null;
		assert range != null;
		this.counter = counter;
		this.range = range;

		if (counter.getRequestsCount() == 0) {
			this.tablePanel = null;
			this.counterRequestAggregation = null;
		} else {
			if (counterRequestAggregation == null) {
				this.counterRequestAggregation = new CounterRequestAggregation(counter);
			} else {
				this.counterRequestAggregation = counterRequestAggregation;
			}
			this.tablePanel = new StatisticsTablePanel(getRemoteCollector(), counter,
					this.counterRequestAggregation);
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

	public void showGlobalRequests() {
		if (counter.getRequestsCount() == 0) {
			final JLabel noRequestsLabel = createNoRequestsLabel();
			mainPanel.add(noRequestsLabel, BorderLayout.CENTER);
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

			showRequests(requests);

			final JPanel requestsSizeAndButtonsPanel = createRequestsSizeAndButtonsPanel();
			mainPanel.add(requestsSizeAndButtonsPanel, BorderLayout.EAST);
		}
	}

	void showDetailRequests() {
		if (detailsPanel == null) {
			detailsPanel = new StatisticsPanel(getRemoteCollector(), counter, range,
					counterRequestAggregation);
			detailsPanel.setVisible(false);
			final List<CounterRequest> requests = counterRequestAggregation.getRequests();
			detailsPanel.showRequests(requests);

			add(detailsPanel, BorderLayout.CENTER);
		}
		detailsPanel.setVisible(!detailsPanel.isVisible());
		detailsPanel.validate();
	}

	void showRequestsAggregatedOrFilteredByClassName(String requestId, final MButton detailsButton) {
		final List<CounterRequest> requests = new CounterRequestAggregation(counter)
				.getRequestsAggregatedOrFilteredByClassName(requestId);
		tablePanel.setList(requests);
		add(tablePanel, BorderLayout.CENTER);

		if (detailsButton != null) {
			final MTable<CounterRequest> myTable = tablePanel.getTable();
			myTable.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
				@Override
				public void valueChanged(ListSelectionEvent e) {
					detailsButton.setEnabled(myTable.getSelectedObject() != null);
				}
			});
			detailsButton.setEnabled(myTable.getSelectedObject() != null);
			myTable.addMouseListener(new MouseAdapter() {
				@Override
				public void mouseClicked(MouseEvent e) {
					if (e.getClickCount() == 2) {
						detailsButton.doClick();
					}
				}
			});
			detailsButton.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					actionCounterSummaryPerClass(true);
				}
			});
		}
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

	private JLabel createNoRequestsLabel() {
		final String key;
		if (isJobCounter()) {
			key = "Aucun_job";
		} else if (isErrorCounter()) {
			key = "Aucune_erreur";
		} else {
			key = "Aucune_requete";
		}
		return new JLabel(' ' + I18N.getString(key));
	}

	private void showRequests(List<CounterRequest> requests) {
		tablePanel.setList(requests);
		Utilities.adjustTableHeight(tablePanel.getTable());
		mainPanel.add(tablePanel, BorderLayout.NORTH);
	}

	private JPanel createRequestsSizeAndButtonsPanel() {
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

		final JPanel panel = Utilities.createButtonsPanel(new JLabel(text));

		if (counter.isBusinessFacadeCounter()) {
			panel.add(createCounterSummaryPerClassButton());
			panel.add(createRuntimeDependenciesButton());
		}

		panel.add(createDetailsButton());

		if (isErrorCounter()) {
			panel.add(createLastErrorsButton());
		}
		if (range.getPeriod() == Period.TOUT) {
			panel.add(createClearCounterButton());
		}

		return panel;
	}

	private MButton createCounterSummaryPerClassButton() {
		final MButton counterSummaryPerClassButton = new MButton(
				I18N.getString("Resume_par_classe"));
		counterSummaryPerClassButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				actionCounterSummaryPerClass(false);
			}
		});
		return counterSummaryPerClassButton;
	}

	private MButton createRuntimeDependenciesButton() {
		final MButton runtimeDependenciesButton = new MButton(I18N.getString("Dependances"),
				ImageIconCache.getImageIcon("pdf.png"));
		runtimeDependenciesButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				try {
					actionRuntimeDependencies();
				} catch (final IOException ex) {
					showException(ex);
				}
			}
		});
		return runtimeDependenciesButton;
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
					actionClearCounter(myCounter);
				}
			}
		});
		return clearCounterButton;
	}

	final void actionClearCounter(final Counter myCounter) {
		try {
			final String message = getRemoteCollector().executeActionAndCollectData(
					Action.CLEAR_COUNTER, myCounter.getName(), null, null, null);
			showMessage(message);
			// TODO à tester
			MainPanel.refreshMainTabFromChild(this);
		} catch (final IOException ex) {
			showException(ex);
		}
	}

	final void actionCounterSummaryPerClass(boolean detailsForAClass) {
		final String requestId;
		if (detailsForAClass) {
			requestId = tablePanel.getTable().getSelectedObject().getId();
		} else {
			requestId = null;
		}
		final CounterSummaryPerClassPanel panel = new CounterSummaryPerClassPanel(
				getRemoteCollector(), counter, range, requestId);
		MainPanel.addOngletFromChild(this, panel);
	}

	final void actionRuntimeDependencies() throws IOException {
		final File tempFile = createTempFileForPdf();
		final OutputStream output = createFileOutputStream(tempFile);
		try {
			final PdfOtherReport pdfOtherReport = new PdfOtherReport(getRemoteCollector()
					.getApplication(), output);
			pdfOtherReport.writeRuntimeDependencies(counter, range);
		} finally {
			output.close();
		}
		Desktop.getDesktop().open(tempFile);
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
