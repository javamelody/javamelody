/*
 * Copyright 2008-2012 by Emeric Vernat
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

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.IOException;
import java.util.List;

import javax.swing.JPanel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import net.bull.javamelody.swing.MButton;
import net.bull.javamelody.swing.Utilities;
import net.bull.javamelody.swing.table.MTable;

/**
 * Panel parent de CounterRequestDetailTablePanel et CounterRequestUsagesPanel.
 * @author Emeric Vernat
 */
abstract class CounterRequestAbstractPanel extends MelodyPanel {
	private static final long serialVersionUID = 1L;

	private final Range range;

	@SuppressWarnings("all")
	private final List<Counter> counters;

	private final MTable<CounterRequest> table;

	CounterRequestAbstractPanel(RemoteCollector remoteCollector, Range range) throws IOException {
		super(remoteCollector);
		this.range = range;
		this.counters = remoteCollector.getCollector().getRangeCountersToBeDisplayed(range);
		this.table = new CounterRequestTable(remoteCollector);
	}

	protected JPanel createButtonsPanel() {
		// TODO traduction
		final MButton openButton = new MButton("Ouvrir",
				ImageIconCache.getImageIcon("action_open.png"));
		openButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				final CounterRequest counterRequest = getTable().getSelectedObject();
				try {
					showRequestDetail(counterRequest);
				} catch (final IOException ex) {
					showException(ex);
				}
			}
		});
		table.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseClicked(MouseEvent e) {
				if (e.getClickCount() == 2) {
					openButton.doClick();
				}
			}
		});
		table.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
			@Override
			public void valueChanged(ListSelectionEvent e) {
				final CounterRequest counterRequest = getTable().getSelectedObject();
				openButton.setEnabled(counterRequest != null);
			}
		});
		openButton.setEnabled(false);

		final MButton usagesButton = new MButton(I18N.getString("Chercher_utilisations"),
				ImageIconCache.getImageIcon("find.png"));
		usagesButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				final CounterRequest counterRequest = getTable().getSelectedObject();
				try {
					showRequestUsages(counterRequest);
				} catch (final IOException ex) {
					showException(ex);
				}
			}
		});
		table.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
			@Override
			public void valueChanged(ListSelectionEvent e) {
				final CounterRequest counterRequest = getTable().getSelectedObject();
				usagesButton.setEnabled(counterRequest != null
						&& doesRequestDisplayUsages(counterRequest));
			}
		});
		usagesButton.setEnabled(false);
		return Utilities.createButtonsPanel(openButton, usagesButton);
	}

	final Counter getCounterByRequestId(CounterRequest counterRequest) {
		return getRemoteCollector().getCollector().getCounterByRequestId(counterRequest);
	}

	final boolean doesRequestDisplayUsages(CounterRequest counterRequest) {
		final Counter parentCounter = getCounterByRequestId(counterRequest);
		return parentCounter != null && !parentCounter.isErrorCounter()
				&& !Counter.HTTP_COUNTER_NAME.equals(parentCounter.getName());
	}

	final MTable<CounterRequest> getTable() {
		return table;
	}

	final List<Counter> getCounters() {
		return counters;
	}

	final void showRequestDetail(CounterRequest counterRequest) throws IOException {
		final CounterRequestDetailPanel panel = new CounterRequestDetailPanel(getRemoteCollector(),
				counterRequest, range);
		MainPanel.addOngletFromChild(this, panel);
	}

	final void showRequestUsages(CounterRequest counterRequest) throws IOException {
		final CounterRequestUsagesPanel panel = new CounterRequestUsagesPanel(getRemoteCollector(),
				counterRequest, range);
		MainPanel.addOngletFromChild(this, panel);
	}
}
