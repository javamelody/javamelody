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

import java.awt.Component;
import java.awt.Graphics;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JPanel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import net.bull.javamelody.internal.model.Counter;
import net.bull.javamelody.internal.model.CounterRequest;
import net.bull.javamelody.internal.model.RemoteCollector;
import net.bull.javamelody.swing.MButton;
import net.bull.javamelody.swing.Utilities;
import net.bull.javamelody.swing.table.MTable;

/**
 * Panel parent de CounterRequestDetailTablePanel et CounterRequestUsagesPanel.
 * @author Emeric Vernat
 */
abstract class CounterRequestAbstractPanel extends MelodyPanel {
	private static final long serialVersionUID = 1L;

	private static final Map<String, ImageIcon> ICON_BY_NAME = new HashMap<>();

	@SuppressWarnings("all")
	private final List<Counter> counters;

	private final MTable<CounterRequest> table;

	CounterRequestAbstractPanel(RemoteCollector remoteCollector) {
		this(remoteCollector, new CounterRequestTable(remoteCollector));
	}

	CounterRequestAbstractPanel(RemoteCollector remoteCollector, CounterRequestTable table) {
		super(remoteCollector);
		// comme dans ScrollingPanel, on ne peut utiliser collector.getRangeCountersToBeDisplayed(range),
		// en revanche collector.getCounters() contient ici les bonnes données
		this.counters = getCollector().getCounters();
		this.table = table;
	}

	protected JPanel createButtonsPanel(boolean includeUsagesButton) {
		final MButton openButton = new MButton(getString("Ouvrir"),
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

		if (includeUsagesButton) {
			final MButton usagesButton = new MButton(getString("Chercher_utilisations"),
					ImageIconCache.getImageIcon("find.png"));
			usagesButton.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					final CounterRequest counterRequest = getTable().getSelectedObject();
					showRequestUsages(counterRequest);
				}
			});
			table.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
				@Override
				public void valueChanged(ListSelectionEvent e) {
					final CounterRequest counterRequest = getTable().getSelectedObject();
					usagesButton.setEnabled(
							counterRequest != null && doesRequestDisplayUsages(counterRequest));
				}
			});
			usagesButton.setEnabled(false);
			return Utilities.createButtonsPanel(openButton, usagesButton);
		}
		return Utilities.createButtonsPanel(openButton);
	}

	final Counter getCounterByRequestId(CounterRequest counterRequest) {
		if (counterRequest == null) {
			return null;
		}
		return getCollector().getCounterByRequestId(counterRequest);
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
				counterRequest);
		MainPanel.addOngletFromChild(this, panel);
	}

	final void showRequestUsages(CounterRequest counterRequest) {
		final CounterRequestUsagesPanel panel = new CounterRequestUsagesPanel(getRemoteCollector(),
				counterRequest);
		MainPanel.addOngletFromChild(this, panel);
	}

	static Icon getCounterIcon(Counter counter, final int margin) {
		if (counter == null || counter.getIconName() == null) {
			return null;
		}

		final String iconName = counter.getIconName();
		ImageIcon iconWithoutMargin = ICON_BY_NAME.get(iconName);
		if (iconWithoutMargin == null) {
			iconWithoutMargin = ImageIconCache.getScaledImageIcon(iconName, 16, 16);
			ICON_BY_NAME.put(iconName, iconWithoutMargin);
		}
		final ImageIcon counterIcon = iconWithoutMargin;
		final Icon icon;
		if (margin == 0) {
			icon = counterIcon;
		} else {
			// ajoute une marge à gauche de l'icône
			icon = new Icon() {
				@Override
				public void paintIcon(Component c, Graphics g, int x, int y) {
					g.drawImage(counterIcon.getImage(), margin + x, y, null);
				}

				@Override
				public int getIconWidth() {
					return counterIcon.getIconWidth() + margin;
				}

				@Override
				public int getIconHeight() {
					return counterIcon.getIconHeight();
				}
			};
		}
		return icon;
	}
}
