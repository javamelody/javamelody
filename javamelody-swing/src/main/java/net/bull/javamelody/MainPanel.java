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
import java.awt.Point;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingUtilities;

import net.bull.javamelody.internal.common.HttpParameter;
import net.bull.javamelody.internal.model.CounterStorage;
import net.bull.javamelody.internal.model.Range;
import net.bull.javamelody.internal.model.RemoteCollector;
import net.bull.javamelody.swing.util.MSwingUtilities;

/**
 * Panel principal.
 * @author Emeric Vernat
 */
class MainPanel extends MelodyPanel {
	private static final Color BACKGROUND = Color.decode("#E6E6E6");
	private static final long serialVersionUID = 1L;

	private final TabbedPane tabbedPane = new TabbedPane();
	private final URL monitoringUrl;
	@SuppressWarnings("all")
	private final List<URL> initialURLs;
	private final JScrollPane scrollPane;
	private Range selectedRange;
	private final boolean collectorServer;

	MainPanel(RemoteCollector remoteCollector, Range selectedRange, boolean collectorServer)
			throws IOException {
		super(remoteCollector);
		this.collectorServer = collectorServer;
		// initialURLs avant setSelectedRange
		this.initialURLs = remoteCollector.getURLs();
		final String collectorUrl = initialURLs.get(0).toExternalForm();
		this.monitoringUrl = new URL(collectorUrl.substring(0, collectorUrl.indexOf('?')));

		setSelectedRange(selectedRange);

		CounterStorage.disableStorage();
		remoteCollector.disableAggregation();
		remoteCollector.collectDataIncludingCurrentRequests();

		scrollPane = new JScrollPane();
		scrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
		scrollPane.getVerticalScrollBar().setUnitIncrement(20);

		final JPanel mainTabPanel = new JPanel(new BorderLayout());
		mainTabPanel.setOpaque(false);
		final MainButtonsPanel mainButtonsPanel = new MainButtonsPanel(remoteCollector,
				getSelectedRange(), monitoringUrl, collectorServer);
		mainTabPanel.add(mainButtonsPanel, BorderLayout.NORTH);
		mainTabPanel.add(scrollPane, BorderLayout.CENTER);

		tabbedPane.addTab(getString("Tableau_de_bord"), mainTabPanel);
		tabbedPane.setTabComponentAt(0, null);
		add(tabbedPane, BorderLayout.CENTER);

		refreshMainTab();
	}

	private void refreshMainTab() {
		final int position = scrollPane.getVerticalScrollBar().getValue();
		final ScrollingPanel scrollingPanel = new ScrollingPanel(getRemoteCollector(),
				getSelectedRange(), monitoringUrl, collectorServer);
		final TabbedPane finalTabbedPane = tabbedPane;
		final JScrollPane finalScrollPane = scrollPane;
		// le clique droit sur le panel principal fonctionne comme sur les autres panels du TabbedPane
		// (malgré le scrollPane du panel principal)
		scrollingPanel.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseClicked(MouseEvent event) {
				// we only look at the right button
				if (SwingUtilities.isRightMouseButton(event)) {
					final Point viewPosition = finalScrollPane.getViewport().getViewPosition();
					final int x = event.getX() - viewPosition.x + finalScrollPane.getX()
							+ finalScrollPane.getParent().getX() + 3;
					final int y = event.getY() - viewPosition.y + finalScrollPane.getY()
							+ finalScrollPane.getParent().getY() + 3;
					final MouseEvent newEvent = new MouseEvent(finalTabbedPane, event.getID(),
							event.getWhen(), event.getModifiersEx(), x, y, event.getXOnScreen(),
							event.getYOnScreen(), event.getClickCount(), event.isPopupTrigger(),
							event.getButton());
					finalTabbedPane.processMouseEvent(newEvent);
				}
			}
		});
		scrollPane.setViewportView(scrollingPanel);
		scrollPane.getVerticalScrollBar().setValue(position);
		// cette récupération du focus dans le panel du scrollPane permet d'utiliser les flèches hauts et bas
		// pour scroller dès l'affichage du panel
		SwingUtilities.invokeLater(new Runnable() {
			@Override
			public void run() {
				scrollingPanel.requestFocus();
			}
		});
	}

	private void addOnglet(JPanel panel) {
		tabbedPane.addTab(panel.getName(), panel);
		tabbedPane.setSelectedIndex(tabbedPane.getTabCount() - 1);
	}

	final Range getSelectedRange() {
		return selectedRange;
	}

	final void setSelectedRange(Range selectedRange) throws IOException {
		this.selectedRange = selectedRange;
		final List<URL> newUrls = new ArrayList<>(initialURLs.size());
		for (final URL url : initialURLs) {
			final URL newUrl = new URL(
					url.toString() + '&' + HttpParameter.PERIOD + '=' + selectedRange.getValue());
			newUrls.add(newUrl);
		}
		getRemoteCollector().setURLs(newUrls);
	}

	void changeRange(Range newRange) {
		final Range currentRange = getSelectedRange();
		try {
			setSelectedRange(newRange);

			getRemoteCollector().collectDataIncludingCurrentRequests();
			refreshMainTab();
		} catch (final IOException e) {
			showException(e);
			// si le changement de période n'a pas abouti, alors on remet l'ancienne période
			try {
				setSelectedRange(currentRange);
			} catch (final IOException e2) {
				showException(e2);
			}
		}
	}

	static MainPanel getParentMainPanelFromChild(Component child) {
		return MSwingUtilities.getAncestorOfClass(MainPanel.class, child);
	}

	static void refreshMainTabFromChild(Component child) {
		final MainPanel mainPanel = getParentMainPanelFromChild(child);
		mainPanel.refreshMainTab();
	}

	static void addOngletFromChild(Component child, JPanel panel) {
		panel.setOpaque(true);
		panel.setBackground(BACKGROUND);
		panel.setBorder(BorderFactory.createEmptyBorder(0, 10, 10, 10));
		final MainPanel mainPanel = getParentMainPanelFromChild(child);
		mainPanel.addOnglet(panel);
	}
}
