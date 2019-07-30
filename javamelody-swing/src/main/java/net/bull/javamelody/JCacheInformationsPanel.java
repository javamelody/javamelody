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
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import java.util.List;

import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingConstants;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import net.bull.javamelody.internal.common.Parameters;
import net.bull.javamelody.internal.model.Action;
import net.bull.javamelody.internal.model.JCacheInformations;
import net.bull.javamelody.internal.model.RemoteCollector;
import net.bull.javamelody.internal.web.html.HtmlJCacheInformationsReport;
import net.bull.javamelody.swing.MButton;
import net.bull.javamelody.swing.Utilities;
import net.bull.javamelody.swing.table.MTable;
import net.bull.javamelody.swing.table.MTableScrollPane;

/**
 * Panel des caches (JCache).
 * @author Emeric Vernat
 */
class JCacheInformationsPanel extends MelodyPanel {
	private static final ImageIcon CLEAR_JCACHES_ICON = ImageIconCache
			.getScaledImageIcon("user-trash.png", 18, 18);

	private static final long serialVersionUID = 1L;

	private final boolean hitsRatioEnabled;
	private final MTable<JCacheInformations> table;

	JCacheInformationsPanel(RemoteCollector remoteCollector,
			List<JCacheInformations> jcacheInformationsList) {
		super(remoteCollector);
		assert jcacheInformationsList != null;
		this.hitsRatioEnabled = HtmlJCacheInformationsReport
				.isHitsRatioEnabled(jcacheInformationsList);

		final MTableScrollPane<JCacheInformations> scrollPane = createScrollPane();
		this.table = scrollPane.getTable();
		table.setList(jcacheInformationsList);
		Utilities.adjustTableHeight(table);

		add(scrollPane, BorderLayout.NORTH);

		final JPanel eastPanel = new JPanel();
		eastPanel.setLayout(new BoxLayout(eastPanel, BoxLayout.Y_AXIS));
		eastPanel.setOpaque(false);
		if (!hitsRatioEnabled) {
			final JLabel statisticsEnabledLabel = new JLabel(
					getString("jcaches_statistics_enable") + ' ');
			statisticsEnabledLabel.setAlignmentX(SwingConstants.WEST);
			eastPanel.add(statisticsEnabledLabel);
		}

		if (Parameters.isSystemActionsEnabled()) {
			final JPanel buttonsPanel = createButtonsPanel();
			buttonsPanel.setAlignmentX(SwingConstants.RIGHT);
			eastPanel.add(buttonsPanel);
		}
		add(eastPanel, BorderLayout.EAST);
	}

	private MTableScrollPane<JCacheInformations> createScrollPane() {
		final MTableScrollPane<JCacheInformations> tableScrollPane = new MTableScrollPane<>();
		final MTable<JCacheInformations> myTable = tableScrollPane.getTable();
		myTable.addColumn("name", getString("Cache"));
		if (hitsRatioEnabled) {
			myTable.addColumn("hitsRatio",
					"<html>" + getString("Efficacite_cache").replaceAll("\n", "<br/>"));
			// la hauteur des entêtes de colonnes est calculée selon la hauteur pour la première colonne
			// (see BasicTableHeaderUI.getHeaderHeight()),
			// donc on agrandit la hauteur de la première entête de colonne, pour qu'elle soit adaptée
			// à celle ci-dessus
			myTable.getColumn("name")
					.setHeaderValue("<html><font size=1><br/></font>"
							+ myTable.getColumn("name").getHeaderValue()
							+ "<font size=1><br/>&nbsp;</font>");
		}
		return tableScrollPane;
	}

	private JPanel createButtonsPanel() {
		final JPanel buttonsPanel = Utilities.createButtonsPanel();

		if (Parameters.isSystemActionsEnabled()) {
			final MButton clearCacheButton = new MButton(getString("Purger"), CLEAR_JCACHES_ICON);
			getTable().getSelectionModel().addListSelectionListener(new ListSelectionListener() {
				@Override
				public void valueChanged(ListSelectionEvent e) {
					final JCacheInformations jcacheInformations = getTable().getSelectedObject();
					clearCacheButton.setEnabled(jcacheInformations != null);
					if (jcacheInformations != null) {
						clearCacheButton.setToolTipText(
								getFormattedString("Purge_cache", jcacheInformations.getName()));
					} else {
						clearCacheButton.setToolTipText(null);
					}
				}
			});
			clearCacheButton.setEnabled(getTable().getSelectedObject() != null);
			clearCacheButton.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					final JCacheInformations jcacheInformations = getTable().getSelectedObject();
					if (jcacheInformations != null
							&& confirm(getFormattedString("confirm_purge_cache",
									jcacheInformations.getName()))) {
						actionClearCache(jcacheInformations);
					}
				}
			});
			buttonsPanel.add(clearCacheButton);

			final MButton clearCachesButton = new MButton(getString("Purge_caches"),
					CLEAR_JCACHES_ICON);
			clearCachesButton.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					if (confirm(getString("confirm_purge_caches"))) {
						actionClearCaches();
					}
				}
			});
			buttonsPanel.add(clearCachesButton);
		}

		return buttonsPanel;
	}

	final void actionClearCache(JCacheInformations jcacheInformations) {
		try {
			final String message = getRemoteCollector().executeActionAndCollectData(
					Action.CLEAR_JCACHE, null, null, null, null, jcacheInformations.getName());
			showMessage(message);
			MainPanel.refreshMainTabFromChild(this);
		} catch (final IOException ex) {
			showException(ex);
		}
	}

	final void actionClearCaches() {
		try {
			final String message = getRemoteCollector().executeActionAndCollectData(
					Action.CLEAR_JCACHES, null, null, null, null, null);
			showMessage(message);
			MainPanel.refreshMainTabFromChild(this);
		} catch (final IOException ex) {
			showException(ex);
		}
	}

	MTable<JCacheInformations> getTable() {
		return table;
	}
}
