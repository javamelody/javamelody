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
import net.bull.javamelody.internal.model.CacheInformations;
import net.bull.javamelody.internal.model.RemoteCollector;
import net.bull.javamelody.internal.web.html.HtmlCacheInformationsReport;
import net.bull.javamelody.swing.MButton;
import net.bull.javamelody.swing.MHyperLink;
import net.bull.javamelody.swing.Utilities;
import net.bull.javamelody.swing.table.MTable;
import net.bull.javamelody.swing.table.MTableScrollPane;

/**
 * Panel des caches (EHCache).
 * @author Emeric Vernat
 */
class CacheInformationsPanel extends MelodyPanel {
	private static final ImageIcon CLEAR_CACHES_ICON = ImageIconCache
			.getScaledImageIcon("user-trash.png", 18, 18);

	private static final long serialVersionUID = 1L;

	private final boolean hitsRatioEnabled;
	private final boolean configurationEnabled;
	private final MTable<CacheInformations> table;

	CacheInformationsPanel(RemoteCollector remoteCollector,
			List<CacheInformations> cacheInformationsList) {
		super(remoteCollector);
		assert cacheInformationsList != null;
		this.hitsRatioEnabled = HtmlCacheInformationsReport
				.isHitsRatioEnabled(cacheInformationsList);
		this.configurationEnabled = HtmlCacheInformationsReport
				.isConfigurationEnabled(cacheInformationsList);

		final MTableScrollPane<CacheInformations> scrollPane = createScrollPane();
		this.table = scrollPane.getTable();
		table.setList(cacheInformationsList);
		Utilities.adjustTableHeight(table);

		add(scrollPane, BorderLayout.NORTH);

		final MHyperLink hyperLink = new MHyperLink(" Configuration reference",
				"http://ehcache.sourceforge.net/apidocs/net/sf/ehcache/config/CacheConfiguration.html#field_summary");
		add(hyperLink, BorderLayout.WEST);

		final JPanel eastPanel = new JPanel();
		eastPanel.setLayout(new BoxLayout(eastPanel, BoxLayout.Y_AXIS));
		eastPanel.setOpaque(false);
		if (!hitsRatioEnabled) {
			final JLabel statisticsEnabledLabel = new JLabel(
					getString("caches_statistics_enable") + ' ');
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

	private MTableScrollPane<CacheInformations> createScrollPane() {
		final MTableScrollPane<CacheInformations> tableScrollPane = new MTableScrollPane<>();
		final MTable<CacheInformations> myTable = tableScrollPane.getTable();
		myTable.addColumn("name", getString("Cache"));
		if (configurationEnabled) {
			myTable.addColumn("inMemoryPercentUsed", getString("Pourcentage_memoire_utilise"));
		}
		myTable.addColumn("inMemoryObjectCount", getString("Nb_objets_en_memoire"));
		myTable.addColumn("onDiskObjectCount", getString("Nb_objets_sur_disque"));
		if (hitsRatioEnabled) {
			myTable.addColumn("inMemoryHitsRatio",
					"<html>" + getString("Efficacite_cache_memoire").replaceAll("\n", "<br/>"));
			myTable.addColumn("hitsRatio",
					"<html>" + getString("Efficacite_cache").replaceAll("\n", "<br/>"));
			// la hauteur des entêtes de colonnes est calculée selon la hauteur pour la première colonne
			// (see BasicTableHeaderUI.getHeaderHeight()),
			// donc on agrandit la hauteur de la première entête de colonne, pour qu'elle soit adaptée
			// aux deux ci-dessus
			myTable.getColumn("name")
					.setHeaderValue("<html><font size=1><br/></font>"
							+ myTable.getColumn("name").getHeaderValue()
							+ "<font size=1><br/>&nbsp;</font>");
		}
		if (configurationEnabled) {
			myTable.addColumn("configuration", getString("Configuration"));
		}
		return tableScrollPane;
	}

	private JPanel createButtonsPanel() {
		final JPanel buttonsPanel = Utilities.createButtonsPanel();

		if (Parameters.isSystemActionsEnabled()) {
			final MButton clearCacheButton = new MButton(getString("Purger"), CLEAR_CACHES_ICON);
			getTable().getSelectionModel().addListSelectionListener(new ListSelectionListener() {
				@Override
				public void valueChanged(ListSelectionEvent e) {
					final CacheInformations cacheInformations = getTable().getSelectedObject();
					clearCacheButton.setEnabled(cacheInformations != null);
					if (cacheInformations != null) {
						clearCacheButton.setToolTipText(
								getFormattedString("Purge_cache", cacheInformations.getName()));
					} else {
						clearCacheButton.setToolTipText(null);
					}
				}
			});
			clearCacheButton.setEnabled(getTable().getSelectedObject() != null);
			clearCacheButton.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					final CacheInformations cacheInformations = getTable().getSelectedObject();
					if (cacheInformations != null
							&& confirm(getFormattedString("confirm_purge_cache",
									cacheInformations.getName()))) {
						actionClearCache(cacheInformations);
					}
				}
			});
			buttonsPanel.add(clearCacheButton);

			final MButton clearCachesButton = new MButton(getString("Purge_caches"),
					CLEAR_CACHES_ICON);
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

	final void actionClearCache(CacheInformations cacheInformations) {
		try {
			final String message = getRemoteCollector().executeActionAndCollectData(
					Action.CLEAR_CACHE, null, null, null, null, cacheInformations.getName());
			showMessage(message);
			MainPanel.refreshMainTabFromChild(this);
		} catch (final IOException ex) {
			showException(ex);
		}
	}

	final void actionClearCaches() {
		try {
			final String message = getRemoteCollector()
					.executeActionAndCollectData(Action.CLEAR_CACHES, null, null, null, null, null);
			showMessage(message);
			MainPanel.refreshMainTabFromChild(this);
		} catch (final IOException ex) {
			showException(ex);
		}
	}

	MTable<CacheInformations> getTable() {
		return table;
	}
}
