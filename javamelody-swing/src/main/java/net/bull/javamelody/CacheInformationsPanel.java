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
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import java.util.List;

import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingConstants;

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
	private static final ImageIcon CLEAR_CACHES_ICON = ImageIconCache.getScaledImageIcon(
			"user-trash.png", 18, 18);

	private static final long serialVersionUID = 1L;

	@SuppressWarnings("all")
	private final List<CacheInformations> cacheInformationsList;
	private final boolean hitsRatioEnabled;
	private final boolean configurationEnabled;

	CacheInformationsPanel(RemoteCollector remoteCollector,
			List<CacheInformations> cacheInformationsList) {
		super(remoteCollector);
		assert cacheInformationsList != null;
		this.cacheInformationsList = cacheInformationsList;
		this.hitsRatioEnabled = HtmlCacheInformationsReport
				.isHitsRatioEnabled(cacheInformationsList);
		this.configurationEnabled = HtmlCacheInformationsReport
				.isConfigurationEnabled(cacheInformationsList);

		final MTableScrollPane<CacheInformations> scrollPane = createScrollPane();
		final MTable<CacheInformations> table = scrollPane.getTable();
		table.setList(cacheInformationsList);
		Utilities.adjustTableHeight(table);

		add(scrollPane, BorderLayout.NORTH);

		final MHyperLink hyperLink = new MHyperLink(
				" Configuration reference",
				"http://ehcache.sourceforge.net/apidocs/net/sf/ehcache/config/CacheConfiguration.html#field_summary");
		add(hyperLink, BorderLayout.WEST);

		final JPanel eastPanel = new JPanel();
		eastPanel.setLayout(new BoxLayout(eastPanel, BoxLayout.Y_AXIS));
		eastPanel.setOpaque(false);
		if (!hitsRatioEnabled) {
			final JLabel statisticsEnabledLabel = new JLabel(
					I18N.getString("caches_statistics_enable") + ' ');
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
		final MTableScrollPane<CacheInformations> tableScrollPane = new MTableScrollPane<CacheInformations>();
		final MTable<CacheInformations> myTable = tableScrollPane.getTable();
		myTable.addColumn("name", I18N.getString("Cache"));
		if (configurationEnabled) {
			myTable.addColumn("inMemoryPercentUsed", I18N.getString("Pourcentage_memoire_utilise"));
		}
		myTable.addColumn("inMemoryObjectCount", I18N.getString("Nb_objets_en_memoire"));
		myTable.addColumn("onDiskObjectCount", I18N.getString("Nb_objets_sur_disque"));
		if (hitsRatioEnabled) {
			myTable.addColumn("inMemoryHitsRatio",
					"<html>" + I18N.getString("Efficacite_cache_memoire").replaceAll("\n", "<br/>"));
			myTable.addColumn("hitsRatio", "<html>"
					+ I18N.getString("Efficacite_cache").replaceAll("\n", "<br/>"));
			// la hauteur des entêtes de colonnes est calculée selon la hauteur pour la première colonne
			// (see BasicTableHeaderUI.getHeaderHeight()),
			// donc on agrandit la hauteur de la première entête de colonne, pour qu'elle soit adaptée
			// aux deux ci-dessus
			myTable.getColumn("name").setHeaderValue(
					"<html><font size=1><br/></font>" + myTable.getColumn("name").getHeaderValue()
							+ "<font size=1><br/>&nbsp;</font>");
		}
		if (configurationEnabled) {
			myTable.addColumn("configuration", I18N.getString("Configuration"));
		}
		return tableScrollPane;
	}

	private JPanel createButtonsPanel() {
		final MButton purgeCachesButton = new MButton(I18N.getString("Purge_caches"),
				CLEAR_CACHES_ICON);
		purgeCachesButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				if (confirm(I18N.getString("confirm_purge_caches"))) {
					actionClearCaches();
				}
			}
		});
		return Utilities.createButtonsPanel(purgeCachesButton);
	}

	final void actionClearCaches() {
		try {
			final String message = getRemoteCollector().executeActionAndCollectData(
					Action.CLEAR_CACHES, null, null, null, null);
			showMessage(message);
			MainPanel.refreshMainTabFromChild(this);
		} catch (final IOException ex) {
			showException(ex);
		}
	}
}
