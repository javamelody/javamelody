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
import java.awt.Cursor;
import java.awt.Desktop;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.IOException;
import java.net.URI;
import java.util.List;

import javax.swing.JLabel;
import javax.swing.JPanel;

import net.bull.javamelody.swing.MButton;
import net.bull.javamelody.swing.Utilities;
import net.bull.javamelody.swing.table.MTable;
import net.bull.javamelody.swing.table.MTableScrollPane;

/**
 * Panel des caches (EHCache).
 * @author Emeric Vernat
 */
class CacheInformationsPanel extends MelodyPanel {
	private static final long serialVersionUID = 1L;

	@SuppressWarnings("all")
	private final List<CacheInformations> cacheInformationsList;
	private final boolean hitsRatioEnabled;
	private final boolean configurationEnabled;
	private final MTable<CacheInformations> table;

	CacheInformationsPanel(RemoteCollector remoteCollector,
			List<CacheInformations> cacheInformationsList) {
		super(remoteCollector, new BorderLayout());
		assert cacheInformationsList != null;
		this.cacheInformationsList = cacheInformationsList;
		this.hitsRatioEnabled = HtmlCacheInformationsReport
				.isHitsRatioEnabled(cacheInformationsList);
		this.configurationEnabled = HtmlCacheInformationsReport
				.isConfigurationEnabled(cacheInformationsList);
		this.table = new MTable<CacheInformations>();

		addScrollPane();

		final JLabel label = new JLabel(" Configuration reference");
		label.setForeground(Color.BLUE.darker());
		label.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
		label.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseClicked(MouseEvent e) {
				try {
					Desktop.getDesktop()
							.browse(new URI(
									"http://ehcache.sourceforge.net/apidocs/net/sf/ehcache/config/CacheConfiguration.html#field_summary"));
				} catch (final Exception ex) {
					showException(ex);
				}
			}
		});
		add(label, BorderLayout.WEST);

		if (Parameters.isSystemActionsEnabled()) {
			addButton();
		}
	}

	private void addScrollPane() {
		final MTableScrollPane<CacheInformations> tableScrollPane = new MTableScrollPane<CacheInformations>(
				table);
		table.addColumn("name", I18N.getString("Cache"));
		if (configurationEnabled) {
			table.addColumn("inMemoryPercentUsed", I18N.getString("Pourcentage_memoire_utilise"));
		}
		table.addColumn("inMemoryObjectCount", I18N.getString("Nb_objets_en_memoire"));
		table.addColumn("onDiskObjectCount", I18N.getString("Nb_objets_sur_disque"));
		if (hitsRatioEnabled) {
			table.addColumn("inMemoryHitsRatio",
					"<html>" + I18N.getString("Efficacite_cache_memoire").replaceAll("\n", "<br/>"));
			table.addColumn("hitsRatio",
					"<html>" + I18N.getString("Efficacite_cache").replaceAll("\n", "<br/>"));
			// la hauteur des entêtes de colonnes est calculée selon la hauteur pour la première colonne
			// (see BasicTableHeaderUI.getHeaderHeight()),
			// donc on agrandit la hauteur de la première entête de colonne, pour qu'elle soit adaptée
			// aux deux ci-dessus
			table.getColumn("name").setHeaderValue(
					"<html><font size=1><br/></font>" + table.getColumn("name").getHeaderValue()
							+ "<font size=1><br/>&nbsp;</font>");
		}
		if (configurationEnabled) {
			table.addColumn("configuration", I18N.getString("Configuration"));
		}

		add(tableScrollPane, BorderLayout.NORTH);

		table.setList(cacheInformationsList);
		Utilities.adjustTableHeight(table);
	}

	private void addButton() {
		final MButton purgeCachesButton = new MButton(I18N.getString("Purge_caches"),
				ImageIconCache.getScaledImageIcon("user-trash.png", 18, 18));
		purgeCachesButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				if (confirm(I18N.getString("confirm_purge_caches"))) {
					// TODO refresh
					try {
						final String message = getRemoteCollector().executeActionAndCollectData(
								Action.CLEAR_CACHES, null, null, null, null);
						showMessage(message);
					} catch (final IOException ex) {
						showException(ex);
					}
				}
			}
		});
		final JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
		buttonPanel.setOpaque(false);
		buttonPanel.add(purgeCachesButton);
		add(buttonPanel, BorderLayout.EAST);
	}
}
