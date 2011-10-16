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
import java.awt.Cursor;
import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.ImageIcon;
import javax.swing.JPanel;
import javax.swing.SwingConstants;

import net.bull.javamelody.swing.AsyncIconLabel;
import net.bull.javamelody.swing.MButton;
import net.bull.javamelody.swing.Utilities;

/**
 * Panel des graphiques principaux.
 * @author Emeric Vernat
 */
class ChartsPanel extends MelodyPanel {
	static final ImageIcon PLUS_ICON = ImageIconCache.getImageIcon("bullets/plus.png");
	static final ImageIcon MINUS_ICON = ImageIconCache.getImageIcon("bullets/minus.png");

	private static final long serialVersionUID = 1L;

	private static final Cursor HAND_CURSOR = Cursor.getPredefinedCursor(Cursor.HAND_CURSOR);
	private static final int NB_COLS = 3;

	@SuppressWarnings("all")
	private static final Map<String, List<String>> JROBIN_NAMES_BY_APPLICATION = new HashMap<String, List<String>>();
	@SuppressWarnings("all")
	private static final Map<String, List<String>> OTHER_JROBIN_NAMES_BY_APPLICATION = new HashMap<String, List<String>>();

	@SuppressWarnings("all")
	private final List<String> jrobinNames;
	@SuppressWarnings("all")
	private final List<String> otherJRobinNames;

	private final String urlPart;
	private JPanel otherJRobinsPanel;

	ChartsPanel(RemoteCollector remoteCollector) throws IOException {
		super(remoteCollector);

		final URL url = getRemoteCollector().getURLs().get(0);
		final String string = url.toString();
		this.urlPart = string.substring(0, string.indexOf('?')) + "?width=200&height=50&"
				+ HttpParameters.GRAPH_PARAMETER + '=';

		this.jrobinNames = getJRobinNames();
		this.otherJRobinNames = getOtherJRobinNames();

		// TODO langue et session pour charger l'image >> LabradorRetriever
		final JPanel mainJRobinsPanel = createJRobinPanel(this.jrobinNames);
		add(mainJRobinsPanel, BorderLayout.NORTH);
		add(createButtonsPanel(), BorderLayout.CENTER);
	}

	private JPanel createJRobinPanel(List<String> myJRobinNames) {
		final JPanel centerPanel = new JPanel(new GridLayout(-1, NB_COLS));
		centerPanel.setOpaque(false);
		for (final String jrobinName : myJRobinNames) {
			final URL graphUrl = getGraphUrl(jrobinName);
			final AsyncIconLabel label = new AsyncIconLabel(graphUrl);
			label.setHorizontalAlignment(SwingConstants.CENTER);
			label.setCursor(HAND_CURSOR);
			// TODO MouseListener pour zoom
			centerPanel.add(label);
		}

		final JPanel graphicsPanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
		graphicsPanel.setOpaque(false);
		graphicsPanel.add(centerPanel);
		return graphicsPanel;
	}

	private URL getGraphUrl(String jrobinName) {
		try {
			return new URL(urlPart + jrobinName);
		} catch (final MalformedURLException e) {
			// ne devrait pas arriver
			throw new IllegalStateException(e);
		}
	}

	private List<String> getJRobinNames() throws IOException {
		final RemoteCollector remoteCollector = getRemoteCollector();
		List<String> result = JROBIN_NAMES_BY_APPLICATION.get(remoteCollector.getApplication());
		if (result == null) {
			// on suppose que la liste des graphiques ne changera pas après le lancement du client Swing
			result = remoteCollector.collectJRobinNames();
			JROBIN_NAMES_BY_APPLICATION.put(remoteCollector.getApplication(), result);
		}
		return result;
	}

	private List<String> getOtherJRobinNames() throws IOException {
		final RemoteCollector remoteCollector = getRemoteCollector();
		List<String> result = OTHER_JROBIN_NAMES_BY_APPLICATION.get(remoteCollector
				.getApplication());
		if (result == null) {
			// on suppose que la liste des graphiques ne changera pas après le lancement du client Swing
			result = remoteCollector.collectOtherJRobinNames();
			OTHER_JROBIN_NAMES_BY_APPLICATION.put(remoteCollector.getApplication(), result);
		}
		return result;
	}

	private JPanel createButtonsPanel() {
		final MButton detailsButton = new MButton(I18N.getString("Autres_courbes"), PLUS_ICON);
		detailsButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				showOtherJRobinsPanel();
				if (detailsButton.getIcon() == PLUS_ICON) {
					detailsButton.setIcon(MINUS_ICON);
				} else {
					detailsButton.setIcon(PLUS_ICON);
				}
			}
		});

		return Utilities.createButtonsPanel(detailsButton);
	}

	final void showOtherJRobinsPanel() {
		if (otherJRobinsPanel == null) {
			otherJRobinsPanel = createJRobinPanel(otherJRobinNames);
			otherJRobinsPanel.setVisible(false);
			add(otherJRobinsPanel, BorderLayout.SOUTH);
		}
		otherJRobinsPanel.setVisible(!otherJRobinsPanel.isVisible());
		otherJRobinsPanel.validate();
	}
}
