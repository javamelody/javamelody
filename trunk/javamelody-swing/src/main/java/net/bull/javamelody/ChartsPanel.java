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

import java.awt.Cursor;
import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.io.IOException;
import java.net.URL;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.JPanel;
import javax.swing.SwingConstants;

import net.bull.javamelody.swing.AsyncIconLabel;

/**
 * Panel des graphiques principaux.
 * @author Emeric Vernat
 */
class ChartsPanel extends MelodyPanel {
	private static final long serialVersionUID = 1L;

	private static final Cursor HAND_CURSOR = Cursor.getPredefinedCursor(Cursor.HAND_CURSOR);
	private static final int NB_COLS = 3;

	@SuppressWarnings("all")
	private static final Map<String, List<String>> JROBIN_NAMES_BY_APPLICATION = new HashMap<String, List<String>>();
	@SuppressWarnings("all")
	private static final Map<String, List<String>> OTHER_JROBIN_NAMES_BY_APPLICATION = new HashMap<String, List<String>>();

	private final String urlPart;

	ChartsPanel(RemoteCollector remoteCollector) throws IOException {
		super(remoteCollector, new FlowLayout(FlowLayout.CENTER));

		final URL url = getRemoteCollector().getURLs().get(0);
		final String string = url.toString();
		this.urlPart = string.substring(0, string.indexOf('?')) + "?width=200&height=50&"
				+ HttpParameters.GRAPH_PARAMETER + '=';

		final JPanel centerPanel = new JPanel(new GridLayout(-1, NB_COLS));
		centerPanel.setOpaque(false);
		final List<String> jrobinNames = getJRobinNames();
		for (final String jrobinName : jrobinNames) {
			final URL graphUrl = getGraphUrl(jrobinName);
			final AsyncIconLabel label = new AsyncIconLabel(graphUrl);
			label.setHorizontalAlignment(SwingConstants.CENTER);
			label.setCursor(HAND_CURSOR);
			// TODO MouseListener pour zoom
			centerPanel.add(label);
		}
		add(centerPanel);
	}

	private URL getGraphUrl(String graphName) throws IOException {
		return new URL(urlPart + graphName);
	}

	final List<String> getJRobinNames() throws IOException {
		final RemoteCollector remoteCollector = getRemoteCollector();
		List<String> jrobinNames = JROBIN_NAMES_BY_APPLICATION
				.get(remoteCollector.getApplication());
		if (jrobinNames == null) {
			// on suppose que la liste des graphiques ne changera pas après le lancement du client Swing
			jrobinNames = remoteCollector.collectJRobinNames();
			JROBIN_NAMES_BY_APPLICATION.put(remoteCollector.getApplication(), jrobinNames);
		}
		return jrobinNames;
	}

	final List<String> getOtherJRobinNames() throws IOException {
		final RemoteCollector remoteCollector = getRemoteCollector();
		List<String> otherJRobinNames = OTHER_JROBIN_NAMES_BY_APPLICATION.get(remoteCollector
				.getApplication());
		if (otherJRobinNames == null) {
			// on suppose que la liste des graphiques ne changera pas après le lancement du client Swing
			otherJRobinNames = remoteCollector.collectOtherJRobinNames();
			OTHER_JROBIN_NAMES_BY_APPLICATION.put(remoteCollector.getApplication(),
					otherJRobinNames);
		}
		return otherJRobinNames;
	}
}
