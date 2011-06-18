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

import java.awt.GridLayout;
import java.text.DecimalFormat;

import javax.swing.JLabel;
import javax.swing.JPanel;

import com.lowagie.text.Font;

/**
 * Panel des informations systèmes.
 * @author Emeric Vernat
 */
class JavaInformationsPanel extends JPanel {
	private static final long serialVersionUID = 1L;

	private final boolean noDatabase = Parameters.isNoDatabase();
	private final DecimalFormat integerFormat = I18N.createIntegerFormat();
	private final DecimalFormat decimalFormat = I18N.createPercentFormat();

	JavaInformationsPanel(JavaInformations javaInformations) {
		super();
		setOpaque(false);
		setLayout(new GridLayout(-1, 2));
		addLabel(I18N.getString("Host"));
		final JLabel hostLabel = new JLabel(javaInformations.getHost());
		hostLabel.setFont(hostLabel.getFont().deriveFont(Font.BOLD));
		add(hostLabel);
		final MemoryInformations memoryInformations = javaInformations.getMemoryInformations();
		final long usedMemory = memoryInformations.getUsedMemory();
		final long maxMemory = memoryInformations.getMaxMemory();
		//		writeGraph("usedMemory", integerFormat.format(usedMemory / 1024 / 1024));
		//		writeln(toBar(memoryInformations.getUsedMemoryPercentage()));
		addLabel(I18N.getString("memoire_utilisee"));
		add(new JLabel(integerFormat.format(usedMemory / 1024 / 1024) + ' ' + I18N.getString("Mo")
				+ " / " + integerFormat.format(maxMemory / 1024 / 1024) + ' '
				+ I18N.getString("Mo")));
		if (javaInformations.getSessionCount() >= 0) {
			addLabel(I18N.getString("nb_sessions_http"));
			// 			writeGraph("httpSessions", integerFormat.format(javaInformations.getSessionCount()));
			add(new JLabel(integerFormat.format(javaInformations.getSessionCount())));
		}
		addLabel("<html>" + I18N.getString("nb_threads_actifs") + "<br>("
				+ I18N.getString("Requetes_http_en_cours") + ')');
		//		writeGraph("activeThreads", integerFormat.format(javaInformations.getActiveThreadCount()));
		add(new JLabel(integerFormat.format(javaInformations.getActiveThreadCount())));
		if (!noDatabase) {
			addLabel(I18N.getString("nb_connexions_actives"));
			// writeGraph("activeConnections", integerFormat.format(javaInformations.getActiveConnectionCount()));
			add(new JLabel(integerFormat.format(javaInformations.getActiveConnectionCount())));
			final int usedConnectionCount = javaInformations.getUsedConnectionCount();
			final int maxConnectionCount = javaInformations.getMaxConnectionCount();
			addLabel("<html>" + I18N.getString("nb_connexions_utilisees") + "<br>("
					+ I18N.getString("ouvertes") + ')');
			//			writeGraph("usedConnections", integerFormat.format(usedConnectionCount));
			if (maxConnectionCount > 0) {
				add(new JLabel(integerFormat.format(usedConnectionCount)));
				//			writeln(toBar(javaInformations.getUsedConnectionPercentage()));
			} else {
				add(new JLabel(integerFormat.format(usedConnectionCount) + " / "
						+ integerFormat.format(maxConnectionCount)));
			}
		}
		if (javaInformations.getSystemLoadAverage() >= 0) {
			addLabel(I18N.getString("Charge_systeme"));
			//			writeGraph("systemLoad", decimalFormat.format(javaInformations.getSystemLoadAverage()));
			add(new JLabel(decimalFormat.format(javaInformations.getSystemLoadAverage())));
		}
	}

	private void addLabel(String text) {
		add(new JLabel(text + ": "));
	}
}
