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
	private final JavaInformations javaInformations;
	private final JPanel gridPanel;
	private JavaInformationsPanel detailsPanel;

	JavaInformationsPanel(JavaInformations javaInformations) {
		super();
		this.javaInformations = javaInformations;
		setOpaque(false);
		setLayout(new BorderLayout());
		// TODO mettre des hauteurs variables selon les labels
		gridPanel = new JPanel(new GridLayout(-1, 2, 10, 0));
		gridPanel.setOpaque(false);
		add(gridPanel, BorderLayout.NORTH);
	}

	void showSummary() {
		addLabel(I18N.getString("Host"));
		final JLabel hostLabel = new JLabel(javaInformations.getHost());
		hostLabel.setFont(hostLabel.getFont().deriveFont(Font.BOLD));
		gridPanel.add(hostLabel);
		final MemoryInformations memoryInformations = javaInformations.getMemoryInformations();
		final long usedMemory = memoryInformations.getUsedMemory();
		final long maxMemory = memoryInformations.getMaxMemory();
		//		writeGraph("usedMemory", integerFormat.format(usedMemory / 1024 / 1024));
		//		writeln(toBar(memoryInformations.getUsedMemoryPercentage()));
		addLabel(I18N.getString("memoire_utilisee"));
		addValue(integerFormat.format(usedMemory / 1024 / 1024) + ' ' + I18N.getString("Mo")
				+ " / " + integerFormat.format(maxMemory / 1024 / 1024) + ' '
				+ I18N.getString("Mo"));
		if (javaInformations.getSessionCount() >= 0) {
			addLabel(I18N.getString("nb_sessions_http"));
			// 			writeGraph("httpSessions", integerFormat.format(javaInformations.getSessionCount()));
			addValue(integerFormat.format(javaInformations.getSessionCount()));
		}
		addLabel("<html>" + I18N.getString("nb_threads_actifs") + "<br>("
				+ I18N.getString("Requetes_http_en_cours") + ')');
		//		writeGraph("activeThreads", integerFormat.format(javaInformations.getActiveThreadCount()));
		addValue(integerFormat.format(javaInformations.getActiveThreadCount()));
		if (!noDatabase) {
			addLabel(I18N.getString("nb_connexions_actives"));
			// writeGraph("activeConnections", integerFormat.format(javaInformations.getActiveConnectionCount()));
			addValue(integerFormat.format(javaInformations.getActiveConnectionCount()));
			final int usedConnectionCount = javaInformations.getUsedConnectionCount();
			final int maxConnectionCount = javaInformations.getMaxConnectionCount();
			addLabel("<html>" + I18N.getString("nb_connexions_utilisees") + "<br>("
					+ I18N.getString("ouvertes") + ')');
			//			writeGraph("usedConnections", integerFormat.format(usedConnectionCount));
			if (maxConnectionCount > 0) {
				addValue(integerFormat.format(usedConnectionCount));
				//			writeln(toBar(javaInformations.getUsedConnectionPercentage()));
			} else {
				addValue(integerFormat.format(usedConnectionCount) + " / "
						+ integerFormat.format(maxConnectionCount));
			}
		}
		if (javaInformations.getSystemLoadAverage() >= 0) {
			addLabel(I18N.getString("Charge_systeme"));
			//			writeGraph("systemLoad", decimalFormat.format(javaInformations.getSystemLoadAverage()));
			addValue(decimalFormat.format(javaInformations.getSystemLoadAverage()));
		}
	}

	void showDetails(boolean repeatHost) {
		if (detailsPanel != null) {
			detailsPanel.setVisible(!detailsPanel.isVisible());
		} else {
			detailsPanel = new JavaInformationsPanel(javaInformations);
			detailsPanel.addDetails(repeatHost);
			add(detailsPanel, BorderLayout.SOUTH);
			// sans cela, le panel n'apparaît pas la première fois
			detailsPanel.setVisible(false);
			detailsPanel.setVisible(true);
		}
	}

	private void addDetails(boolean repeatHost) {
		if (repeatHost) {
			addLabel(I18N.getString("Host"));
			final JLabel hostLabel = new JLabel(javaInformations.getHost());
			hostLabel.setFont(hostLabel.getFont().deriveFont(Font.BOLD));
			gridPanel.add(hostLabel);
		}
		addLabel(I18N.getString("OS"));
		final String osIconName = HtmlJavaInformationsReport
				.getOSIconName(javaInformations.getOS());
		final JLabel osLabel = new JLabel(javaInformations.getOS() + " ("
				+ javaInformations.getAvailableProcessors() + ' ' + I18N.getString("coeurs") + ')');
		if (osIconName != null) {
			osLabel.setIcon(ImageIconCache.getImageIcon("servers/" + osIconName));
		}
		gridPanel.add(osLabel);
		addLabel(I18N.getString("Java"));
		addValue(javaInformations.getJavaVersion());
		addLabel(I18N.getString("JVM"));
		addLabel(javaInformations.getJvmVersion());
		// TODO
		//		if (javaInformations.getJvmVersion().contains("Client")) {
		//			write("&nbsp;&nbsp;&nbsp;<img src='?resource=alert.png' alt=\"#Client_JVM#\" title=\"#Client_JVM#\"/>");
		//		}
		addLabel(I18N.getString("PID"));
		addValue(javaInformations.getPID());
		//		final long unixOpenFileDescriptorCount = javaInformations.getUnixOpenFileDescriptorCount();
		//		if (unixOpenFileDescriptorCount >= 0) {
		//			final long unixMaxFileDescriptorCount = javaInformations
		//					.getUnixMaxFileDescriptorCount();
		//			write("<tr><td>#nb_fichiers#</td><td>");
		//			writeGraph("fileDescriptors", integerFormat.format(unixOpenFileDescriptorCount));
		//			writeln(" / " + integerFormat.format(unixMaxFileDescriptorCount) + "&nbsp;&nbsp;&nbsp;");
		//			writeln(toBar(javaInformations.getUnixOpenFileDescriptorPercentage()));
		//			writeln(columnEnd);
		//		}
		//		writeServerInfoAndContextPath(javaInformations);
		addLabel(I18N.getString("Demarrage"));
		addValue(I18N.createDateAndTimeFormat().format(javaInformations.getStartDate()));
		// TODO
		//		addLabel(I18N.getString("Arguments_JVM"));
		//		addValue("<html>" + javaInformations.getJvmArguments().replace("\n", "<br/>"));

		if (javaInformations.getSessionCount() >= 0) {
			addLabel(I18N.getString("httpSessionsMeanAge"));
			// writeGraph("httpSessionsMeanAge", integerFormat.format(javaInformations.getSessionMeanAgeInMinutes()));
			addValue(integerFormat.format(javaInformations.getSessionMeanAgeInMinutes()));
		}
		//
		//		writeTomcatInformations(javaInformations.getTomcatInformationsList());
		//
		//		writeMemoryInformations(javaInformations.getMemoryInformations());

		if (javaInformations.getFreeDiskSpaceInTemp() >= 0) {
			// on considère que l'espace libre sur le disque dur est celui sur la partition du répertoire temporaire
			addLabel(I18N.getString("Free_disk_space"));
			addValue(integerFormat.format(javaInformations.getFreeDiskSpaceInTemp() / 1024 / 1024)
					+ ' ' + I18N.getString("Mo"));
		}

		//		writeDatabaseVersionAndDataSourceDetails(javaInformations);
		//
		//		if (javaInformations.isDependenciesEnabled()) {
		//			writeln("<tr><td valign='top'>#Dependencies#: </td><td>");
		//			writeDependencies(javaInformations);
		//			writeln(columnEnd);
		//		}
	}

	private void addLabel(String text) {
		gridPanel.add(new JLabel(text + ": "));
	}

	private void addValue(String value) {
		gridPanel.add(new JLabel(value));
	}
}
