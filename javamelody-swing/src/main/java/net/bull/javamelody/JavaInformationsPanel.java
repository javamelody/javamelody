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
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.net.URI;
import java.net.URL;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.List;

import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SpringLayout;
import javax.swing.SwingConstants;

import net.bull.javamelody.util.MSwingUtilities;
import net.bull.javamelody.util.SpringUtilities;

import com.lowagie.text.Font;

/**
 * Panel des informations systèmes.
 * @author Emeric Vernat
 */
class JavaInformationsPanel extends JPanel {
	static final ImageIcon PLUS_ICON = ImageIconCache.getImageIcon("bullets/plus.png");
	static final ImageIcon MINUS_ICON = ImageIconCache.getImageIcon("bullets/minus.png");

	private static final long serialVersionUID = 1L;

	private final boolean noDatabase = Parameters.isNoDatabase();
	private final DecimalFormat integerFormat = I18N.createIntegerFormat();
	private final DecimalFormat decimalFormat = I18N.createPercentFormat();
	private final JavaInformations javaInformations;
	private final URL monitoringUrl;
	private final JPanel gridPanel;
	private JavaInformationsPanel detailsPanel;

	JavaInformationsPanel(JavaInformations javaInformations, URL monitoringUrl) {
		super(new BorderLayout());
		this.javaInformations = javaInformations;
		this.monitoringUrl = monitoringUrl;
		setOpaque(false);
		gridPanel = new JPanel(new SpringLayout());
		gridPanel.setOpaque(false);
		add(gridPanel, BorderLayout.NORTH);
	}

	void showSummary() {
		addLabel(I18N.getString("Host"));
		final JLabel hostLabel = new JLabel(javaInformations.getHost());
		hostLabel.setFont(hostLabel.getFont().deriveFont(Font.BOLD));
		addJLabel(hostLabel);
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
		addLabel(I18N.getString("nb_threads_actifs") + "\n("
				+ I18N.getString("Requetes_http_en_cours") + ')');
		//		writeGraph("activeThreads", integerFormat.format(javaInformations.getActiveThreadCount()));
		addValue(integerFormat.format(javaInformations.getActiveThreadCount()));
		if (!noDatabase) {
			addLabel(I18N.getString("nb_connexions_actives"));
			// writeGraph("activeConnections", integerFormat.format(javaInformations.getActiveConnectionCount()));
			addValue(integerFormat.format(javaInformations.getActiveConnectionCount()));
			final int usedConnectionCount = javaInformations.getUsedConnectionCount();
			final int maxConnectionCount = javaInformations.getMaxConnectionCount();
			addLabel(I18N.getString("nb_connexions_utilisees") + "\n(" + I18N.getString("ouvertes")
					+ ')');
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
		makeGrid();
	}

	void showDetails(boolean repeatHost) {
		if (detailsPanel != null) {
			detailsPanel.setVisible(!detailsPanel.isVisible());
		} else {
			detailsPanel = new JavaInformationsPanel(javaInformations, monitoringUrl);
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
			addJLabel(hostLabel);
		}
		addLabel(I18N.getString("OS"));
		final String osIconName = HtmlJavaInformationsReport
				.getOSIconName(javaInformations.getOS());
		final JLabel osLabel = new JLabel(javaInformations.getOS() + " ("
				+ javaInformations.getAvailableProcessors() + ' ' + I18N.getString("coeurs") + ')');
		if (osIconName != null) {
			osLabel.setIcon(ImageIconCache.getImageIcon("servers/" + osIconName));
		}
		addJLabel(osLabel);
		addLabel(I18N.getString("Java"));
		addValue(javaInformations.getJavaVersion());
		addLabel(I18N.getString("JVM"));
		final JLabel jvmVersionLabel = new JLabel(javaInformations.getJvmVersion());
		if (javaInformations.getJvmVersion().contains("Client")) {
			jvmVersionLabel.setIcon(ImageIconCache.getImageIcon("alert.png"));
			jvmVersionLabel.setHorizontalTextPosition(SwingConstants.LEFT);
			jvmVersionLabel.setToolTipText(I18N.getString("Client_JVM"));
		}
		addJLabel(jvmVersionLabel);
		addLabel(I18N.getString("PID"));
		addValue(javaInformations.getPID());
		final long unixOpenFileDescriptorCount = javaInformations.getUnixOpenFileDescriptorCount();
		if (unixOpenFileDescriptorCount >= 0) {
			final long unixMaxFileDescriptorCount = javaInformations
					.getUnixMaxFileDescriptorCount();
			addLabel(I18N.getString("nb_fichiers"));
			addValue(integerFormat.format(unixOpenFileDescriptorCount) + " / "
					+ integerFormat.format(unixMaxFileDescriptorCount));
			// writeGraph("fileDescriptors", integerFormat.format(unixOpenFileDescriptorCount));
			// writeln(toBar(javaInformations.getUnixOpenFileDescriptorPercentage()));
		}
		writeServerInfoAndContextPath();
		addLabel(I18N.getString("Demarrage"));
		addValue(I18N.createDateAndTimeFormat().format(javaInformations.getStartDate()));
		addLabel(I18N.getString("Arguments_JVM"));
		addValue(javaInformations.getJvmArguments());

		if (javaInformations.getSessionCount() >= 0) {
			addLabel(I18N.getString("httpSessionsMeanAge"));
			// writeGraph("httpSessionsMeanAge", integerFormat.format(javaInformations.getSessionMeanAgeInMinutes()));
			addValue(integerFormat.format(javaInformations.getSessionMeanAgeInMinutes()));
		}

		writeTomcatInformations(javaInformations.getTomcatInformationsList());

		writeMemoryInformations(javaInformations.getMemoryInformations());

		if (javaInformations.getFreeDiskSpaceInTemp() >= 0) {
			// on considère que l'espace libre sur le disque dur est celui sur la partition du répertoire temporaire
			addLabel(I18N.getString("Free_disk_space"));
			addValue(integerFormat.format(javaInformations.getFreeDiskSpaceInTemp() / 1024 / 1024)
					+ ' ' + I18N.getString("Mo"));
		}

		writeDatabaseVersionAndDataSourceDetails();

		if (javaInformations.isDependenciesEnabled()) {
			addLabel(I18N.getString("Dependencies"));
			writeDependencies();
		}
		makeGrid();
	}

	private void writeServerInfoAndContextPath() {
		final String serverInfo = javaInformations.getServerInfo();
		if (serverInfo != null) {
			addLabel(I18N.getString("Serveur"));
			final String applicationServerIconName = HtmlJavaInformationsReport
					.getApplicationServerIconName(serverInfo);
			final JLabel serverInfoLabel = new JLabel(serverInfo);
			if (applicationServerIconName != null) {
				serverInfoLabel.setIcon(ImageIconCache.getImageIcon("servers/"
						+ applicationServerIconName));
			}
			addJLabel(serverInfoLabel);
			addLabel(I18N.getString("Contexte_webapp"));
			addValue(javaInformations.getContextPath());
		}
	}

	private void writeDatabaseVersionAndDataSourceDetails() {
		if (!noDatabase && javaInformations.getDataBaseVersion() != null) {
			addLabel(I18N.getString("Base_de_donnees"));
			addValue(javaInformations.getDataBaseVersion());
		}
		if (javaInformations.getDataSourceDetails() != null) {
			addLabel(I18N.getString("DataSource_jdbc"));
			addValue(javaInformations.getDataSourceDetails());
			addLabel("");
			final JLabel dataSourceReferenceLabel = new JLabel("DataSource reference");
			dataSourceReferenceLabel.setForeground(Color.BLUE.darker());
			dataSourceReferenceLabel.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
			dataSourceReferenceLabel.addMouseListener(new MouseAdapter() {
				@Override
				public void mouseClicked(MouseEvent e) {
					try {
						Desktop.getDesktop()
								.browse(new URI(
										"http://commons.apache.org/dbcp/apidocs/org/apache/commons/dbcp/BasicDataSource.html"));
					} catch (final Exception ex) {
						MSwingUtilities.showException(ex);
					}
				}
			});
			gridPanel.add(dataSourceReferenceLabel);
		}
	}

	private void writeTomcatInformations(List<TomcatInformations> tomcatInformationsList) {
		final List<TomcatInformations> list = new ArrayList<TomcatInformations>();
		for (final TomcatInformations tomcatInformations : tomcatInformationsList) {
			if (tomcatInformations.getRequestCount() > 0) {
				list.add(tomcatInformations);
			}
		}
		//		final boolean onlyOne = list.size() == 1;
		final String equals = " = ";
		for (final TomcatInformations tomcatInformations : list) {
			addLabel("Tomcat " + I18N.htmlEncode(tomcatInformations.getName(), false));
			// rq: on n'affiche pas pour l'instant getCurrentThreadCount
			final int currentThreadsBusy = tomcatInformations.getCurrentThreadsBusy();
			addValue(I18N.getString("busyThreads") + equals
					+ integerFormat.format(currentThreadsBusy) + " /  "
					+ integerFormat.format(tomcatInformations.getMaxThreads()) + '\n'
					+ I18N.getString("bytesReceived") + equals
					+ integerFormat.format(tomcatInformations.getBytesReceived()) + '\n'
					+ I18N.getString("bytesSent") + equals
					+ integerFormat.format(tomcatInformations.getBytesSent()) + '\n'
					+ I18N.getString("requestCount") + equals
					+ integerFormat.format(tomcatInformations.getRequestCount()) + '\n'
					+ I18N.getString("errorCount") + equals
					+ integerFormat.format(tomcatInformations.getErrorCount()) + '\n'
					+ I18N.getString("processingTime") + equals
					+ integerFormat.format(tomcatInformations.getProcessingTime()) + '\n'
					+ I18N.getString("maxProcessingTime") + equals
					+ integerFormat.format(tomcatInformations.getMaxTime()));
			//			if (onlyOne) {
			//				writeGraph("tomcatBusyThreads", integerFormat.format(currentThreadsBusy));
			//          }
			//			writeln(toBar(100d * currentThreadsBusy / tomcatInformations.getMaxThreads()));
			//			if (onlyOne) {
			//				writeGraph("tomcatBytesReceived",
			//						integerFormat.format(tomcatInformations.getBytesReceived()));
			//			}
			//			if (onlyOne) {
			//				writeGraph("tomcatBytesSent",
			//						integerFormat.format(tomcatInformations.getBytesSent()));
			//			}
		}
	}

	private void writeMemoryInformations(MemoryInformations memoryInformations) {
		addLabel(I18N.getString("Gestion_memoire"));
		addValue(memoryInformations.getMemoryDetails().replace(" Mo", ' ' + I18N.getString("Mo")));

		final long usedPermGen = memoryInformations.getUsedPermGen();
		if (usedPermGen > 0) {
			// perm gen est à 0 sous jrockit
			final long maxPermGen = memoryInformations.getMaxPermGen();
			addLabel(I18N.getString("Memoire_Perm_Gen"));
			final String permGen = integerFormat.format(usedPermGen / 1024 / 1024) + ' '
					+ I18N.getString("Mo");
			if (maxPermGen > 0) {
				addValue(permGen + " / " + integerFormat.format(maxPermGen / 1024 / 1024) + ' '
						+ I18N.getString("Mo"));
				// writeln(toBar(memoryInformations.getUsedPermGenPercentage()));
			} else {
				addValue(permGen);
			}
		}
	}

	private void writeDependencies() {
		final int nbDependencies = javaInformations.getDependenciesList().size();
		final JPanel panel = new JPanel(new BorderLayout());
		panel.setOpaque(false);
		final JLabel nbDependenciesLabel = new JLabel(I18N.getFormattedString("nb_dependencies",
				nbDependencies));
		panel.add(nbDependenciesLabel, BorderLayout.CENTER);
		if (nbDependencies > 0) {
			nbDependenciesLabel.setText(nbDependenciesLabel.getText() + " ; ");
			final MButton detailsButton = new MButton(I18N.getString("Details"), PLUS_ICON);
			panel.add(detailsButton, BorderLayout.EAST);
			final JLabel dependenciesLabel = new JLabel("<html>"
					+ javaInformations.getDependencies().replace("\n", "<br/>"));
			final JPanel dependendiesDetailsPanel = new JPanel(new BorderLayout());
			panel.add(dependendiesDetailsPanel, BorderLayout.SOUTH);
			dependendiesDetailsPanel.setVisible(false);
			dependendiesDetailsPanel.setOpaque(false);
			dependendiesDetailsPanel.add(dependenciesLabel, BorderLayout.NORTH);
			if (javaInformations.doesPomXmlExists() && Parameters.isSystemActionsEnabled()) {
				final MButton pomXmlButton = new MButton(I18N.getString("pom.xml"),
						ImageIconCache.getScaledImageIcon("xml.png", 14, 14));
				dependendiesDetailsPanel.add(pomXmlButton, BorderLayout.WEST);
				dependendiesDetailsPanel.add(new JLabel(""), BorderLayout.CENTER);
				pomXmlButton.addActionListener(new ActionListener() {
					@Override
					public void actionPerformed(ActionEvent e) {
						try {
							Desktop.getDesktop().browse(
									new URI(getMonitoringUrl().toExternalForm() + "?part=pom.xml"));
						} catch (final Exception ex) {
							MSwingUtilities.showException(ex);
						}
					}
				});
			}
			final JPanel localGridPanel = gridPanel;
			detailsButton.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					dependendiesDetailsPanel.setVisible(!dependendiesDetailsPanel.isVisible());
					localGridPanel.validate();
					if (detailsButton.getIcon() == PLUS_ICON) {
						detailsButton.setIcon(MINUS_ICON);
					} else {
						detailsButton.setIcon(PLUS_ICON);
					}
				}
			});
		}
		gridPanel.add(panel);
	}

	private void makeGrid() {
		SpringUtilities.makeCompactGrid(gridPanel, gridPanel.getComponentCount() / 2, 2, 0, 0, 10,
				5);
	}

	private void addLabel(String text) {
		String tmp = text;
		if (tmp.indexOf('\n') != -1) {
			// JLabel accepte la syntaxe html
			tmp = "<html>" + tmp.replace("\n", "<br/>");
		}
		final JLabel label = new JLabel(tmp + ": ");
		label.setVerticalAlignment(SwingConstants.TOP);
		addJLabel(label);
	}

	private void addValue(String value) {
		String tmp = value;
		if (tmp.indexOf('\n') != -1) {
			// JLabel accepte la syntaxe html
			tmp = "<html>" + tmp.replace("\n", "<br/>");
		}
		addJLabel(new JLabel(tmp));
	}

	private void addJLabel(JLabel jLabel) {
		gridPanel.add(jLabel);
	}

	URL getMonitoringUrl() {
		return monitoringUrl;
	}
}
