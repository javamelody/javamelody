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
import java.awt.Desktop;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import java.net.URI;
import java.net.URL;
import java.util.List;

import javax.swing.JPanel;

import net.bull.javamelody.util.MSwingUtilities;

/**
 * Panel des boutons principaux.
 * @author Emeric Vernat
 */
class SystemInformationsButtonsPanel extends JPanel {
	private static final long serialVersionUID = 1L;

	@SuppressWarnings("all")
	private final RemoteCollector remoteCollector;
	@SuppressWarnings("all")
	private final List<JavaInformations> javaInformationsList;
	private final URL monitoringUrl;

	SystemInformationsButtonsPanel(RemoteCollector remoteCollector, URL monitoringUrl) {
		super(new BorderLayout());
		assert remoteCollector != null;
		this.remoteCollector = remoteCollector;
		this.javaInformationsList = remoteCollector.getJavaInformationsList();
		this.monitoringUrl = monitoringUrl;

		setOpaque(false);

		final JPanel northPanel = new JPanel(new FlowLayout(FlowLayout.CENTER, 5, 0));
		northPanel.setOpaque(false);
		final JPanel southPanel = new JPanel(new FlowLayout(FlowLayout.CENTER, 5, 0));
		southPanel.setOpaque(false);

		final MButton gcButton = new MButton(I18N.getString("ramasse_miette"),
				ImageIconCache.getScaledImageIcon("broom.png", 20, 20));
		gcButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				if (confirm(I18N.getString("confirm_ramasse_miette"))) {
					// TODO
				}
			}
		});
		northPanel.add(gcButton);
		final MButton heapDumpButton = new MButton(I18N.getString("heap_dump"),
				ImageIconCache.getScaledImageIcon("heapdump.png", 20, 20));
		heapDumpButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				if (confirm(I18N.getString("confirm_heap_dump"))) {
					// TODO
				}
			}
		});
		northPanel.add(heapDumpButton);
		final MButton heapHistoButton = new MButton(I18N.getString("heaphisto"),
				ImageIconCache.getScaledImageIcon("memory.png", 20, 20));
		heapHistoButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				try {
					addOnglet(new HeapInformationsPanel(getRemoteCollector()));
				} catch (final IOException ex) {
					MSwingUtilities.showException(ex);
				}
			}
		});
		northPanel.add(heapHistoButton);

		if (isSessionsEnabled()) {
			final MButton invalidateSessionsButton = new MButton(
					I18N.getString("invalidate_sessions"), ImageIconCache.getScaledImageIcon(
							"user-trash.png", 20, 20));
			invalidateSessionsButton.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					if (confirm(I18N.getString("confirm_invalidate_sessions"))) {
						// TODO
					}
				}
			});
			northPanel.add(invalidateSessionsButton);
			final MButton sessionsButton = new MButton(I18N.getString("sessions"),
					ImageIconCache.getScaledImageIcon("system-users.png", 20, 20));
			sessionsButton.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					try {
						addOnglet(new SessionInformationsPanel(getRemoteCollector()));
					} catch (final IOException ex) {
						MSwingUtilities.showException(ex);
					}
				}
			});
			northPanel.add(sessionsButton);
		}
		if (doesWebXmlExists()) {
			// on n'affiche le lien web.xml que si le fichier existe (pour api servlet 3.0 par ex)
			final MButton webXmlButton = new MButton(I18N.getString("web.xml"),
					ImageIconCache.getScaledImageIcon("xml.png", 20, 20));
			webXmlButton.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					try {
						Desktop.getDesktop().browse(
								new URI(getMonitoringUrl().toExternalForm() + "?part=web.xml"));
					} catch (final Exception ex) {
						MSwingUtilities.showException(ex);
					}
				}
			});
			southPanel.add(webXmlButton);
		}

		final MButton mbeansButton = new MButton(I18N.getString("MBeans"),
				ImageIconCache.getScaledImageIcon("mbeans.png", 20, 20));
		mbeansButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				// TODO
			}
		});
		southPanel.add(mbeansButton);
		final MButton processesButton = new MButton(I18N.getString("processes"),
				ImageIconCache.getScaledImageIcon("processes.png", 20, 20));
		processesButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				try {
					addOnglet(new ProcessInformationsPanel(getRemoteCollector()));
				} catch (final IOException ex) {
					MSwingUtilities.showException(ex);
				}
			}
		});
		southPanel.add(processesButton);

		final String serverInfo = javaInformationsList.get(0).getServerInfo();
		if (serverInfo != null && !serverInfo.contains("Winstone")) {
			// on n'affiche pas le lien JNDI si serveur Winstone car cela n'a pas d'intérêt
			// pour Hudson/Jenkins sous Winstone, et surtout car (Winstone)Context.listBindings
			// renvoie une liste de NameClassPair au lieu d'une liste de Binding comme il le devrait

			final MButton jndiButton = new MButton(I18N.getString("Arbre_JNDI"),
					ImageIconCache.getScaledImageIcon("jndi.png", 20, 20));
			jndiButton.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					// TODO
				}
			});
			southPanel.add(jndiButton);
		}

		if (isDatabaseEnabled()) {
			final MButton connectionsButton = new MButton(
					I18N.getString("Connexions_jdbc_ouvertes"), ImageIconCache.getScaledImageIcon(
							"db.png", 20, 20));
			connectionsButton.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					try {
						addOnglet(new SessionInformationsPanel(getRemoteCollector()));
					} catch (final IOException ex) {
						MSwingUtilities.showException(ex);
					}
				}
			});
			southPanel.add(connectionsButton);

			final MButton databaseButton = new MButton(I18N.getString("database"),
					ImageIconCache.getScaledImageIcon("db.png", 20, 20));
			databaseButton.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					try {
						addOnglet(new DatabaseInformationsPanel(getRemoteCollector()));
					} catch (final IOException ex) {
						MSwingUtilities.showException(ex);
					}
				}
			});
			southPanel.add(databaseButton);
		}

		add(northPanel, BorderLayout.NORTH);
		add(southPanel, BorderLayout.SOUTH);
	}

	private boolean isDatabaseEnabled() {
		return javaInformationsList.get(0).getDataBaseVersion() != null
				&& !javaInformationsList.get(0).getDataBaseVersion().contains("Exception");
	}

	private boolean doesWebXmlExists() {
		return javaInformationsList.get(0).doesWebXmlExists();
	}

	private boolean isSessionsEnabled() {
		return javaInformationsList.get(0).getSessionCount() >= 0;
	}

	void addOnglet(JPanel panel) {
		MainPanel.addOngletFromChild(this, panel);
	}

	boolean confirm(String message) {
		return MSwingUtilities.showConfirmation(this, message);
	}

	URL getMonitoringUrl() {
		return monitoringUrl;
	}

	RemoteCollector getRemoteCollector() {
		return remoteCollector;
	}
}
