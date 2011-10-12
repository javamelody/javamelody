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

import javax.swing.ImageIcon;
import javax.swing.JPanel;

import net.bull.javamelody.swing.MButton;

/**
 * Panel des boutons principaux.
 * @author Emeric Vernat
 */
class SystemInformationsButtonsPanel extends MelodyPanel {
	private static final ImageIcon XML_ICON = ImageIconCache.getScaledImageIcon("xml.png", 20, 20);
	private static final ImageIcon SESSIONS_ICON = ImageIconCache.getScaledImageIcon(
			"system-users.png", 20, 20);
	private static final ImageIcon PROCESSES_ICON = ImageIconCache.getScaledImageIcon(
			"processes.png", 20, 20);
	private static final ImageIcon MBEANS_ICON = ImageIconCache.getScaledImageIcon("mbeans.png",
			20, 20);
	private static final ImageIcon JNDI_ICON = ImageIconCache
			.getScaledImageIcon("jndi.png", 20, 20);
	private static final ImageIcon INVALIDATE_SESSION_ICON = ImageIconCache.getScaledImageIcon(
			"user-trash.png", 20, 20);
	private static final ImageIcon HEAP_HISTO_ICON = ImageIconCache.getScaledImageIcon(
			"memory.png", 20, 20);
	private static final ImageIcon HEAP_DUMP_ICON = ImageIconCache.getScaledImageIcon(
			"heapdump.png", 20, 20);
	private static final ImageIcon GC_ICON = ImageIconCache.getScaledImageIcon("broom.png", 20, 20);
	private static final ImageIcon DATABASE_ICON = ImageIconCache.getScaledImageIcon("db.png", 20,
			20);

	private static final long serialVersionUID = 1L;

	@SuppressWarnings("all")
	private final List<JavaInformations> javaInformationsList;
	private final URL monitoringUrl;

	SystemInformationsButtonsPanel(RemoteCollector remoteCollector, URL monitoringUrl) {
		super(remoteCollector);
		this.javaInformationsList = remoteCollector.getJavaInformationsList();
		this.monitoringUrl = monitoringUrl;

		final JPanel northPanel = new JPanel(new FlowLayout(FlowLayout.CENTER, 5, 0));
		northPanel.setOpaque(false);
		final JPanel southPanel = new JPanel(new FlowLayout(FlowLayout.CENTER, 5, 0));
		southPanel.setOpaque(false);

		northPanel.add(createGcButton());
		northPanel.add(createHeapDumpButton());
		northPanel.add(createHeapHistoButton());

		if (isSessionsEnabled()) {
			northPanel.add(createInvalidateSessionsButton());
			northPanel.add(createSessionsButton());
		}
		if (doesWebXmlExists()) {
			// on n'affiche le lien web.xml que si le fichier existe (pour api servlet 3.0 par ex)
			southPanel.add(createWebXmlButton());
		}

		southPanel.add(createMBeansButton());
		southPanel.add(createProcessesButton());

		final String serverInfo = javaInformationsList.get(0).getServerInfo();
		if (serverInfo != null && !serverInfo.contains("Winstone")) {
			// on n'affiche pas le lien JNDI si serveur Winstone car cela n'a pas d'intérêt
			// pour Hudson/Jenkins sous Winstone, et surtout car (Winstone)Context.listBindings
			// renvoie une liste de NameClassPair au lieu d'une liste de Binding comme il le devrait

			southPanel.add(createJndiButton());
		}

		if (isDatabaseEnabled()) {
			southPanel.add(createConnectionsButton());

			southPanel.add(createDatabaseButton());
		}

		add(northPanel, BorderLayout.NORTH);
		add(southPanel, BorderLayout.SOUTH);
	}

	private MButton createGcButton() {
		final MButton gcButton = new MButton(I18N.getString("ramasse_miette"), GC_ICON);
		gcButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				if (confirm(I18N.getString("confirm_ramasse_miette"))) {
					executeAction(Action.GC);
				}
			}
		});
		return gcButton;
	}

	private MButton createHeapDumpButton() {
		final MButton heapDumpButton = new MButton(I18N.getString("heap_dump"), HEAP_DUMP_ICON);
		heapDumpButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				if (confirm(I18N.getString("confirm_heap_dump"))) {
					executeAction(Action.HEAP_DUMP);
				}
			}
		});
		return heapDumpButton;
	}

	private MButton createHeapHistoButton() {
		final MButton heapHistoButton = new MButton(I18N.getString("heaphisto"), HEAP_HISTO_ICON);
		heapHistoButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				try {
					addOnglet(new HeapInformationsPanel(getRemoteCollector()));
				} catch (final IOException ex) {
					showException(ex);
				}
			}
		});
		return heapHistoButton;
	}

	private MButton createInvalidateSessionsButton() {
		final MButton invalidateSessionsButton = new MButton(I18N.getString("invalidate_sessions"),
				INVALIDATE_SESSION_ICON);
		invalidateSessionsButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				if (confirm(I18N.getString("confirm_invalidate_sessions"))) {
					executeAction(Action.INVALIDATE_SESSIONS);
				}
			}
		});
		return invalidateSessionsButton;
	}

	private MButton createSessionsButton() {
		final MButton sessionsButton = new MButton(I18N.getString("sessions"), SESSIONS_ICON);
		sessionsButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				try {
					addOnglet(new SessionInformationsPanel(getRemoteCollector()));
				} catch (final IOException ex) {
					showException(ex);
				}
			}
		});
		return sessionsButton;
	}

	private MButton createWebXmlButton() {
		final MButton webXmlButton = new MButton(I18N.getString("web.xml"), XML_ICON);
		webXmlButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				try {
					Desktop.getDesktop().browse(
							new URI(getMonitoringUrl().toExternalForm() + "?part=web.xml"));
				} catch (final Exception ex) {
					showException(ex);
				}
			}
		});
		return webXmlButton;
	}

	private MButton createMBeansButton() {
		final MButton mbeansButton = new MButton(I18N.getString("MBeans"), MBEANS_ICON);
		mbeansButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				// TODO
			}
		});
		return mbeansButton;
	}

	private MButton createProcessesButton() {
		final MButton processesButton = new MButton(I18N.getString("processes"), PROCESSES_ICON);
		processesButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				try {
					addOnglet(new ProcessInformationsPanel(getRemoteCollector()));
				} catch (final IOException ex) {
					showException(ex);
				}
			}
		});
		return processesButton;
	}

	private MButton createJndiButton() {
		final MButton jndiButton = new MButton(I18N.getString("Arbre_JNDI"), JNDI_ICON);
		jndiButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				// TODO
			}
		});
		return jndiButton;
	}

	private MButton createConnectionsButton() {
		final MButton connectionsButton = new MButton(I18N.getString("Connexions_jdbc_ouvertes"),
				DATABASE_ICON);
		connectionsButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				try {
					addOnglet(new ConnectionInformationsPanel(getRemoteCollector()));
				} catch (final IOException ex) {
					showException(ex);
				}
			}
		});
		return connectionsButton;
	}

	private MButton createDatabaseButton() {
		final MButton databaseButton = new MButton(I18N.getString("database"), DATABASE_ICON);
		databaseButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				try {
					addOnglet(new DatabaseInformationsPanel(getRemoteCollector()));
				} catch (final IOException ex) {
					showException(ex);
				}
			}
		});
		return databaseButton;
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

	final void executeAction(Action action) {
		try {
			final String message = getRemoteCollector().executeActionAndCollectData(action, null,
					null, null, null);
			showMessage(message);
			MainPanel.refreshMainTabFromChild(this);
		} catch (final IOException ex) {
			showException(ex);
		}
	}

	final void addOnglet(JPanel panel) {
		MainPanel.addOngletFromChild(this, panel);
	}

	URL getMonitoringUrl() {
		return monitoringUrl;
	}
}
