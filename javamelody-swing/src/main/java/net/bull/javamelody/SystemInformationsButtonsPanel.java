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
import java.awt.Desktop;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import java.net.URI;
import java.net.URL;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JPanel;

import net.bull.javamelody.internal.common.Parameters;
import net.bull.javamelody.internal.model.Action;
import net.bull.javamelody.internal.model.JavaInformations;
import net.bull.javamelody.internal.model.RemoteCollector;
import net.bull.javamelody.swing.MButton;

/**
 * Panel des boutons principaux.
 * @author Emeric Vernat
 */
class SystemInformationsButtonsPanel extends MelodyPanel {
	private static final ImageIcon CURRENT_REQUESTS_ICON = ImageIconCache
			.getScaledImageIcon("hourglass.png", 20, 20);
	private static final ImageIcon XML_ICON = ImageIconCache.getScaledImageIcon("xml.png", 20, 20);
	private static final ImageIcon SESSIONS_ICON = ImageIconCache
			.getScaledImageIcon("system-users.png", 20, 20);
	private static final ImageIcon HOTSPOTS_ICON = ImageIconCache.getScaledImageIcon("clock.png",
			20, 20);
	private static final ImageIcon PROCESSES_ICON = ImageIconCache
			.getScaledImageIcon("processes.png", 20, 20);
	private static final ImageIcon MBEANS_ICON = ImageIconCache.getScaledImageIcon("mbeans.png", 20,
			20);
	private static final ImageIcon JNDI_ICON = ImageIconCache.getScaledImageIcon("jndi.png", 20,
			20);
	private static final ImageIcon INVALIDATE_SESSION_ICON = ImageIconCache
			.getScaledImageIcon("user-trash.png", 20, 20);
	private static final ImageIcon HEAP_HISTO_ICON = ImageIconCache.getScaledImageIcon("memory.png",
			20, 20);
	private static final ImageIcon HEAP_DUMP_ICON = ImageIconCache
			.getScaledImageIcon("heapdump.png", 20, 20);
	private static final ImageIcon GC_ICON = ImageIconCache.getScaledImageIcon("broom.png", 20, 20);
	private static final ImageIcon DATABASE_ICON = ImageIconCache.getScaledImageIcon("db.png", 20,
			20);

	private static final long serialVersionUID = 1L;

	@SuppressWarnings("all")
	private final List<JavaInformations> javaInformationsList;
	private final URL monitoringUrl;

	SystemInformationsButtonsPanel(RemoteCollector remoteCollector, URL monitoringUrl,
			boolean collectorServer) {
		super(remoteCollector);
		this.javaInformationsList = getJavaInformationsList();
		this.monitoringUrl = monitoringUrl;

		if (collectorServer) {
			final JPanel currentRequestsPanel = new JPanel(new FlowLayout(FlowLayout.CENTER, 0, 0));
			currentRequestsPanel.setBorder(BorderFactory.createEmptyBorder(0, 0, 15, 0));
			currentRequestsPanel.setOpaque(false);
			currentRequestsPanel.add(createCurrentRequestsButton());
			add(currentRequestsPanel, BorderLayout.NORTH);
		}
		if (Parameters.isSystemActionsEnabled()) {
			final JPanel centerPanel = new JPanel(new FlowLayout(FlowLayout.CENTER, 5, 0));
			centerPanel.setOpaque(false);
			final JPanel southPanel = new JPanel(new FlowLayout(FlowLayout.CENTER, 5, 0));
			southPanel.setOpaque(false);

			centerPanel.add(createGcButton());
			centerPanel.add(createHeapDumpButton());
			centerPanel.add(createHeapHistoButton());

			if (isSessionsEnabled()) {
				centerPanel.add(createInvalidateSessionsButton());
				centerPanel.add(createSessionsButton());
			}

			centerPanel.add(createHotspotsButton());

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

			add(centerPanel, BorderLayout.CENTER);
			add(southPanel, BorderLayout.SOUTH);
		}
	}

	private MButton createCurrentRequestsButton() {
		final MButton currentRequestsButton = new MButton(getString("Requetes_en_cours"),
				CURRENT_REQUESTS_ICON);
		currentRequestsButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				try {
					final CurrentRequestsForCollectorServerPanel panel = new CurrentRequestsForCollectorServerPanel(
							getRemoteCollector());
					addOnglet(panel);
				} catch (final IOException e1) {
					showException(e1);
				}
			}
		});
		return currentRequestsButton;
	}

	private MButton createGcButton() {
		final MButton gcButton = new MButton(getString("ramasse_miette"), GC_ICON);
		gcButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				if (confirm(getString("confirm_ramasse_miette"))) {
					executeAction(Action.GC);
				}
			}
		});
		return gcButton;
	}

	private MButton createHeapDumpButton() {
		final MButton heapDumpButton = new MButton(getString("heap_dump"), HEAP_DUMP_ICON);
		heapDumpButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				if (confirm(getString("confirm_heap_dump"))) {
					executeAction(Action.HEAP_DUMP);
				}
			}
		});
		return heapDumpButton;
	}

	private MButton createHeapHistoButton() {
		final MButton heapHistoButton = new MButton(getString("heaphisto"), HEAP_HISTO_ICON);
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
		final MButton invalidateSessionsButton = new MButton(getString("invalidate_sessions"),
				INVALIDATE_SESSION_ICON);
		invalidateSessionsButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				if (confirm(getString("confirm_invalidate_sessions"))) {
					executeAction(Action.INVALIDATE_SESSIONS);
				}
			}
		});
		return invalidateSessionsButton;
	}

	private MButton createSessionsButton() {
		final MButton sessionsButton = new MButton(getString("sessions"), SESSIONS_ICON);
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

	private MButton createHotspotsButton() {
		final MButton hotspotsButton = new MButton(getString("hotspots"), HOTSPOTS_ICON);
		hotspotsButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				try {
					addOnglet(new HotspotsPanel(getRemoteCollector()));
				} catch (final IOException ex) {
					showException(ex);
				}
			}
		});
		return hotspotsButton;
	}

	private MButton createWebXmlButton() {
		final MButton webXmlButton = new MButton(getString("web.xml"), XML_ICON);
		webXmlButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				try {
					Desktop.getDesktop()
							.browse(new URI(getMonitoringUrl().toExternalForm() + "?part=web.xml"));
				} catch (final Exception ex) {
					showException(ex);
				}
			}
		});
		return webXmlButton;
	}

	private MButton createMBeansButton() {
		final MButton mbeansButton = new MButton(getString("MBeans"), MBEANS_ICON);
		mbeansButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				try {
					addOnglet(new MBeansPanel(getRemoteCollector()));
				} catch (final IOException ex) {
					showException(ex);
				}
			}
		});
		return mbeansButton;
	}

	private MButton createProcessesButton() {
		final MButton processesButton = new MButton(getString("processes"), PROCESSES_ICON);
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
		final MButton jndiButton = new MButton(getString("Arbre_JNDI"), JNDI_ICON);
		jndiButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				try {
					addOnglet(new JndiBindingsPanel(getRemoteCollector()));
				} catch (final IOException ex) {
					showException(ex);
				}
			}
		});
		return jndiButton;
	}

	private MButton createConnectionsButton() {
		final MButton connectionsButton = new MButton(getString("Connexions_jdbc_ouvertes"),
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
		final MButton databaseButton = new MButton(getString("database"), DATABASE_ICON);
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
					null, null, null, null);
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
