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
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.List;

import javax.swing.JButton;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;

/**
 * Panel des boutons principaux.
 * @author Emeric Vernat
 */
class SystemInformationsButtonsPanel extends JPanel {
	private static final long serialVersionUID = 1L;

	@SuppressWarnings("all")
	private final List<JavaInformations> javaInformationsList;

	SystemInformationsButtonsPanel(List<JavaInformations> javaInformationsList) {
		super();
		this.javaInformationsList = javaInformationsList;

		setOpaque(false);
		setLayout(new BorderLayout());

		final JPanel northPanel = new JPanel(new FlowLayout(FlowLayout.CENTER, 5, 0));
		northPanel.setOpaque(false);
		final JPanel southPanel = new JPanel(new FlowLayout(FlowLayout.CENTER, 5, 0));
		southPanel.setOpaque(false);

		final JButton gcButton = new JButton(I18N.getString("ramasse_miette"),
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
		final JButton heapDumpButton = new JButton(I18N.getString("heap_dump"),
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
		final JButton heapHistoButton = new JButton(I18N.getString("heaphisto"),
				ImageIconCache.getScaledImageIcon("memory.png", 20, 20));
		northPanel.add(heapHistoButton);

		if (isSessionsEnabled()) {
			final JButton invalidateSessionsButton = new JButton(
					I18N.getString("invalidate_sessions"), ImageIconCache.getScaledImageIcon(
							"user-trash.png", 18, 18));
			invalidateSessionsButton.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					if (confirm(I18N.getString("confirm_invalidate_sessions"))) {
						// TODO
					}
				}
			});
			northPanel.add(invalidateSessionsButton);
			final JButton sessionsButton = new JButton(I18N.getString("sessions"),
					ImageIconCache.getScaledImageIcon("system-users.png", 20, 20));
			northPanel.add(sessionsButton);
		}
		if (doesWebXmlExists()) {
			// on n'affiche le lien web.xml que si le fichier existe (pour api servlet 3.0 par ex)
			final JButton webXmlButton = new JButton(I18N.getString("web.xml"),
					ImageIconCache.getScaledImageIcon("xml.png", 20, 20));
			southPanel.add(webXmlButton);
		}

		final JButton mbeansButton = new JButton(I18N.getString("MBeans"),
				ImageIconCache.getScaledImageIcon("mbeans.png", 20, 20));
		southPanel.add(mbeansButton);
		final JButton processesButton = new JButton(I18N.getString("processes"),
				ImageIconCache.getScaledImageIcon("processes.png", 20, 20));
		southPanel.add(processesButton);

		final String serverInfo = javaInformationsList.get(0).getServerInfo();
		if (serverInfo != null && !serverInfo.contains("Winstone")) {
			// on n'affiche pas le lien JNDI si serveur Winstone car cela n'a pas d'intérêt
			// pour Hudson/Jenkins sous Winstone, et surtout car (Winstone)Context.listBindings
			// renvoie une liste de NameClassPair au lieu d'une liste de Binding comme il le devrait

			final JButton jndiButton = new JButton(I18N.getString("Arbre_JNDI"),
					ImageIconCache.getScaledImageIcon("jndi.png", 20, 20));
			southPanel.add(jndiButton);
		}

		if (isDatabaseEnabled()) {
			final JButton connectionsButton = new JButton(
					I18N.getString("Connexions_jdbc_ouvertes"), ImageIconCache.getScaledImageIcon(
							"db.png", 20, 20));
			southPanel.add(connectionsButton);

			final JButton databaseButton = new JButton(I18N.getString("database"),
					ImageIconCache.getScaledImageIcon("db.png", 20, 20));
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

	boolean confirm(String message) {
		return JOptionPane.showConfirmDialog(SwingUtilities.getWindowAncestor(this), message,
				UIManager.getString("OptionPane.titleText"), JOptionPane.YES_OPTION
						| JOptionPane.CANCEL_OPTION) == JOptionPane.OK_OPTION;
	}
}
