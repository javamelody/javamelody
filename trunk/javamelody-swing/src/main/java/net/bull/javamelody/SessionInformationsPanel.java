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
import java.io.IOException;
import java.util.List;

import javax.swing.JLabel;
import javax.swing.JPanel;

import net.bull.javamelody.table.MTable;
import net.bull.javamelody.table.MTableScrollPane;

/**
 * Panel de la liste des sessions.
 * @author Emeric Vernat
 */
class SessionInformationsPanel extends JPanel {
	private static final long serialVersionUID = 1L;

	@SuppressWarnings("all")
	private final List<SessionInformations> sessionInformations;
	private final MTable<SessionInformations> table;

	SessionInformationsPanel(RemoteCollector remoteCollector) throws IOException {
		super(new BorderLayout());
		assert remoteCollector != null;
		this.sessionInformations = remoteCollector.collectSessionInformations(null);
		this.table = new MTable<SessionInformations>();

		setOpaque(false);

		addScrollPane();

		final JLabel label = new JLabel(' ' + I18N.getString("Temps_threads"));
		add(label, BorderLayout.WEST);
	}

	private void addScrollPane() {
		final MTableScrollPane<SessionInformations> tableScrollPane = new MTableScrollPane<SessionInformations>(
				table);
		//		table.addColumn("name", I18N.getString("Thread"));
		// TODO

		add(tableScrollPane, BorderLayout.NORTH);
	}
}
