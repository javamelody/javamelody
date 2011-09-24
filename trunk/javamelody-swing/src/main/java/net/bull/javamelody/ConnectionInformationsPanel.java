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

import net.bull.javamelody.swing.Utilities;
import net.bull.javamelody.swing.table.MTable;
import net.bull.javamelody.swing.table.MTableScrollPane;

/**
 * Panel de la liste des connexions jdbc.
 * @author Emeric Vernat
 */
class ConnectionInformationsPanel extends MelodyPanel {
	private static final long serialVersionUID = 1L;

	@SuppressWarnings("all")
	private List<List<ConnectionInformations>> connectionInformationsList;
	private MTable<ConnectionInformations> table;

	ConnectionInformationsPanel(RemoteCollector remoteCollector) throws IOException {
		super(remoteCollector);

		refresh();
	}

	private void refresh() throws IOException {
		removeAll();

		this.connectionInformationsList = getRemoteCollector().collectConnectionInformations();
		this.table = new MTable<ConnectionInformations>();

		setName(I18N.getString("Connexions_jdbc_ouvertes"));
		final JLabel titleLabel = Utilities.createParagraphTitle(
				I18N.getString("Connexions_jdbc_ouvertes"), "db.png");
		add(titleLabel, BorderLayout.NORTH);

		addScrollPane();

		final JLabel label = new JLabel(' ' + I18N.getString("Temps_threads"));
		add(label, BorderLayout.SOUTH);
	}

	private void addScrollPane() {
		final MTableScrollPane<ConnectionInformations> tableScrollPane = new MTableScrollPane<ConnectionInformations>(
				table);
		//		table.addColumn("name", I18N.getString("Thread"));
		// TODO

		add(tableScrollPane, BorderLayout.CENTER);
	}
}
