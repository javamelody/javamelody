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
import java.awt.Component;
import java.awt.Desktop;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.IOException;
import java.io.Serializable;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Map;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTable;
import javax.swing.SwingConstants;
import javax.swing.table.TableColumn;

import net.bull.javamelody.internal.model.MavenArtifact;
import net.bull.javamelody.internal.model.RemoteCollector;
import net.bull.javamelody.swing.MButton;
import net.bull.javamelody.swing.Utilities;
import net.bull.javamelody.swing.table.MDefaultTableCellRenderer;
import net.bull.javamelody.swing.table.MTable;
import net.bull.javamelody.swing.table.MTableScrollPane;

/**
 * Panel de la liste des dépendances.
 * @author Emeric Vernat
 */
class DependenciesPanel extends MelodyPanel {
	private static final long serialVersionUID = 1L;

	@SuppressWarnings("all")
	private Map<String, MavenArtifact> dependencies;
	private MTable<String> table;

	class NameTableCellRenderer extends MDefaultTableCellRenderer {
		private static final long serialVersionUID = 1L;

		@Override
		public Component getTableCellRendererComponent(JTable jtable, Object value,
				boolean isSelected, boolean hasFocus, int row, int column) {
			final MavenArtifact mavenArtifact = getDependencyByViewRow(row);
			Object myValue;
			if (mavenArtifact != null) {
				try {
					myValue = mavenArtifact.getName();
				} catch (final IOException e) {
					myValue = e.toString();
				}
			} else {
				myValue = null;
			}

			return super.getTableCellRendererComponent(jtable, myValue, isSelected, hasFocus, row,
					column);
		}
	}

	class MavenIdTableCellRenderer extends MDefaultTableCellRenderer {
		private static final long serialVersionUID = 1L;

		@Override
		public Component getTableCellRendererComponent(JTable jtable, Object value,
				boolean isSelected, boolean hasFocus, int row, int column) {
			final MavenArtifact mavenArtifact = getDependencyByViewRow(row);
			final Object myValue;
			if (mavenArtifact != null) {
				myValue = mavenArtifact.getGroupId() + ':' + mavenArtifact.getArtifactId() + ':'
						+ mavenArtifact.getVersion();
			} else {
				myValue = null;
			}

			return super.getTableCellRendererComponent(jtable, myValue, isSelected, hasFocus, row,
					column);
		}
	}

	class LicenseTableCellRenderer extends MDefaultTableCellRenderer {
		private static final long serialVersionUID = 1L;

		@Override
		public Component getTableCellRendererComponent(JTable jtable, Object value,
				boolean isSelected, boolean hasFocus, int row, int column) {
			final MavenArtifact mavenArtifact = getDependencyByViewRow(row);
			Object myValue;
			if (mavenArtifact != null) {
				try {
					final StringBuilder sb = new StringBuilder();
					for (final String license : mavenArtifact.getLicenseUrlsByName().keySet()) {
						if (sb.length() > 0) {
							sb.append(", ");
						}
						sb.append(license);
					}
					myValue = sb.toString();
				} catch (final IOException e) {
					myValue = e.toString();
				}
			} else {
				myValue = null;
			}

			return super.getTableCellRendererComponent(jtable, myValue, isSelected, hasFocus, row,
					column);
		}
	}

	DependenciesPanel(RemoteCollector remoteCollector) throws IOException {
		super(remoteCollector);

		refresh();
	}

	MavenArtifact getDependencyByViewRow(int viewRow) {
		final String artifact = getTable().getList()
				.get(getTable().convertRowIndexToModel(viewRow));
		return dependencies.get(artifact);
	}

	final void refresh() throws IOException {
		removeAll();

		this.dependencies = getRemoteCollector().collectWebappDependencies();

		setName(getString("Dependencies"));
		final JLabel titleLabel = Utilities.createParagraphTitle(getName(), "beans.png");
		add(titleLabel, BorderLayout.NORTH);

		final MTableScrollPane<String> scrollPane = createScrollPane();
		this.table = scrollPane.getTable();

		table.setList(new ArrayList<>(dependencies.keySet()));

		add(scrollPane, BorderLayout.CENTER);

		final JLabel summaryLabel = new JLabel(
				getFormattedString("nb_dependencies", dependencies.size()));
		summaryLabel.setHorizontalAlignment(SwingConstants.RIGHT);
		final JPanel southPanel = new JPanel(new BorderLayout());
		southPanel.setOpaque(false);
		southPanel.add(createButtonsPanel(), BorderLayout.NORTH);
		southPanel.add(summaryLabel, BorderLayout.CENTER);
		add(southPanel, BorderLayout.SOUTH);
	}

	private MTableScrollPane<String> createScrollPane() {
		final MTableScrollPane<String> tableScrollPane = new MTableScrollPane<>();
		final MTable<String> myTable = tableScrollPane.getTable();

		final TableColumn artifactTableColumn = new TableColumn(myTable.getColumnCount());
		artifactTableColumn.setIdentifier(myTable.getColumnCount());
		artifactTableColumn.setHeaderValue("Artifact");
		artifactTableColumn.setCellRenderer(new MDefaultTableCellRenderer());
		myTable.addColumn(artifactTableColumn);

		final TableColumn nameTableColumn = new TableColumn(myTable.getColumnCount());
		nameTableColumn.setIdentifier(myTable.getColumnCount());
		nameTableColumn.setHeaderValue(getString("Nom"));
		nameTableColumn.setCellRenderer(new NameTableCellRenderer());
		myTable.addColumn(nameTableColumn);

		final TableColumn mavenIdTableColumn = new TableColumn(myTable.getColumnCount());
		mavenIdTableColumn.setIdentifier(myTable.getColumnCount());
		mavenIdTableColumn.setHeaderValue("Maven Id");
		mavenIdTableColumn.setCellRenderer(new MavenIdTableCellRenderer());
		myTable.addColumn(mavenIdTableColumn);

		final TableColumn licenseTableColumn = new TableColumn(myTable.getColumnCount());
		licenseTableColumn.setIdentifier(myTable.getColumnCount());
		licenseTableColumn.setHeaderValue(getString("Licence"));
		licenseTableColumn.setCellRenderer(new LicenseTableCellRenderer());
		myTable.addColumn(licenseTableColumn);

		myTable.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseClicked(MouseEvent e) {
				if (e.getClickCount() == 2) {
					actionOpenUrl();
				}
			}
		});

		return tableScrollPane;
	}

	private JPanel createButtonsPanel() {
		final MButton refreshButton = createRefreshButton();
		refreshButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				try {
					refresh();
				} catch (final IOException ex) {
					showException(ex);
				}
			}
		});
		final MButton xmlJsonButton = createXmlJsonButton((Serializable) dependencies);
		return Utilities.createButtonsPanel(refreshButton, xmlJsonButton);
	}

	MTable<String> getTable() {
		return table;
	}

	void actionOpenUrl() {
		final String artifact = getTable().getSelectedObject();
		final MavenArtifact dependency = dependencies.get(artifact);
		try {
			if (dependency != null && dependency.getUrl() != null) {
				Desktop.getDesktop().browse(new URI(dependency.getUrl()));
			}
		} catch (final IOException | URISyntaxException ex) {
			showException(ex);
		}
	}
}
