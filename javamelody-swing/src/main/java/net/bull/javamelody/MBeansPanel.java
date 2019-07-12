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
import java.awt.Container;
import java.awt.Desktop;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.io.Serializable;
import java.util.List;
import java.util.Map;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;

import net.bull.javamelody.internal.model.MBeanNode;
import net.bull.javamelody.internal.model.RemoteCollector;
import net.bull.javamelody.internal.web.pdf.PdfOtherReport;
import net.bull.javamelody.swing.MButton;
import net.bull.javamelody.swing.Utilities;

/**
 * Panel des MBeans.
 * @author Emeric Vernat
 */
class MBeansPanel extends MelodyPanel {
	private static final long serialVersionUID = 1L;

	private static final Font BOLD_FONT = new JLabel().getFont().deriveFont(Font.BOLD);

	private static final ImageIcon EXPAND_ALL_ICON = new ImageIcon(
			MainButtonsPanel.class.getResource("/icons/expand-all.png"));

	private static final ImageIcon COLLAPSE_ALL_ICON = new ImageIcon(
			MainButtonsPanel.class.getResource("/icons/collapse-all.png"));

	@SuppressWarnings("all")
	private Map<String, List<MBeanNode>> mbeansByTitle;

	MBeansPanel(RemoteCollector remoteCollector) throws IOException {
		super(remoteCollector);

		refresh();
	}

	final void refresh() throws IOException {
		removeAll();

		this.mbeansByTitle = getRemoteCollector().collectMBeans();

		setName(getString("MBeans"));

		add(createScrollPane(), BorderLayout.CENTER);

		add(createButtonsPanel(), BorderLayout.SOUTH);
	}

	private JScrollPane createScrollPane() {
		final JScrollPane scrollPane = new JScrollPane();
		final JPanel panel = new JPanel();
		panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
		panel.setBorder(BorderFactory.createEmptyBorder(0, 10, 10, 10));
		scrollPane.setViewportView(panel);
		scrollPane.getVerticalScrollBar().setUnitIncrement(20);
		// cette récupération du focus dans le panel du scrollPane permet d'utiliser les flèches hauts et bas
		// pour scroller dès l'affichage du panel
		SwingUtilities.invokeLater(new Runnable() {
			@Override
			public void run() {
				panel.requestFocus();
			}
		});

		panel.setOpaque(false);
		for (final Map.Entry<String, List<MBeanNode>> entry : mbeansByTitle.entrySet()) {
			final String title;
			if (mbeansByTitle.size() == 1) {
				title = getString("MBeans");
			} else {
				title = entry.getKey();
			}
			final List<MBeanNode> mbeans = entry.getValue();
			final JLabel titleLabel = Utilities.createParagraphTitle(title, "mbeans.png");
			titleLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
			panel.add(titleLabel);

			final JPanel treePanel = createDomainTreePanel(mbeans);
			treePanel.setAlignmentX(Component.LEFT_ALIGNMENT);
			treePanel.setBorder(MBeanNodePanel.LEFT_MARGIN_BORDER);
			panel.add(treePanel);
		}
		return scrollPane;
	}

	private JPanel createDomainTreePanel(List<MBeanNode> mbeans) {
		final JPanel treePanel = new JPanel();
		treePanel.setOpaque(false);
		treePanel.setLayout(new BoxLayout(treePanel, BoxLayout.Y_AXIS));

		final MBeanNode platformNode = mbeans.get(0);
		treePanel.add(MBeanNodePanel.createNodeTreePanel(platformNode.getChildren()));

		for (final MBeanNode node : mbeans) {
			if (node != platformNode) {
				treePanel.add(new JLabel(""));
				final JLabel nodeLabel = new JLabel(node.getName());
				nodeLabel.setFont(BOLD_FONT);
				treePanel.add(nodeLabel);
				treePanel.add(MBeanNodePanel.createNodeTreePanel(node.getChildren()));
			}
		}
		return treePanel;
	}

	private JPanel createButtonsPanel() {
		final MButton expandAllButton = new MButton("", EXPAND_ALL_ICON);
		expandAllButton.setToolTipText(getString("Tout_montrer"));
		expandAllButton.setPreferredSize(new Dimension(expandAllButton.getPreferredSize().width - 4,
				expandAllButton.getPreferredSize().height + 3));
		expandAllButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				actionExpandAll();
			}
		});

		final MButton collapseAllButton = new MButton("", COLLAPSE_ALL_ICON);
		collapseAllButton.setToolTipText(getString("Tout_reduire"));
		collapseAllButton
				.setPreferredSize(new Dimension(collapseAllButton.getPreferredSize().width - 4,
						collapseAllButton.getPreferredSize().height + 3));
		collapseAllButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				actionCollapseAll();
			}
		});

		final MButton pdfButton = createPdfButton();
		pdfButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				try {
					actionPdf();
				} catch (final IOException ex) {
					showException(ex);
				}
			}
		});
		final Serializable serializable = (Serializable) mbeansByTitle;
		final MButton xmlJsonButton = createXmlJsonButton(serializable);

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

		return Utilities.createButtonsPanel(expandAllButton, collapseAllButton, refreshButton,
				pdfButton, xmlJsonButton);
	}

	final void actionExpandAll() {
		expandAll(this);
	}

	final void actionCollapseAll() {
		collapseAll(this);
	}

	private void expandAll(Container container) {
		for (final Component component : container.getComponents()) {
			if (component instanceof MBeanNodePanel) {
				((MBeanNodePanel) component).expand();
			}
			if (component instanceof Container) {
				expandAll((Container) component);
			}
			if (component instanceof JScrollPane) {
				SwingUtilities.invokeLater(new Runnable() {
					@Override
					public void run() {
						// pour annuler le scrollToVisible des MBeanNodePanel,
						// on repositionne le scrollPane tout en haut
						((JScrollPane) component).getViewport().setViewPosition(new Point(0, 0));
					}
				});
			}
		}
	}

	private void collapseAll(Container container) {
		for (final Component component : container.getComponents()) {
			if (component instanceof MBeanNodePanel) {
				((MBeanNodePanel) component).collapse();
			}
			if (component instanceof Container) {
				collapseAll((Container) component);
			}
		}
	}

	final void actionPdf() throws IOException {
		final File tempFile = createTempFileForPdf();
		final PdfOtherReport pdfOtherReport = createPdfOtherReport(tempFile);
		try {
			pdfOtherReport.writeMBeans(mbeansByTitle);
		} finally {
			pdfOtherReport.close();
		}
		Desktop.getDesktop().open(tempFile);
	}
}
