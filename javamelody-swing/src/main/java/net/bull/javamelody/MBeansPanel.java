/*
 * Copyright 2008-2012 by Emeric Vernat
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
import java.awt.Component;
import java.awt.Desktop;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.io.Serializable;
import java.util.List;
import java.util.Map;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import net.bull.javamelody.swing.MButton;
import net.bull.javamelody.swing.Utilities;

/**
 * Panel des MBeans.
 * @author Emeric Vernat
 */
class MBeansPanel extends MelodyPanel {
	private static final long serialVersionUID = 1L;

	private static final Font BOLD_FONT = new JLabel().getFont().deriveFont(Font.BOLD);

	@SuppressWarnings("all")
	private Map<String, List<MBeanNode>> mbeansByTitle;

	MBeansPanel(RemoteCollector remoteCollector) throws IOException {
		super(remoteCollector);

		refresh();
	}

	final void refresh() throws IOException {
		removeAll();

		this.mbeansByTitle = getRemoteCollector().collectMBeans();

		setName(I18N.getString("MBeans"));

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

		panel.setOpaque(false);
		for (final Map.Entry<String, List<MBeanNode>> entry : mbeansByTitle.entrySet()) {
			final String title;
			if (mbeansByTitle.size() == 1) {
				title = I18N.getString("MBeans");
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

		return Utilities.createButtonsPanel(refreshButton, pdfButton, xmlJsonButton);
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
