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
import java.awt.Color;
import java.awt.Cursor;
import java.awt.Rectangle;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import javax.swing.border.Border;

import net.bull.javamelody.internal.common.I18N;
import net.bull.javamelody.internal.model.MBeanNode;
import net.bull.javamelody.internal.model.MBeanNode.MBeanAttribute;
import net.bull.javamelody.swing.Utilities;
import net.bull.javamelody.swing.table.MDefaultTableCellRenderer;
import net.bull.javamelody.swing.table.MMultiLineTableCellRenderer;
import net.bull.javamelody.swing.table.MTable;

/**
 * Panel des MBeanNode.
 * @author Emeric Vernat
 */
class MBeanNodePanel extends JPanel {
	static final Border LEFT_MARGIN_BORDER = BorderFactory.createEmptyBorder(0, 30, 0, 0);

	private static final long serialVersionUID = 1L;

	private static final Color FOREGROUND = Color.BLUE.darker();

	private static final Cursor HAND_CURSOR = Cursor.getPredefinedCursor(Cursor.HAND_CURSOR);

	private static final ImageIcon PLUS_ICON = ImageIconCache.getImageIcon("bullets/plus.png");

	private static final ImageIcon MINUS_ICON = ImageIconCache.getImageIcon("bullets/minus.png");

	private static final MMultiLineTableCellRenderer FORMATTED_VALUE_CELL_RENDERER = new MMultiLineTableCellRenderer();

	private static final MDefaultTableCellRenderer DESCRIPTION_CELL_RENDERER = new MDefaultTableCellRenderer() {
		private static final long serialVersionUID = 1L;

		@Override
		protected void setValue(Object value) {
			if (value != null) {
				super.setValue('(' + value.toString() + ')');
			} else {
				super.setValue(null);
			}
		}
	};

	private static final MouseListener LABEL_MOUSE_LISTENER = new MouseAdapter() {
		@Override
		public void mouseClicked(MouseEvent event) {
			final MBeanNodePanel nodePanel = (MBeanNodePanel) event.getComponent().getParent();
			nodePanel.onClick();
		}
	};

	private static final MouseListener TABLE_MOUSE_LISTENER = new MouseAdapter() {
		@SuppressWarnings("unchecked")
		@Override
		public void mouseClicked(MouseEvent e) {
			if (e.getClickCount() == 2) {
				final MTable<MBeanAttribute> table = (MTable<MBeanAttribute>) e.getComponent();
				final MBeanAttribute attribute = table.getSelectedObject();
				Utilities.showTextInPopup(table, attribute.getName(),
						attribute.getFormattedValue());
			}
		}
	};

	private final MBeanNode node;

	private JLabel label;

	private JPanel detailPanel;

	MBeanNodePanel(MBeanNode node) {
		super(new BorderLayout());
		assert node != null;
		this.node = node;
		init();
	}

	private void init() {
		setOpaque(false);
		String name = node.getName();
		final int indexOfComma = name.indexOf(',');
		if (node.getChildren() != null || indexOfComma != -1) {
			if (indexOfComma != -1) {
				name = name.substring(indexOfComma + 1);
			}
			label = new JLabel(name);
			if (node.getDescription() != null) {
				label.setToolTipText("<html>" + name + "<br/>(" + node.getDescription() + ')');
			}
			label.setIcon(PLUS_ICON);
			label.setForeground(FOREGROUND);
			label.setCursor(HAND_CURSOR);
			label.addMouseListener(LABEL_MOUSE_LISTENER);
			add(label, BorderLayout.CENTER);
		} else {
			detailPanel = createAttributesPanel();
			add(detailPanel, BorderLayout.CENTER);
		}
	}

	void onClick() {
		if (detailPanel == null) {
			final List<MBeanNode> children = node.getChildren();
			if (children != null) {
				detailPanel = createNodeTreePanel(children);
			} else {
				detailPanel = createAttributesPanel();
			}
			detailPanel.setBorder(LEFT_MARGIN_BORDER);
			detailPanel.setVisible(false);
			add(detailPanel, BorderLayout.SOUTH);
		}
		detailPanel.setVisible(!detailPanel.isVisible());
		if (detailPanel.isVisible()) {
			SwingUtilities.invokeLater(new Runnable() {
				@Override
				public void run() {
					scrollToVisible();
				}
			});
			label.setIcon(MINUS_ICON);
		} else {
			label.setIcon(PLUS_ICON);
		}
		validate();
	}

	void expand() {
		if (detailPanel == null || !detailPanel.isVisible()) {
			onClick();
		}
	}

	void collapse() {
		if (label != null && detailPanel != null && detailPanel.isVisible()) {
			onClick();
		}
	}

	private JPanel createAttributesPanel() {
		final List<MBeanAttribute> attributes = node.getAttributes();
		boolean descriptionDisplayed = false;
		for (final MBeanAttribute attribute : attributes) {
			if (attribute.getDescription() != null) {
				descriptionDisplayed = true;
				break;
			}
		}
		final JPanel attributesPanel = new JPanel(new BorderLayout());
		attributesPanel.setOpaque(false);
		if (node.getDescription() != null) {
			final JLabel descriptionLabel = new JLabel('(' + node.getDescription() + ')');
			attributesPanel.add(descriptionLabel, BorderLayout.NORTH);
		}

		final MTable<MBeanAttribute> table = new MTable<>();
		table.addColumn("name", I18N.getString("Nom"));
		table.addColumn("formattedValue", I18N.getString("Contenu"));
		table.setColumnCellRenderer("formattedValue", FORMATTED_VALUE_CELL_RENDERER);
		if (descriptionDisplayed) {
			table.addColumn("description", "");
			table.setColumnCellRenderer("description", DESCRIPTION_CELL_RENDERER);
		}
		table.setList(attributes);
		table.addMouseListener(TABLE_MOUSE_LISTENER);
		attributesPanel.add(table, BorderLayout.CENTER);
		return attributesPanel;
	}

	void scrollToVisible() {
		final Rectangle localBounds = SwingUtilities.getLocalBounds(MBeanNodePanel.this);
		localBounds.grow(0, 15);
		scrollRectToVisible(localBounds);
	}

	static JPanel createNodeTreePanel(List<MBeanNode> nodes) {
		final JPanel nodeTreePanel = new JPanel();
		nodeTreePanel.setOpaque(false);
		nodeTreePanel.setLayout(new BoxLayout(nodeTreePanel, BoxLayout.Y_AXIS));

		for (final MBeanNode node : nodes) {
			nodeTreePanel.add(new MBeanNodePanel(node));
		}
		return nodeTreePanel;
	}
}
