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
import java.awt.Color;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.border.Border;

/**
 * Panel des MBeanNode.
 * @author Emeric Vernat
 */
class MBeanNodePanel extends JPanel {
	static final Border LEFT_MARGIN_BORDER = BorderFactory.createEmptyBorder(0, 40, 0, 0);

	private static final long serialVersionUID = 1L;

	private static final Color FOREGROUND = Color.BLUE.darker();

	private static final ImageIcon PLUS_ICON = ImageIconCache.getImageIcon("bullets/plus.png");

	private static final ImageIcon MINUS_ICON = ImageIconCache.getImageIcon("bullets/minus.png");

	private static final MouseListener MOUSE_LISTENER = new MouseAdapter() {
		@Override
		public void mouseClicked(MouseEvent event) {
			final MBeanNodePanel nodePanel = (MBeanNodePanel) event.getComponent().getParent();
			nodePanel.onClick();
		}
	};

	private final MBeanNode node;

	private JLabel label;

	private JPanel childrenPanel;

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
			label.setIcon(PLUS_ICON);
			label.setForeground(FOREGROUND);
			label.addMouseListener(MOUSE_LISTENER);
			add(label, BorderLayout.CENTER);
		} else {
			// TODO afficher attributs
		}
	}

	void onClick() {
		final List<MBeanNode> children = node.getChildren();
		if (children != null) {
			if (childrenPanel == null) {
				childrenPanel = createNodeTreePanel(children);
				childrenPanel.setBorder(LEFT_MARGIN_BORDER);
				childrenPanel.setVisible(false);
				add(childrenPanel, BorderLayout.SOUTH);
			}

			childrenPanel.setVisible(!childrenPanel.isVisible());
		} else {
			// TODO affichier attributs
		}
		if (label.getIcon() == PLUS_ICON) {
			label.setIcon(MINUS_ICON);
		} else {
			label.setIcon(PLUS_ICON);
		}
		validate();
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
