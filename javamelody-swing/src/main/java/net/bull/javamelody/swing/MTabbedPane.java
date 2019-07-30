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
package net.bull.javamelody.swing;

import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;

import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JTabbedPane;
import javax.swing.SwingUtilities;

/**
 * TabbedPane.<br>
 * On ne laisse qu'un constructeur qui positionne TAB_PLACEMENT à TOP et TAB_LAYOUT_POLICY à
 * SCROLL_TAB_LAYOUT.
 *
 * @author Emeric Vernat
 */
public class MTabbedPane extends JTabbedPane {
	private static final long serialVersionUID = 1L;
	@SuppressWarnings("all")
	private static final MouseWheelScroller MOUSE_WHEEL_SCROLLER = new MouseWheelScroller();
	@SuppressWarnings("all")
	private static final TabSelectionMouseHandler TAB_SELECTION_MOUSE_HANDLER = new TabSelectionMouseHandler();

	private static class MouseWheelScroller implements MouseWheelListener {
		@Override
		public void mouseWheelMoved(MouseWheelEvent event) {
			final MTabbedPane tabbedPane = (MTabbedPane) event.getSource();
			tabbedPane.mouseWheelMoved(event);
		}
	}

	private static class TabSelectionMouseHandler extends MouseAdapter {
		@Override
		public void mouseClicked(MouseEvent event) {
			final MTabbedPane tabbedPane = (MTabbedPane) event.getSource();
			tabbedPane.mouseClicked(event);
		}
	}

	private class MenuSelectionHandler implements ActionListener {
		MenuSelectionHandler() {
			super();
		}

		@Override
		public void actionPerformed(ActionEvent event) {
			final JMenuItem menuItem = (JMenuItem) event.getSource();
			setSelectedIndex(Integer.parseInt(menuItem.getName()));
		}
	}

	/**
	 * Constructeur.
	 */
	public MTabbedPane() {
		super(TOP);
		// pas SCROLL_TAB_LAYOUT car en Look and feel Nimbus la bordure sous les onglets ne s'afficherait pas
		initListeners();
	}

	/**
	 * Initialisation des listeners.
	 */
	private void initListeners() {
		// listeners inspirés de Patrick Gotthardt's Weblog
		addMouseListener(TAB_SELECTION_MOUSE_HANDLER);
		addMouseWheelListener(MOUSE_WHEEL_SCROLLER);
	}

	void mouseWheelMoved(MouseWheelEvent event) {
		final int dir = event.getWheelRotation() > 0 ? 1 : -1; // wheelRotation
		// peut valoir 2 ou -2 par ex.
		int selIndex = getSelectedIndex();
		final int maxIndex = getTabCount() - 1;
		if (selIndex == 0 && dir < 0 || selIndex == maxIndex && dir > 0) {
			selIndex = maxIndex - selIndex;
		} else {
			selIndex += dir;
		}
		setSelectedIndex(selIndex);
	}

	void mouseClicked(MouseEvent event) {
		// we only look at the right button
		if (SwingUtilities.isRightMouseButton(event)) {
			final JPopupMenu menu = createPopupMenu();
			menu.show(this, event.getX(), event.getY());
		}
	}

	protected JPopupMenu createPopupMenu() {
		final JPopupMenu menu = new JPopupMenu();
		final MenuSelectionHandler menuSelectionHandler = new MenuSelectionHandler();
		final int tabCount = getTabCount();
		final int selectedIndex = getSelectedIndex();
		for (int i = 0; i < tabCount; i++) {
			final JMenuItem menuItem = new JMenuItem(getTitleAt(i), getIconAt(i));
			if (i == selectedIndex) {
				menuItem.setFont(menuItem.getFont().deriveFont(Font.BOLD));
			}
			if (!isEnabledAt(i)) {
				menuItem.setEnabled(false);
			}
			menuItem.setName(String.valueOf(i));
			menuItem.addActionListener(menuSelectionHandler);
			menu.add(menuItem);
		}
		return menu;
	}
}
