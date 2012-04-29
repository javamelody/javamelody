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

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.Icon;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JSeparator;

import net.bull.javamelody.swing.MButtonTabComponent;
import net.bull.javamelody.swing.MTabbedPane;

/**
 * TabbedPane dont les onglets peuvent être fermés.
 *
 * @author Emeric Vernat
 */
public class TabbedPane extends MTabbedPane {
	private static final long serialVersionUID = 1L;

	private static final String CLOSE = "Close";
	private static final String CLOSE_OTHERS = "CloseOthers";
	private static final String CLOSE_ALL = "CloseAll";

	private class CloseMenuSelectionHandler implements ActionListener {
		CloseMenuSelectionHandler() {
			super();
		}

		@Override
		public void actionPerformed(ActionEvent event) {
			final JMenuItem menuItem = (JMenuItem) event.getSource();
			final String name = menuItem.getName();
			if (CLOSE.equals(name)) {
				removeTabAt(getSelectedIndex());
			} else if (CLOSE_OTHERS.equals(name)) {
				final int tabCount = getTabCount();
				final int selectedIndex = getSelectedIndex();
				for (int i = tabCount - 1; i >= 0; i--) {
					if (i != selectedIndex && getTabComponentAt(i) != null) {
						removeTabAt(i);
					}
				}
			} else if (CLOSE_ALL.equals(name)) {
				final int tabCount = getTabCount();
				for (int i = tabCount - 1; i >= 0; i--) {
					if (getTabComponentAt(i) != null) {
						removeTabAt(i);
					}
				}
			} else {
				throw new IllegalStateException("name inconnu : " + name);
			}
		}
	}

	/** {@inheritDoc} */
	@Override
	public void insertTab(String title, Icon icon, Component component, String tip, int index) {
		super.insertTab(title, icon, component, tip, index);
		setTabComponentAt(index, new MButtonTabComponent(this));
	}

	@Override
	protected JPopupMenu createPopupMenu() {
		final JPopupMenu menu = super.createPopupMenu();
		final CloseMenuSelectionHandler closeMenuSelectionHandler = new CloseMenuSelectionHandler();
		// TODO traductions
		final JMenuItem closeMenuItem = new JMenuItem(CLOSE);
		final JMenuItem closeOthersMenuItem = new JMenuItem(CLOSE_OTHERS);
		final JMenuItem closeAllMenuItem = new JMenuItem(CLOSE_ALL);
		closeMenuItem.setName(CLOSE);
		closeOthersMenuItem.setName(CLOSE_OTHERS);
		closeAllMenuItem.setName(CLOSE_ALL);
		closeMenuItem.addActionListener(closeMenuSelectionHandler);
		closeOthersMenuItem.addActionListener(closeMenuSelectionHandler);
		closeAllMenuItem.addActionListener(closeMenuSelectionHandler);
		menu.add(new JSeparator(), 0);
		menu.add(closeAllMenuItem, 0);
		menu.add(closeOthersMenuItem, 0);
		menu.add(closeMenuItem, 0);
		if (getTabComponentAt(getSelectedIndex()) == null) {
			closeMenuItem.setEnabled(false);
		}

		return menu;
	}
}
