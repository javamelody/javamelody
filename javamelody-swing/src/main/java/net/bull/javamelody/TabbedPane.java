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

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;

import javax.swing.Icon;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JSeparator;

import net.bull.javamelody.internal.common.I18N;
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
	protected void processMouseEvent(MouseEvent event) { // NOPMD
		super.processMouseEvent(event);
	}

	@Override
	protected JPopupMenu createPopupMenu() {
		final JPopupMenu menu = super.createPopupMenu();
		final CloseMenuSelectionHandler closeMenuSelectionHandler = new CloseMenuSelectionHandler();
		final JMenuItem closeMenuItem = new JMenuItem(I18N.getString("Fermer"));
		final JMenuItem closeOthersMenuItem = new JMenuItem(I18N.getString("Fermer_les_autres"));
		final JMenuItem closeAllMenuItem = new JMenuItem(I18N.getString("Fermer_tout"));
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
