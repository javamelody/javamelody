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
package net.bull.javamelody.swing.table;

import java.awt.Component;

import javax.swing.JTable;

/**
 * Définit un renderer pour représenter une cellule avec des retours chariots, de hauteur variable, dans une JTable.
 *
 * @author Emeric Vernat
 */
public class MMultiLineTableCellRenderer extends MDefaultTableCellRenderer {
	private static final long serialVersionUID = 1L;

	/** {@inheritDoc} */
	@Override
	public void setValue(final Object value) {
		if (value == null) {
			setToolTipText(null);
			setText("");
		} else {
			final String text = value.toString();
			if (text.indexOf('\n') == -1) {
				setToolTipText(null);
				setText(text);
			} else {
				final StringBuilder sb = new StringBuilder(text.length() + text.length() / 4);
				sb.append("<html>");
				if (getHorizontalAlignment() == CENTER) {
					sb.append("<center>");
				}
				sb.append(text.replace("\n", "<br/>").replace(" ", "&nbsp;"));
				final String string = sb.toString();
				setToolTipText(string);
				setText(string);
			}
		}
	}

	/** {@inheritDoc} */
	@Override
	public Component getTableCellRendererComponent(final JTable table, final Object value,
			final boolean isSelected, final boolean hasFocus, final int row, final int column) {
		// Surcharge pour appeler adjustRowHeight.
		final Component component = super.getTableCellRendererComponent(table, value, isSelected,
				hasFocus, row, column);
		adjustRowHeight(table, component, row);
		return component;
	}
}
