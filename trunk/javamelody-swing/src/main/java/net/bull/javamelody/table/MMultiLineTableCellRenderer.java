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
package net.bull.javamelody.table;

import java.awt.Component;
import java.util.StringTokenizer;

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
				final StringTokenizer st = new StringTokenizer(text, "\n");
				while (st.hasMoreTokens()) {
					sb.append(st.nextToken());
					if (st.hasMoreTokens()) {
						sb.append("<br>");
					}
				}
				setToolTipText(text);
				setText(sb.toString());
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
