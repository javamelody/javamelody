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

import javax.swing.JLabel;
import javax.swing.JTable;

/**
 * Définit un renderer pour représenter un Component AWT ou Swing se dessinant lui-même dans une JTable (par ex. un JLabel avec icône, texte, toolTipText, fonte, foreground, horizontalAlignement ...).
 * 
 * @author Emeric Vernat
 */
public class MComponentTableCellRenderer extends MDefaultTableCellRenderer {
	private static final long serialVersionUID = 1L;

	private static final String ERROR_TEXT = "??";

	private boolean error;

	/** {@inheritDoc} */
	@Override
	public Component getTableCellRendererComponent(final JTable table, final Object value,
			final boolean isSelected, final boolean hasFocus, final int row, final int column) {
		Component component;
		if (error || value == null) {
			setText(null);
			error = false;
			component = this;
		}

		if (value instanceof Component) {
			component = (Component) value;
		} else {
			this.setText(ERROR_TEXT);
			error = true;
			component = this;
		}

		if (component instanceof JLabel) {
			final Component superComponent = super.getTableCellRendererComponent(table, null,
					isSelected, hasFocus, row, column);
			if (superComponent instanceof JLabel) {
				final JLabel label = (JLabel) component;
				final JLabel superLabel = (JLabel) superComponent;
				label.setBackground(superLabel.getBackground());
				label.setBorder(superLabel.getBorder());
			}
		}

		return component;
	}
}
