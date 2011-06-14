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

import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.UIManager;
import javax.swing.border.Border;
import javax.swing.table.TableCellRenderer;

/**
 * Définit un renderer pour représenter un Boolean à l'aide d'une checkbox dans une JTable.
 * 
 * @author Emeric Vernat
 */
public class MBooleanTableCellRenderer extends JCheckBox implements TableCellRenderer {
	private static final long serialVersionUID = 1L;

	@SuppressWarnings("all")
	private static final Border BORDER = UIManager.getBorder("Table.focusCellHighlightBorder");

	/**
	 * Constructeur.
	 */
	public MBooleanTableCellRenderer() {
		super();
		setHorizontalAlignment(CENTER);
		setOpaque(true);
		setEnabled(false);
		setBorderPainted(true);
	}

	/** {@inheritDoc} */
	@Override
	public Component getTableCellRendererComponent(final JTable table, final Object value,
			final boolean isSelected, final boolean hasFocus, final int row, final int column) {

		if (isSelected) {
			super.setForeground(table.getSelectionForeground());
			super.setBackground(table.getSelectionBackground());
		} else {
			super.setForeground(table.getForeground());
			if (MTable.BICOLOR_LINE != null && row % 2 == 0) {
				super.setBackground(MTable.BICOLOR_LINE);
			} else {
				super.setBackground(table.getBackground());
			}
		}

		if (hasFocus) {
			setBorder(BORDER);
		} else {
			setBorder(null);
		}

		setEnabled(table.isCellEditable(row, column));

		if (value instanceof Boolean) {
			final boolean selected = ((Boolean) value).booleanValue();
			this.setSelected(selected);
			// this.setToolTipText(selected ? "vrai" : "false");
			return this;
		}
		final JLabel label = (JLabel) table.getDefaultRenderer(String.class)
				.getTableCellRendererComponent(table, null, isSelected, hasFocus, row, column);
		if (value == null) {
			label.setText(null);
		} else {
			label.setText("??");
		}
		return label;
	}
}
