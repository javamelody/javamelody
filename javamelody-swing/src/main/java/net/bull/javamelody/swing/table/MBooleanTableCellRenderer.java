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
