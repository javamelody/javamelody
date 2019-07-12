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
		final Component component;
		if (error || value == null) {
			setText(null);
			error = false;
			component = this;
		} else if (value instanceof Component) {
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
