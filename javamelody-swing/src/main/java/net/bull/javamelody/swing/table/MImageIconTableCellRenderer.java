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
package net.bull.javamelody.swing.table;

import javax.swing.Icon;
import javax.swing.SwingConstants;

/**
 * Définit un renderer pour représenter une Icon dans une JTable.
 *
 * @author Emeric Vernat
 */
public class MImageIconTableCellRenderer extends MDefaultTableCellRenderer {
	private static final long serialVersionUID = 1L;

	private static final String ERROR_TEXT = "??";

	private boolean error;

	/**
	 * Constructeur.
	 */
	public MImageIconTableCellRenderer() {
		super();
		setHorizontalAlignment(SwingConstants.CENTER);
	}

	/** {@inheritDoc} */
	@Override
	public void setValue(final Object value) {
		if (error) {
			setText(null);
			error = false;
		}

		if (value == null) {
			setIcon(null);
		} else {
			if (value instanceof Icon) {
				setIcon((Icon) value);
			} else {
				this.setText(ERROR_TEXT);
				error = true;
			}
		}
	}
}
