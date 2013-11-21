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
package net.bull.javamelody.swing.table;

import java.text.DateFormat;
import java.util.Date;

/**
 * Définit un renderer pour représenter une Date dans une JTable.
 *
 * @author Emeric Vernat
 */
public class MDateTableCellRenderer extends MDefaultTableCellRenderer {
	private static final long serialVersionUID = 1L;

	private DateFormat dateFormat;

	/**
	 * Constructeur.
	 */
	public MDateTableCellRenderer() {
		super();
		dateFormat = DateFormat.getDateInstance(DateFormat.SHORT);
		setHorizontalAlignment(RIGHT);
	}

	/**
	 * Retourne la valeur de la propriété dateFormat.
	 *
	 * @return DateFormat
	 * @see #setDateFormat
	 */
	public DateFormat getDateFormat() {
		return dateFormat;
	}

	/**
	 * Définit la valeur de la propriété dateFormat.
	 *
	 * @param newDateFormat
	 *           DateFormat
	 * @see #getDateFormat
	 */
	public void setDateFormat(final DateFormat newDateFormat) {
		dateFormat = newDateFormat;
	}

	/** {@inheritDoc} */
	@Override
	public void setValue(final Object value) {
		if (value == null) {
			this.setText(null);
		} else {
			if (value instanceof Date) {
				this.setText(getDateFormat().format((Date) value));
			} else {
				this.setText("??");
			}
		}
	}
}
