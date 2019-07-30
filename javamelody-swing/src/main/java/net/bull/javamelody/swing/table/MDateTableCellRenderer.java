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
