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

import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.NumberFormat;

/**
 * Définit un renderer pour représenter un Integer dans une JTable.
 *
 * @author Emeric Vernat
 */
public class MIntegerTableCellRenderer extends MDefaultTableCellRenderer {
	private static final long serialVersionUID = 1L;

	private NumberFormat numberFormat;

	/**
	 * Constructeur.
	 */
	public MIntegerTableCellRenderer() {
		super();
		final DecimalFormatSymbols symbols = DecimalFormatSymbols.getInstance();
		// symbols.setDecimalSeparator(','); (selon la Locale par défaut)
		final String pattern = "#,##0"; // max fraction digits 2, grouping used true, grouping size 3
		numberFormat = new DecimalFormat(pattern, symbols);
		setHorizontalAlignment(RIGHT);
	}

	/**
	 * Retourne la valeur de la propriété numberFormat.
	 *
	 * @return NumberFormat
	 * @see #setNumberFormat
	 */
	public NumberFormat getNumberFormat() {
		return numberFormat;
	}

	/**
	 * Définit la valeur de la propriété numberFormat.
	 *
	 * @param newNumberFormat
	 *           NumberFormat
	 * @see #getNumberFormat
	 */
	public void setNumberFormat(final NumberFormat newNumberFormat) {
		numberFormat = newNumberFormat;
	}

	/** {@inheritDoc} */
	@Override
	public void setValue(final Object value) {
		if (value == null) {
			this.setText(null);
		} else {
			if (value instanceof Integer) {
				this.setText(getNumberFormat().format(value));
			} else if (value instanceof Number) {
				// Number inclue Integer, Long, BigInteger, Double, Float ...
				this.setText(getNumberFormat().format(((Number) value).longValue()));
			} else {
				this.setText("??");
			}
		}
	}
}
