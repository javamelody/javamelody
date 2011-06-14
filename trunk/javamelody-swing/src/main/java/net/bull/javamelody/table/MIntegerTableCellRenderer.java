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
