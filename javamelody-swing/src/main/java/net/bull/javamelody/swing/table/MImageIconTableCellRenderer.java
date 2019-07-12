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
