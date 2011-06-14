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

import java.util.Collection;
import java.util.Iterator;

/**
 * Définit un renderer pour représenter une List dans une JTable.
 * 
 * @author Emeric Vernat
 */
public class MListTableCellRenderer extends MDefaultTableCellRenderer {
	private static final long serialVersionUID = 1L;

	/** {@inheritDoc} */
	@Override
	public void setValue(final Object value) {
		if (value == null) {
			this.setText(null);
		} else if (value instanceof Collection) {
			// Collection inclue ArrayList, HashMap, Vector, Hashtable ...

			// extrait de AbstractCollection.toString pour éviter substring pour enlever []
			final Collection<?> collection = (Collection<?>) value;
			final StringBuilder stringBuffer = new StringBuilder();
			final Iterator<?> iterator = collection.iterator();
			final int maxIndex = collection.size() - 1;
			for (int i = 0; i <= maxIndex; i++) {
				stringBuffer.append(String.valueOf(iterator.next()));
				if (i < maxIndex) {
					stringBuffer.append(", ");
				}
			}
			this.setText(stringBuffer.toString());
		} else {
			this.setText("??");
		}
	}
}
