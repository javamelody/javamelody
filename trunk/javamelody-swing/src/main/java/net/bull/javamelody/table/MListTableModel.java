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

import java.util.ArrayList;
import java.util.List;

import javax.swing.JTable;
import javax.swing.table.AbstractTableModel;

/**
 * TableModel de MTable.
 *
 * @param <T>
 *           Type des valeurs de la liste
 * @author Emeric Vernat
 */
public abstract class MListTableModel<T> extends AbstractTableModel {
	private static final long serialVersionUID = 1L;

	private final JTable table;

	@SuppressWarnings("all")
	private List<T> list = new ArrayList<T>(0);

	/**
	 * Constructeur.
	 *
	 * @param table
	 *           JTable
	 */
	public MListTableModel(final JTable table) {
		super();
		this.table = table;
	}

	/** {@inheritDoc} */
	@Override
	public abstract Object getValueAt(final int rowIndex, final int columnIndex);

	/**
	 * Retourne la table liée à ce modèle.
	 *
	 * @return JTable
	 */
	protected JTable getTable() {
		return table;
	}

	/** {@inheritDoc} */
	@Override
	public Class<?> getColumnClass(final int column) {
		if (column >= 0 && column < getColumnCount()) {
			// on parcours la colonne jusqu'à trouver un objet non null
			// pour trouver la classe même si la 1ère ligne est nulle
			final int rowCount = getRowCount();
			for (int i = 0; i < rowCount; i++) {
				final Object value = getValueAt(i, column);
				if (value != null) {
					return value.getClass();
				}
			}
		}

		// en dernier recours
		return Object.class;
	}

	/**
	 * Ajoute un objet.
	 *
	 * @param object
	 *           TypeValue
	 * @see #removeObject
	 */
	public void addObject(final T object) {
		getList().add(object);
		fireTableDataChanged();
	}

	/**
	 * Enlève un objet.
	 *
	 * @param object
	 *           TypeValue
	 * @see #addObject
	 */
	public void removeObject(final T object) {
		getList().remove(object);
		fireTableDataChanged();
	}

	/**
	 * Retourne la valeur de la propriété list.
	 *
	 * @return List
	 * @see #setList
	 */
	public List<T> getList() {
		return new ArrayList<T>(list);
	}

	/**
	 * Définit la valeur de la propriété list.
	 *
	 * @param newList
	 *           List
	 * @see #getList
	 */
	public void setList(final List<T> newList) {
		if (newList != null) {
			list = new ArrayList<T>(newList);
		} else {
			list = new ArrayList<T>(0);
		}
		fireTableDataChanged();
	}

	/**
	 * Retourne l'objet à la position rowIndex.
	 *
	 * @return Object
	 * @param rowIndex
	 *           int
	 */
	public T getObjectAt(final int rowIndex) {
		return getList().get(rowIndex);
	}

	/** {@inheritDoc} */
	@Override
	public int getRowCount() {
		return getList().size();
	}

	/** {@inheritDoc} */
	@Override
	public int getColumnCount() {
		return table.getColumnCount();
	}

	/**
	 * Retourne un booléen suivant que la cellule est éditable (false par défaut). <br/>
	 * Ici, l'implémentation est faite selon que la propriété cellEditor de la TableColumn correspondante est nulle ou non.
	 *
	 * @return boolean
	 * @param row
	 *           int
	 * @param column
	 *           int
	 */
	@Override
	public boolean isCellEditable(final int row, final int column) {
		final int index = table.convertColumnIndexToView(column);
		return table.isEnabled() && table.getColumnModel().getColumn(index).getCellEditor() != null;
	}
}
