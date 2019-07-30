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
	private List<T> list = new ArrayList<>(0);

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
	public abstract Object getValueAt(int rowIndex, int columnIndex);

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
		return new ArrayList<>(list);
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
			list = new ArrayList<>(newList);
		} else {
			list = new ArrayList<>(0);
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
