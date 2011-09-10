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

import java.awt.Rectangle;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.swing.ListSelectionModel;
import javax.swing.table.TableModel;

/**
 * Composant Table typé servant de base à MTable.
 *
 * @param <T>
 *           Type des valeurs de la liste
 * @author Emeric Vernat
 */
public class MListTable<T> extends MBasicTable {
	private static final long serialVersionUID = 1L;

	/**
	 * Constructeur.
	 *
	 * @param dataModel
	 *           Modèle pour les données (par exemple, MTableModel)
	 */
	public MListTable(final MListTableModel<T> dataModel) {
		super(dataModel);
	}

	/** {@inheritDoc} */
	@Override
	public void setModel(final TableModel tableModel) {
		if (!(tableModel instanceof MListTableModel)) {
			throw new IllegalArgumentException("model doit être instance de "
					+ MListTableModel.class.getName());
		}
		super.setModel(tableModel);
	}

	/**
	 * Retourne la valeur de la propriété listTableModel.
	 *
	 * @return MListTableModel
	 */
	@SuppressWarnings("unchecked")
	protected MListTableModel<T> getListTableModel() {
		return (MListTableModel<T>) getModel();
	}

	/**
	 * Retourne la valeur de la propriété list.
	 *
	 * @return List
	 * @see #setList
	 */
	public List<T> getList() {
		return getListTableModel().getList();
	}

	/**
	 * Définit la valeur de la propriété list. <BR>
	 *
	 * Cette méthode adaptent automatiquement les largeurs des colonnes selon les données.
	 *
	 * @param newList
	 *           List
	 * @see #getList
	 */
	public void setList(final List<T> newList) {
		getListTableModel().setList(newList);

		adjustColumnWidths();

		// Réinitialise les hauteurs des lignes qui pourraient avoir été particularisées
		// avec setRowHeight(row, rowHeight).
		setRowHeight(getRowHeight());

		// remarque perf: on pourrait parcourir les colonnes pour affecter des renderers aux colonnes n'en ayant pas selon getDefaultRenderer(getColumnClass(i))
		// pour éliminer les appels à getDefaultRenderer pour chaque cellule
	}

	/**
	 * Retourne la liste d'objets sélectionnés.
	 *
	 * @return List
	 * @see #setSelectedList
	 */
	public List<T> getSelectedList() {
		final int[] selectedRows = getSelectedRows();
		int selectedRow;
		final int rowCount = getRowCount();
		final int length = selectedRows.length;
		final List<T> selectedList = new ArrayList<T>(length);
		for (int i = 0; i < length; i++) {
			selectedRow = selectedRows[i];
			// getSelectedRows peut renvoyer des lignes qui ne sont plus
			// dans la table si ce sont les denières sélectionnées
			if (selectedRow >= 0 && selectedRow < rowCount) {
				selectedList.add(getObjectAt(selectedRow));
			}
		}

		return selectedList;
	}

	/**
	 * Retourne l'objet sélectionné.
	 *
	 * @return TypeValue
	 * @see #setSelectedObject
	 */
	public T getSelectedObject() {
		if (getSelectionModel().getSelectionMode() != ListSelectionModel.SINGLE_SELECTION) {
			throw new IllegalStateException(
					"Appel à getSelectedObject() invalide pour une table en sélection multiple");
		}
		return getObjectAt(getSelectedRow());
	}

	/**
	 * Définit la liste d'objets sélectionnés.
	 *
	 * @param newSelectedList
	 *           List
	 * @see #getSelectedList
	 */
	public void setSelectedList(final List<T> newSelectedList) {
		clearSelection();
		if (newSelectedList == null || newSelectedList.isEmpty()) {
			return;
		}

		final ListSelectionModel listSelectionModel = getSelectionModel();
		final List<T> list = getList();
		Object object;
		int rowIndex;

		for (final Iterator<T> it = newSelectedList.iterator(); it.hasNext();) {
			object = it.next();
			rowIndex = list.indexOf(object);
			rowIndex = convertRowIndexToView(rowIndex);
			if (rowIndex > -1) {
				listSelectionModel.addSelectionInterval(rowIndex, rowIndex);
			}
		}

		// scrolle pour afficher la première ligne sélectionnée
		final int firstIndex = getSelectionModel().getMinSelectionIndex();
		final Rectangle cellRect = getCellRect(firstIndex, 0, true);
		scrollRectToVisible(cellRect);
	}

	/**
	 * Définit l'objet sélectionné. L'objet peut être null pour ne rien sélectionné.
	 *
	 * @param newSelectedObject
	 *           TypeValue
	 * @see #getSelectedObject
	 */
	public void setSelectedObject(final T newSelectedObject) {
		if (newSelectedObject != null) {
			final List<T> newSelectedList = new ArrayList<T>(1);
			newSelectedList.add(newSelectedObject);
			setSelectedList(newSelectedList);
		} else {
			clearSelection();
		}
	}

	/**
	 * Renvoie l'objet à la position demandée. Attention rowIndex est l'index vu de la JTable avec les index, et non pas vu du ListTableModel.
	 *
	 * @return Object
	 * @param rowIndex
	 *           int
	 */
	T getObjectAt(final int rowIndex) {
		// getSelectedRow peut renvoyer une ligne qui n'est plus dans la table
		// si c'est la dernière sélectionnée
		if (rowIndex < 0 || rowIndex >= getRowCount()) {
			return null;
		}

		final int row = convertRowIndexToModel(rowIndex);

		return getListTableModel().getObjectAt(row);
	}
}
