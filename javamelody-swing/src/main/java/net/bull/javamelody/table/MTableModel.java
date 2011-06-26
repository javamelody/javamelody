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

import java.lang.reflect.Method;

import javax.swing.JTable;

/**
 * TableModel de MTable, qui utilise les String (enum) pour déterminer les valeurs dans les colonnes.
 *
 * @param <T>
 *           Type des valeurs de la liste
 * @author Emeric Vernat
 */
public class MTableModel<T> extends MListTableModel<T> {
	private static final long serialVersionUID = 1L;

	/**
	 * Constructeur.
	 *
	 * @param table
	 *           JTable
	 */
	public MTableModel(final JTable table) {
		super(table);
	}

	/** {@inheritDoc} */
	@Override
	public int getColumnCount() {
		return getTable().getColumnCount();
	}

	/** {@inheritDoc} */
	@Override
	public Object getValueAt(final int rowIndex, final int columnIndex) {
		final T object = getObjectAt(rowIndex);
		final String identifier = getIdentifier(columnIndex);
		if (identifier == null || object == null) {
			// cas particulier: si cette colonne n'a pas d'identifier, alors on suppose qu'il y a un renderer spécifique sur la colonne
			// qui utilisera comme valeur l'objet correspondant à cette ligne dans le tableau
			return object;
		}
		// la valeur affichée dans la cellule est le résultat de l'appel
		// à la méthode "get" + Identifier sur l'objet
		try {
			// le getter pour un attribut commence par "get" selon la convention de nommage
			final Method method = object.getClass().getDeclaredMethod(
					"get" + Character.toUpperCase(identifier.charAt(0)) + identifier.substring(1),
					(Class<?>[]) null);
			// la méthode n'est pas forcément public dans notre cas
			method.setAccessible(true);
			return method.invoke(object, (Object[]) null);
		} catch (final Exception e) {
			// s'il y a une erreur dans le getter, alors il ne faut surtout pas afficher de popup d'erreur
			// car tous les affichages de cellules vont en permanence afficher des popups ce qui rendrait l'application bloquée,
			// donc on se contente de logguer le message de l'exception dans un warning (sans trace de l'exception pour ne pas saturer le log non plus)
			// et on affiche "??" dans la cellule
			e.printStackTrace(System.err);
			return "??";
		}
	}

	// note: cette méthode getColumnName n'est normalement jamais appelée, car le libellé de la colonne est déjà défini quand on appelle MTable.addColumn(String, String)
	/** {@inheritDoc} */
	@Override
	public String getColumnName(final int columnIndex) {
		final String identifier = getIdentifier(columnIndex);
		if (identifier == null) {
			return super.getColumnName(columnIndex);
		}
		return identifier;
	}

	/**
	 * Retourne l'attribut correspondant à un numéro de colonne.
	 *
	 * @param columnIndex
	 *           int
	 * @return String
	 */
	private String getIdentifier(final int columnIndex) {
		final int index = getTable().convertColumnIndexToView(columnIndex);
		// selon MTable.addColumn, l'identifier est de type String
		final Object identifier = getTable().getColumnModel().getColumn(index).getIdentifier();
		if (identifier instanceof String) {
			// si l'identifier est de type String, alors on va l'utiliser dans getValueAt, getColumnClass ou getColumnName
			return (String) identifier;
		}
		// mais sinon on ignore l'identifier et ces 3 méthodes auront un comportement par défaut
		return null;
	}
}
