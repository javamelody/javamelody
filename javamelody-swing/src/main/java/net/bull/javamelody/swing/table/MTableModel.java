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
			// le getter pour un attribut commence par "get" ou "is" selon la convention de nommage
			final String upperFirstIdentifier = Character.toUpperCase(identifier.charAt(0))
					+ identifier.substring(1);
			Method method;
			try {
				method = object.getClass().getDeclaredMethod("get" + upperFirstIdentifier,
						(Class<?>[]) null);
			} catch (final NoSuchMethodException e) {
				try {
					method = object.getClass().getDeclaredMethod("is" + upperFirstIdentifier,
							(Class<?>[]) null);
				} catch (final NoSuchMethodException e2) {
					// si non trouvée non plus, alors l'exception lancée est e et non e2
					throw e;
				}
			}
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
