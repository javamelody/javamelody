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

import java.awt.Color;
import java.awt.Component;

import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableModel;

/**
 * Composant Table, qui utilise une API typée pour la liste des valeurs et qui utilise les String (enum) pour définir les valeurs dans les colonnes.
 *
 * @param <T>
 *           Type des valeurs de la liste
 * @author Emeric Vernat
 */
public class MTable<T> extends MListTable<T> {
	// TODO étudier l'intérêt d'utiliser JXTable de SwingX (malgré l'abandon du projet par Oracle?)
	// et si oui remplacer cette implémentation

	static final Color BICOLOR_LINE = Color.decode("#E7E7E7");

	private static final long serialVersionUID = 1L;

	/**
	 * Constructeur.
	 */
	public MTable() {
		// on utilise le modèle par défaut créé par la méthode createDefaultDataModel() ci-dessus
		this(null);
	}

	/**
	 * Constructeur.
	 *
	 * @param dataModel
	 *           Modèle pour les données
	 */
	protected MTable(final MListTableModel<T> dataModel) {
		super(dataModel);
		// la table ne crée pas automatiquement les colonnes à partir du dataModel,
		// et les colonnes seront définies en utilisant la méthode addColumn ci-dessous
		setAutoCreateColumnsFromModel(false);
		// le rowSorter sera automatiquement ajouté lors des appels à setModel (la classe MListTableModel définit getColumnClass à partir des données)
		setAutoCreateRowSorter(true);
		// fond de couleur blanc (plutôt que gris en look and feel Nimbus ; utile pour le rendu des cases à cocher dans le tableau)
		setBackground(Color.WHITE);
	}

	@Override
	protected TableModel createDefaultDataModel() {
		return new MTableModel<T>(this);
	}

	/**
	 * Ajoute une colonne dans la table.
	 *
	 * @param attribute
	 *           Nom de l'attribut des objets à afficher dans la colonne<br/>
	 * @param libelle
	 *           Libellé à afficher en entête de la colonne
	 * @return this (fluent)
	 */
	public MTable<T> addColumn(final String attribute, final String libelle) {
		final int modelIndex = getColumnCount();
		final TableColumn tableColumn = new TableColumn(modelIndex);
		// on met l'énumération de l'attribut comme identifier dans le TableColumn pour s'en servir dans MTableModel
		tableColumn.setIdentifier(attribute);
		if (libelle == null) {
			// on prend par défaut l'attribut si le libellé n'est pas précisé,
			tableColumn.setHeaderValue(attribute);
		} else {
			// le libellé a été précisé pour l'entête de cette colonne
			tableColumn.setHeaderValue(libelle);
		}
		// ajoute la colonne dans la table
		super.addColumn(tableColumn);
		return this;
	}

	/**
	 * Méthode typée pour définir un renderer spécifique pour les cellules d'une colonne dans la table.
	 *
	 * @param attribute
	 *           Nom de l'attribut des objets pour une colonne existante
	 * @param cellRenderer
	 *           Renderer des cellules dans cette colonne
	 */
	public void setColumnCellRenderer(final String attribute, final TableCellRenderer cellRenderer) {
		getColumn(attribute).setCellRenderer(cellRenderer);
	}

	/** {@inheritDoc} */
	@Override
	public Component prepareRenderer(final TableCellRenderer renderer, final int rowIndex,
			final int vColIndex) {
		// Surcharge pour la gestion des lignes de couleurs alternées.
		final Component component = super.prepareRenderer(renderer, rowIndex, vColIndex);
		if (BICOLOR_LINE != null && !isRowSelected(rowIndex)) {
			if (rowIndex % 2 == 0) {
				component.setBackground(BICOLOR_LINE);
			} else {
				component.setBackground(getBackground());
			}
		}

		return component;
	}
}
