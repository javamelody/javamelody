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

import java.awt.Component;

import javax.swing.JTable;
import javax.swing.table.DefaultTableCellRenderer;

/**
 * Renderer par défaut pour les cellules des JTables. <br/>
 * La super classe contient un certain nombre d'optimisation de performances pour le rendu.
 *
 * @author Emeric Vernat
 */
public class MDefaultTableCellRenderer extends DefaultTableCellRenderer {
	private static final long serialVersionUID = 1L;

	/**
	 * Ajustement de la hauteur de cette ligne en fonction de la taille du renderer (ex: la taille d'une icône ou d'un label html).
	 *
	 * @param table
	 *           javax.swing.JTable
	 * @param component
	 *           java.awt.Component
	 * @param row
	 *           int
	 */
	protected void adjustRowHeight(final JTable table, final Component component, final int row) {
		// Ajustement de la hauteur de cette ligne en fonction de la taille du renderer
		final int cellHeight = table.getRowHeight(row);
		final int rendererHeight = component.getPreferredSize().height;
		if (cellHeight < rendererHeight - 2) { // dans le cas normal, cellHeight est � 16 et rendererHeight est � 18
			table.setRowHeight(row, rendererHeight);
		}
	}
}
