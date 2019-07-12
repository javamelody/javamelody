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
	 *           JTable
	 * @param component
	 *           Component
	 * @param row
	 *           int
	 */
	protected void adjustRowHeight(final JTable table, final Component component, final int row) {
		// Ajustement de la hauteur de cette ligne en fonction de la taille du renderer
		final int cellHeight = table.getRowHeight(row);
		final int rendererHeight = component.getPreferredSize().height;
		if (cellHeight < rendererHeight - 4) { // dans le cas normal, cellHeight est à 16 et rendererHeight est à 20
			table.setRowHeight(row, rendererHeight);
		}
	}
}
