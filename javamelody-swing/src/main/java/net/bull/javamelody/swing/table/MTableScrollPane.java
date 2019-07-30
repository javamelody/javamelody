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

import javax.swing.JScrollPane;

/**
 * ScrollPane avec une table directement intégrée.
 *
 * @param <T>
 *           Type des valeurs de la liste
 * @author Emeric Vernat
 */
public class MTableScrollPane<T> extends JScrollPane {
	private static final long serialVersionUID = 1L;

	/**
	 * Constructeur.
	 */
	public MTableScrollPane() {
		super();
		setViewportView(new MTable<T>());
	}

	/**
	 * Constructeur.
	 *
	 * @param table
	 *           MTable
	 */
	public MTableScrollPane(final MTable<T> table) {
		super();
		if (table == null) {
			throw new IllegalArgumentException();
		}
		setViewportView(table);
	}

	/**
	 * Retourne la table incluse.
	 *
	 * @return MTable
	 */
	@SuppressWarnings("unchecked")
	public MTable<T> getTable() {
		return (MTable<T>) getViewport().getView();
	}

	/**
	 * Redéfinit la table incluse.
	 *
	 * @param table
	 *           MTable
	 */
	public void setTable(final MTable<T> table) {
		setViewportView(table);
	}
}
