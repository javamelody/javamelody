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

import java.text.DateFormat;

/**
 * Définit un renderer pour représenter une heure (Date) dans une JTable.
 *
 * @author Emeric Vernat
 */
public class MHourTableCellRenderer extends MDateTableCellRenderer {
	private static final long serialVersionUID = 1L;

	/**
	 * Constructeur.
	 */
	public MHourTableCellRenderer() {
		super();
		setDateFormat(DateFormat.getTimeInstance(DateFormat.SHORT));
	}
}
