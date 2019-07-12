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

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JPopupMenu;

import org.apache.log4j.Logger;

import net.bull.javamelody.swing.MMenuItem;
import net.bull.javamelody.swing.print.MCsvLocalWriter;
import net.bull.javamelody.swing.print.MHtmlWriter;
import net.bull.javamelody.swing.print.MJavaPrinter;
import net.bull.javamelody.swing.print.MJsonWriter;
import net.bull.javamelody.swing.print.MPdfWriter;
import net.bull.javamelody.swing.print.MPrinter;
import net.bull.javamelody.swing.print.MRtfWriter;
import net.bull.javamelody.swing.print.MXmlWriter;
import net.bull.javamelody.swing.util.MSwingUtilities;

/**
 * Popup menu des tables.
 *
 * @author Emeric Vernat
 */
class TablePopupMenu extends JPopupMenu {
	private static final long serialVersionUID = 1L;

	/**
	 * Constructeur.
	 * @param table MBasicTable
	 */
	TablePopupMenu(final MBasicTable table) {
		super();

		final List<MPrinter> printers = getPrinters();
		for (final MPrinter printer : printers) {
			final MMenuItem menuItem = new MMenuItem(printer.getName(), printer.getIcon());
			add(menuItem);
			menuItem.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(final ActionEvent event) {
					try {
						printer.print(table);
					} catch (final IOException e) {
						MSwingUtilities.showException(e);
					}
				}
			});
		}
	}

	/**
	 * Retourne la liste des objets d'export / impression.
	 *
	 * @return List
	 */
	private List<MPrinter> getPrinters() {
		// ne sont pas inclus le printer "Clipboard" qui est utilisé directement avec Ctrl+C, les printers PDF/RTF paysages et le printer CSV US
		final List<MPrinter> printers = new ArrayList<>();
		printers.add(new MCsvLocalWriter());
		try {
			Class.forName("com.lowagie.text.Document");
			printers.add(new MPdfWriter());
		} catch (final ClassNotFoundException e) {
			// l'export PDF ne sera pas disponible dans cette application
			Logger.getLogger(TablePopupMenu.class).debug("Export PDF non disponible sans iText");
		}
		try {
			Class.forName("com.lowagie.text.rtf.RtfWriter2");
			printers.add(new MRtfWriter());
		} catch (final ClassNotFoundException e) {
			// l'export RTF ne sera pas disponible dans cette application
			Logger.getLogger(TablePopupMenu.class)
					.debug("Export RTF non disponible sans iText-RTF");
		}
		printers.add(new MHtmlWriter());
		try {
			Class.forName("com.thoughtworks.xstream.XStream");
			printers.add(new MXmlWriter());
			printers.add(new MJsonWriter());
		} catch (final ClassNotFoundException e) {
			// l'export XML et JSON ne seront pas disponibles dans cette application
			Logger.getLogger(TablePopupMenu.class)
					.debug("Exports XML et JSON non disponibles sans XStream et XPP3");
		}
		printers.add(new MJavaPrinter());
		return printers;
	}
}
