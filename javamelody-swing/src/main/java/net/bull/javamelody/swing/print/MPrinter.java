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
package net.bull.javamelody.swing.print;

import java.awt.Component;
import java.awt.Desktop;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;

import javax.swing.Icon;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JTabbedPane;
import javax.swing.JTable;

import net.bull.javamelody.swing.table.MBasicTable;

/**
 * Classe d'impression et d'export.
 *
 * @author Emeric Vernat
 */
public abstract class MPrinter {
	// instance du fileChooser (pour export)
	private static JFileChooser fileChooser;

	/**
	 * Constructeur.
	 */
	protected MPrinter() {
		super();
	}

	/**
	 * Construit le titre à inclure dans l'impression/export.
	 *
	 * @param component
	 *           Component
	 * @return String
	 */
	protected String buildTitle(final Component component) {
		Component current = component;
		final StringBuilder title = new StringBuilder();
		while (current != null) {
			final String currentTitle = getTitleIfKnownComponent(current);
			if (currentTitle != null && currentTitle.length() != 0) {
				if (title.length() != 0) {
					title.insert(0, " - ");
				}
				title.insert(0, currentTitle);
			}
			current = current.getParent();
		}

		return title.length() != 0 ? title.toString() : null;
	}

	private String getTitleIfKnownComponent(Component current) {
		if (current instanceof JTabbedPane) {
			final JTabbedPane tabbedPane = (JTabbedPane) current;
			return tabbedPane.getTitleAt(tabbedPane.getSelectedIndex());
		} else if (current instanceof JFrame) {
			return ((JFrame) current).getTitle();
		} else if (current instanceof JDialog) {
			return ((JDialog) current).getTitle();
		}
		return null;
	}

	/**
	 * Renvoie le texte de la cellule de cette JTable à la ligne et la colonne spécifiée.
	 *
	 * @return String
	 * @param table
	 *           MBasicTable
	 * @param row
	 *           int
	 * @param column
	 *           int
	 */
	protected String getTextAt(final MBasicTable table, final int row, final int column) {
		String text = table.getTextAt(row, column);
		// suppression de tags éventuellements présents (par ex. avec un MultiLineTableCellRenderer)
		if (text != null && text.startsWith("<html>")) {
			text = text.replaceFirst("<html>", "");
			text = text.replaceFirst("<center>", "");
			text = text.replaceAll("<br/>", "\n");
			text = text.replaceAll("&nbsp;", " ");
			text = text.replaceFirst("<font color='#[0-9]*+'>", "");
		}
		return text;
	}

	/**
	 * Renvoie la valeur de la cellule de cette JTable à la ligne et la colonne spécifiée.
	 *
	 * @return String
	 * @param table
	 *           MBasicTable
	 * @param row
	 *           int
	 * @param column
	 *           int
	 */
	protected Object getValueAt(final MBasicTable table, final int row, final int column) {
		return table.getModel().getValueAt(row, column);
	}

	/**
	 * Renvoie la boîte de dialogue swing de choix du fichier d'export. (Initialisée pour s'ouvrir sur le répertoire courant user.dir).
	 *
	 * @return JFileChooser
	 */
	public static synchronized JFileChooser getFileChooser() { // NOPMD
		if (fileChooser == null) {
			final String currentDirectory = System.getProperty("user.dir");
			fileChooser = new JFileChooser(currentDirectory);
		}
		return fileChooser;
	}

	/**
	 * Choix du fichier pour un export.
	 *
	 * @return File
	 * @param table
	 *           JTable
	 * @param extension
	 *           String
	 * @throws IOException
	 *            Erreur disque
	 */
	protected File chooseFile(final JTable table, final String extension) throws IOException {
		final JFileChooser myFileChooser = getFileChooser();

		final MExtensionFileFilter filter = new MExtensionFileFilter(extension);
		myFileChooser.addChoosableFileFilter(filter);

		String title = buildTitle(table);
		if (title != null) {
			// on remplace par des espaces les caractères interdits dans les noms de fichiers : \ / : * ? " < > |
			final String notAllowed = "\\/:*?\"<>|";
			final int notAllowedLength = notAllowed.length();
			for (int i = 0; i < notAllowedLength; i++) {
				title = title.replace(notAllowed.charAt(i), ' ');
			}
			myFileChooser.setSelectedFile(new File(title));
		}
		// l'extension sera ajoutée ci-dessous au nom du fichier

		try {
			final Component parent = table.getParent() != null ? table.getParent() : table;
			if (myFileChooser.showSaveDialog(parent) == JFileChooser.APPROVE_OPTION) {
				String fileName = myFileChooser.getSelectedFile().getCanonicalPath();
				if (!fileName.endsWith('.' + extension)) {
					fileName += '.' + extension; // NOPMD
				}

				return new File(fileName);
			}
			return null;
		} finally {
			myFileChooser.removeChoosableFileFilter(filter);
		}
	}

	/**
	 * Retourne le fichier de sortie, par défaut on ouvre la boîte de dialogue de choix du fichier.
	 *
	 * @return File
	 * @param table
	 *           JTable
	 * @throws IOException
	 *            Erreur disque
	 */
	protected File getFile(final JTable table) throws IOException {
		return chooseFile(table, getFileExtension());
	}

	/**
	 * Méthode abstraite assurant le polymorphisme des instances.
	 *
	 * @param table
	 *           MBasicTable
	 * @param outputStream
	 *           OutputStream
	 * @throws IOException
	 *            Erreur disque
	 */
	public abstract void print(MBasicTable table, OutputStream outputStream) throws IOException;

	/**
	 * Méthode abstraite : les instances doivent renvoyer leur nom.
	 *
	 * @return String
	 */
	public abstract String getName();

	/**
	 * Méthode abstraite : les instances doivent renvoyer l'extension du fichier exporté.
	 *
	 * @return String
	 */
	public abstract String getFileExtension();

	/**
	 * Méthode abstraite : les instances doivent renvoyer l'icône représentant le type.
	 *
	 * @return Icon
	 */
	public abstract Icon getIcon();

	/**
	 * Impression/export de la table.
	 *
	 * @param table
	 *           MBasicTable
	 * @throws IOException
	 *            Erreur disque
	 */
	public void print(final MBasicTable table) throws IOException {
		final File file = getFile(table);
		if (file != null) {
			try (final OutputStream outputStream = new BufferedOutputStream(new FileOutputStream(file))) {
				print(table, outputStream);
			}

			showDocument(file);
		}
	}

	/**
	 * Affiche le document. <br/>
	 * Les implémentations peuvent surcharger cette méthode.
	 *
	 * @param targetFile
	 *           File
	 * @throws IOException
	 *            Erreur disque
	 */
	protected void showDocument(final File targetFile) throws IOException {
		try {
			Desktop.getDesktop().open(targetFile);
		} catch (final IOException e) {
			throw new IOException("Is there an associated application for \"" + targetFile.getName()
					+ "\"?\n" + e.getMessage(), e);
		}

		// on pourrait imprimer le fichier directement (par exemple CSV avec Excel) en supposant que Desktop.getDesktop().isDesktopSupported()
		// et Desktop.getDesktop().isSupported(Desktop.Action.PRINT) retournent true
		// ce qui est le cas en Windows XP SP2 (et Java 1.6)
		// Desktop.getDesktop().print(targetFile);
	}

	/**
	 * Retourne le nom pour affichage.
	 *
	 * @return String
	 */
	@Override
	public String toString() {
		return getName();
	}
}
