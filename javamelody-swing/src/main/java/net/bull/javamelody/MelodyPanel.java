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
package net.bull.javamelody;

import java.awt.LayoutManager;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;

import javax.swing.JPanel;

import net.bull.javamelody.swing.util.MSwingUtilities;

/**
 * Panel parent.
 * @author Emeric Vernat
 */
class MelodyPanel extends JPanel {
	private static final long serialVersionUID = 1L;

	@SuppressWarnings("all")
	private final RemoteCollector remoteCollector;

	MelodyPanel(RemoteCollector remoteCollector, LayoutManager layout) {
		super(layout);
		setOpaque(false);
		assert remoteCollector != null;
		this.remoteCollector = remoteCollector;
	}

	final boolean confirm(String message) {
		return MSwingUtilities.showConfirmation(this, message);
	}

	final void showMessage(final String message) {
		MSwingUtilities.showMessage(this, message);
	}

	/**
	 * Affiche la trace de l'exception dans la console d'erreur et affiche une boîte de dialogue pour afficher l'exception.
	 * @param throwable Throwable
	 */
	static void showException(Throwable throwable) {
		MSwingUtilities.showException(throwable);
	}

	RemoteCollector getRemoteCollector() {
		return remoteCollector;
	}

	File createTempFileForPdf() {
		final String application = getRemoteCollector().getApplication();
		final File tempFile = new File(System.getProperty("java.io.tmpdir"),
				PdfReport.getFileName(application));
		tempFile.deleteOnExit();
		return tempFile;
	}

	OutputStream createFileOutputStream(File tempFile) throws IOException {
		return new BufferedOutputStream(new FileOutputStream(tempFile));
	}
}
