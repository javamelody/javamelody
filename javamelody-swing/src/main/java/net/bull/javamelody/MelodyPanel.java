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
package net.bull.javamelody;

import java.awt.BorderLayout;
import java.awt.Desktop;
import java.awt.Dimension;
import java.awt.LayoutManager;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import javax.swing.AbstractAction;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.KeyStroke;

import net.bull.javamelody.internal.common.I18N;
import net.bull.javamelody.internal.model.Collector;
import net.bull.javamelody.internal.model.Counter;
import net.bull.javamelody.internal.model.JavaInformations;
import net.bull.javamelody.internal.model.RemoteCollector;
import net.bull.javamelody.internal.model.TransportFormat;
import net.bull.javamelody.internal.web.pdf.PdfOtherReport;
import net.bull.javamelody.internal.web.pdf.PdfReport;
import net.bull.javamelody.swing.MButton;
import net.bull.javamelody.swing.MMenuItem;

/**
 * Panel parent.
 * @author Emeric Vernat
 */
class MelodyPanel extends JPanel {
	private static final long serialVersionUID = 1L;
	private static final ImageIcon MORE_ICON = new ImageIcon(
			MainButtonsPanel.class.getResource("/icons/down-arrow.png"));

	@SuppressWarnings("all")
	private final RemoteCollector remoteCollector;

	MelodyPanel(RemoteCollector remoteCollector) {
		this(remoteCollector, new BorderLayout());
	}

	MelodyPanel(RemoteCollector remoteCollector, LayoutManager layout) {
		super(layout);
		setOpaque(false);
		assert remoteCollector != null;
		this.remoteCollector = remoteCollector;
	}

	final boolean confirm(String message) {
		return MainFrame.showConfirmation(this, message);
	}

	final void showMessage(final String message) {
		MainFrame.showMessage(this, message);
	}

	/**
	 * Affiche la trace de l'exception dans la console d'erreur et affiche une boîte de dialogue pour afficher l'exception.
	 * @param throwable Throwable
	 */
	final void showException(Throwable throwable) {
		MainFrame.showException(this, throwable);
	}

	final RemoteCollector getRemoteCollector() {
		return remoteCollector;
	}

	final Collector getCollector() {
		return getRemoteCollector().getCollector();
	}

	final List<JavaInformations> getJavaInformationsList() {
		return getRemoteCollector().getJavaInformationsList();
	}

	/**
	 * Retourne une traduction dans la locale courante.
	 * @param key clé d'un libellé dans les fichiers de traduction
	 * @return String
	 */
	static String getString(String key) {
		return I18N.getString(key);
	}

	/**
	 * Retourne une traduction dans la locale courante et insère les arguments aux positions {i}.
	 * @param key clé d'un libellé dans les fichiers de traduction
	 * @param arguments Valeur à inclure dans le résultat
	 * @return String
	 */
	static String getFormattedString(String key, Object... arguments) {
		return I18N.getFormattedString(key, arguments);
	}

	final File createTempFileForPdf() {
		final String application = getRemoteCollector().getApplication();
		File tempFile = new File(System.getProperty("java.io.tmpdir"),
				PdfReport.getFileName(application));
		final String path = tempFile.getPath();
		int i = 1;
		while (tempFile.exists()) {
			tempFile = new File(path.replace(".pdf", "-" + i + ".pdf"));
			i++;
		}
		// on essayera de le supprimer à la fin, s'il n'est pas encore ouvert dans le reader
		tempFile.deleteOnExit();
		return tempFile;
	}

	final OutputStream createFileOutputStream(File tempFile) throws IOException {
		return new BufferedOutputStream(new FileOutputStream(tempFile));
	}

	@SuppressWarnings("resource")
	final PdfOtherReport createPdfOtherReport(File file) throws IOException {
		final String application = getRemoteCollector().getApplication();
		final OutputStream output = createFileOutputStream(file);
		return new PdfOtherReport(application, output);
	}

	final MButton createRefreshButton() {
		final MButton refreshButton = new MButton(getString("Actualiser"),
				ImageIconCache.getImageIcon("action_refresh.png"));
		refreshButton.setToolTipText(getString("Rafraichir") + " (F5)");
		refreshButton.setActionCommand("refresh");
		refreshButton.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW)
				.put(KeyStroke.getKeyStroke("F5"), "doRefresh");
		refreshButton.getActionMap().put("doRefresh", new AbstractAction() {
			private static final long serialVersionUID = 1L;

			@Override
			public void actionPerformed(ActionEvent e) {
				refreshButton.doClick();
			}
		});
		return refreshButton;
	}

	final MButton createPdfButton() {
		final MButton pdfButton = new MButton(getString("PDF"),
				ImageIconCache.getImageIcon("pdf.png"));
		pdfButton.setToolTipText(getString("afficher_PDF") + " (F12)");
		pdfButton.setActionCommand("pdf");
		pdfButton.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke("F12"),
				"doPdf");
		pdfButton.getActionMap().put("doPdf", new AbstractAction() {
			private static final long serialVersionUID = 1L;

			@Override
			public void actionPerformed(ActionEvent e) {
				pdfButton.doClick();
			}
		});
		return pdfButton;
	}

	final MButton createXmlJsonButton(final Serializable serializable) {
		final MButton xmlJsonButton = new MButton("", MORE_ICON);
		xmlJsonButton.setPreferredSize(new Dimension(xmlJsonButton.getPreferredSize().width - 12,
				xmlJsonButton.getPreferredSize().height));
		final Serializable mySerializable;
		if (serializable == null) {
			// un paramètre null veut dire qu'il faut instancier ici les données par défaut
			mySerializable = createDefaultSerializable();
		} else {
			mySerializable = serializable;
		}

		final ActionListener menuActionListener = new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				final TransportFormat format = TransportFormat.valueOf(e.getActionCommand());
				try {
					final String application = getRemoteCollector().getApplication();
					final File tempFile = new File(System.getProperty("java.io.tmpdir"), PdfReport
							.getFileName(application).replace(".pdf", "." + format.getCode()));
					tempFile.deleteOnExit();
					try (final OutputStream output = createFileOutputStream(tempFile)) {
						format.writeSerializableTo(mySerializable, output);
					}
					Desktop.getDesktop().open(tempFile);
				} catch (final Exception ex) {
					showException(ex);
				}
			}
		};

		xmlJsonButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				final JPopupMenu popupMenu = new JPopupMenu();
				final MMenuItem xmlMenuItem = new MMenuItem("XML",
						ImageIconCache.getImageIcon("xml.png"));
				final MMenuItem jsonMenuItem = new MMenuItem("JSON",
						ImageIconCache.getImageIcon("xml.png"));
				xmlMenuItem.setToolTipText(getString("export_xml"));
				jsonMenuItem.setToolTipText(getString("export_json"));
				xmlMenuItem.setActionCommand(TransportFormat.XML.toString());
				jsonMenuItem.setActionCommand(TransportFormat.JSON.toString());
				xmlMenuItem.addActionListener(menuActionListener);
				jsonMenuItem.addActionListener(menuActionListener);
				popupMenu.add(xmlMenuItem);
				popupMenu.add(jsonMenuItem);
				popupMenu.show(xmlJsonButton, 2, xmlJsonButton.getHeight());
			}
		});
		return xmlJsonButton;
	}

	private Serializable createDefaultSerializable() {
		final List<JavaInformations> javaInformationsList = getJavaInformationsList();
		final List<Counter> counters = getCollector().getCounters();
		final List<Serializable> serialized = new ArrayList<>(
				counters.size() + javaInformationsList.size());
		// on clone les counters avant de les sérialiser pour ne pas avoir de problèmes de concurrences d'accès
		for (final Counter counter : counters) {
			serialized.add(counter.clone());
		}
		serialized.addAll(javaInformationsList);
		return (Serializable) serialized;
	}
}
