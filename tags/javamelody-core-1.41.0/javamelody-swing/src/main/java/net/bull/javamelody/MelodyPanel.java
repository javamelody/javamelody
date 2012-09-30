/*
 * Copyright 2008-2012 by Emeric Vernat
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

import javax.swing.AbstractAction;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.KeyStroke;

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

	final File createTempFileForPdf() {
		final String application = getRemoteCollector().getApplication();
		final File tempFile = new File(System.getProperty("java.io.tmpdir"),
				PdfReport.getFileName(application));
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
		final MButton refreshButton = new MButton(I18N.getString("Actualiser"),
				ImageIconCache.getImageIcon("action_refresh.png"));
		refreshButton.setToolTipText(I18N.getString("Rafraichir") + " (F5)");
		refreshButton.setActionCommand("refresh");
		refreshButton.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(
				KeyStroke.getKeyStroke("F5"), "doRefresh");
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
		final MButton pdfButton = new MButton(I18N.getString("PDF"),
				ImageIconCache.getImageIcon("pdf.png"));
		pdfButton.setToolTipText(I18N.getString("afficher_PDF") + " (F12)");
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
						format.writeSerializableTo(serializable, output);
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
				final MMenuItem xmlMenuItem = new MMenuItem("XML", ImageIconCache
						.getImageIcon("xml.png"));
				final MMenuItem jsonMenuItem = new MMenuItem("JSON", ImageIconCache
						.getImageIcon("xml.png"));
				xmlMenuItem.setToolTipText(I18N.getString("export_xml"));
				jsonMenuItem.setToolTipText(I18N.getString("export_json"));
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
}
