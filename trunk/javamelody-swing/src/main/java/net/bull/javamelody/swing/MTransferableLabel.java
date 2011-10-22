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
package net.bull.javamelody.swing;

import java.awt.Graphics2D;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.event.MouseEvent;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;

import javax.imageio.ImageIO;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.TransferHandler;
import javax.swing.event.MouseInputAdapter;

/**
 * Label dont l'image est transférable vers un fichier, un navigateur, paint ou un traitement de texte.
 * @author Emeric Vernat
 */
public class MTransferableLabel extends JLabel {
	private static final long serialVersionUID = 1L;

	class FileTransfer extends TransferHandler {
		private static final long serialVersionUID = 1L;

		@Override
		public boolean canImport(JComponent comp, DataFlavor[] transferFlavors) {
			return false;
		}

		/** {@inheritDoc} */
		@Override
		public int getSourceActions(JComponent c) {
			return COPY;
		}

		/** {@inheritDoc} */
		@Override
		protected Transferable createTransferable(JComponent c) {
			return createFileTransferable();
		}

		// si le fichier est droppé vers l'explorateur windows, on pourrait supprimer
		// le fichier temporaire, mais si le fichier est droppé vers une application
		// alors cette application ouvre le fichier en place et sa suppression se ferait
		// avant que l'application puisse lire le fichier
		//		    @Override
		//		    /** {@inheritDoc} */
		//		    protected void exportDone(JComponent source, Transferable data, int action) {
		//		      FileSelection fileSelection = (FileSelection) data;
		//		      File tmpFile = fileSelection.get(0);
		//		      if (tmpFile.exists()) {
		//		        tmpFile.delete();
		//		      }
		//		    }
	}

	private static final class MouseHandler extends MouseInputAdapter {
		private MouseEvent firstMouseEvent;

		MouseHandler() {
			super();
		}

		@Override
		public void mousePressed(MouseEvent e) {
			firstMouseEvent = e;
			e.consume();
		}

		@Override
		public void mouseDragged(MouseEvent e) {
			if (firstMouseEvent != null) {
				e.consume();
				final int dx = Math.abs(e.getX() - firstMouseEvent.getX());
				final int dy = Math.abs(e.getY() - firstMouseEvent.getY());
				if (dx > 5 || dy > 5) {
					final JComponent c = (JComponent) e.getSource();
					final TransferHandler handler = c.getTransferHandler();
					handler.exportAsDrag(c, firstMouseEvent, TransferHandler.COPY);
					firstMouseEvent = null;
				}
			}
		}
	}

	/**
	 * Constructeur.
	 */
	public MTransferableLabel() {
		super();
		init();
	}

	/**
	 * Constructeur.
	 * @param image Icon
	 */
	public MTransferableLabel(Icon image) {
		super(image);
		init();
	}

	/**
	 * Constructeur.
	 * @param text String
	 */
	public MTransferableLabel(String text) {
		super(text);
		init();
	}

	/**
	 * Affiche la boîte de dialogue d'export de l'image.
	 * @throws IOException e
	 */
	public void export() throws IOException {
		final byte[] imageData = getImageData();
		if (imageData != null) {
			final File file = ImageFileChooser.chooseImage(this, false, getName() + ".png");
			if (file != null) {
				final FileOutputStream fileOutputStream = new FileOutputStream(file);
				try {
					fileOutputStream.write(imageData);
				} finally {
					fileOutputStream.close();
				}
			}
		}
	}

	private void init() {
		setTransferHandler(new FileTransfer());
		final MouseInputAdapter mouseHandler = new MouseHandler();
		addMouseMotionListener(mouseHandler);
		addMouseListener(mouseHandler);
	}

	FileTransferable createFileTransferable() {
		final byte[] imageData = getImageData();
		if (imageData != null) {
			return new FileTransferable(getName() + ".png", imageData);
		}
		return null;
	}

	private byte[] getImageData() {
		if (getIcon() instanceof ImageIcon) {
			final ImageIcon icon = (ImageIcon) getIcon();
			final BufferedImage scratchImage = convertImageIconToBufferedImage(icon);
			return convertBufferedImageToImageData(scratchImage, "png");
		}
		return null;
	}

	private BufferedImage convertImageIconToBufferedImage(ImageIcon icon) {
		final int type = BufferedImage.TYPE_INT_ARGB;
		final int width = icon.getIconWidth();
		final int height = icon.getIconHeight();
		final BufferedImage scratchImage = new BufferedImage(width, height, type);
		final Graphics2D g2 = scratchImage.createGraphics();
		g2.drawImage(icon.getImage(), 0, 0, null);
		g2.dispose();
		return scratchImage;
	}

	private byte[] convertBufferedImageToImageData(BufferedImage scratchImage, String format) {
		final ByteArrayOutputStream output = new ByteArrayOutputStream();
		try {
			ImageIO.write(scratchImage, format, output);
		} catch (final IOException e) {
			// ne devrait pas arriver
			throw new IllegalStateException(e);
		}
		return output.toByteArray();
	}
}
