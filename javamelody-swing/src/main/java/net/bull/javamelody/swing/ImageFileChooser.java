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

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.image.BufferedImage;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.Iterator;

import javax.imageio.ImageIO;
import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JFileChooser;

import net.bull.javamelody.swing.print.MExtensionFileFilter;
import net.bull.javamelody.swing.util.MSwingUtilities;

/**
 * FileChooser pour une image.
 * @author Emeric Vernat
 */
public class ImageFileChooser extends JFileChooser {
	private static final long serialVersionUID = 1L;
	private static final ImageFileChooser IMAGE_FILE_CHOOSER = new ImageFileChooser();
	private final String[] extensions = { "png", "gif", "jpg", "jpeg", "bmp" };

	/**
	 * Constructeur.
	 */
	public ImageFileChooser() {
		super();
		// TODO traduction
		setFileFilter(new MExtensionFileFilter(extensions, "Fichiers images"));
		setAccessory(new ImageFilePreviewer(this));
	}

	/**
	 * Ouvre la boîte de dialogue de choix de fichier image.
	 * <br>Retourne le fichier choisi, ou null si annulé.
	 * @param parent Component
	 * @param openOrSave boolean
	 * @param fileName String
	 * @return File
	 * @throws IOException   Si format, ou taille en ko, ou taille en dimension invalide
	 */
	public static File chooseImage(Component parent, boolean openOrSave, String fileName)
			throws IOException {
		int result;
		if (fileName != null) {
			IMAGE_FILE_CHOOSER.setSelectedFile(new File(fileName));
		}
		if (openOrSave) {
			result = IMAGE_FILE_CHOOSER.showOpenDialog(parent);
		} else {
			result = IMAGE_FILE_CHOOSER.showSaveDialog(parent);
		}
		if (result == JFileChooser.APPROVE_OPTION) {
			File file = IMAGE_FILE_CHOOSER.getSelectedFile();
			file = IMAGE_FILE_CHOOSER.checkFile(file, openOrSave);
			return file;
		}
		return null;
	}

	/**
	 * Vérifie qu'un fichier est bien une image valide.
	 * @param file File
	 * @throws IOException   Si format, ou taille en ko, ou taille en pixels invalide
	 */
	public static void checkImage(File file) throws IOException {
		IMAGE_FILE_CHOOSER.checkFile(file, true);
	}

	/**
	 * En lecture, vérifie le format du fichier sélectionné.
	 * En enregistrement, vérifie le format avec l'extension et retourne un nouveau fichier sinon.
	 * @param file File
	 * @param openOrSave boolean
	 * @return File
	 * @throws IOException   Si format invalide
	 */
	private File checkFile(File file, boolean openOrSave) throws IOException {
		final String extension = getExtension(file.getName()).toLowerCase();
		if (!Arrays.asList(extensions).contains(extension)) {
			if (openOrSave) {
				final StringBuilder sb = new StringBuilder(
						"L'extension du fichier sélectionné n'est pas un format d'image accepté");
				sb.append(" (");
				for (final Iterator<String> it = Arrays.asList(extensions).iterator(); it.hasNext();) {
					sb.append(it.next());
					if (it.hasNext()) {
						sb.append(", ");
					}
				}
				sb.append(')');
				throw new IOException(sb.toString());
			}
			// si pas d'extension ou extension inconnue, on prend png par défaut
			return new File(file.getPath() + ".png");
		}

		return file;
	}

	/**
	 * Gets the extension of a filename.
	 * <p>
	 * This method returns the textual part of the filename after the last dot.
	 * There must be no directory separator after the dot.
	 * <pre>
	 * foo.txt      --> "txt"
	 * a/b/c.jpg    --> "jpg"
	 * a/b.txt/c    --> ""
	 * a/b/c        --> ""
	 * </pre>
	 * <p>
	 * The output will be the same irrespective of the machine that the code is running on.
	 * (Author Apache Commons IO 1.1)
	 *
	 * @param filename the filename to retrieve the extension of.
	 * @return the extension of the file or an empty string if none exists.
	 */
	private static String getExtension(String filename) {
		if (filename == null) {
			return null;
		}
		// The extension separator character.
		final char extensionSeparator = '.';
		// The Unix separator character.
		final char unixSeparator = '/';
		// The Windows separator character.
		final char windowsSeparator = '\\';
		final int extensionPos = filename.lastIndexOf(extensionSeparator);
		final int lastUnixPos = filename.lastIndexOf(unixSeparator);
		final int lastWindowsPos = filename.lastIndexOf(windowsSeparator);
		final int lastSeparator = Math.max(lastUnixPos, lastWindowsPos);
		final int index = lastSeparator > extensionPos ? -1 : extensionPos;
		if (index == -1) {
			return "";
		}
		return filename.substring(index + 1);
	}

	/**
	 * Composant de prévisualisation d'images dans le fileChooser.
	 */
	private static class ImageFilePreviewer extends JComponent implements PropertyChangeListener {
		private static final long serialVersionUID = 1L;
		private ImageIcon thumbnail;
		private String dimension;

		ImageFilePreviewer(JFileChooser fc) {
			super();
			setPreferredSize(new Dimension(100, 85));
			fc.addPropertyChangeListener(this);
			setBorder(BorderFactory.createLoweredBevelBorder());
		}

		/**
		 * Charge le fichier si c'est une image.
		 * @param f File
		 * @throws IOException e
		 */
		public void loadImage(File f) throws IOException {
			if (f == null || !f.exists()) {
				thumbnail = null;
			} else {
				final ImageIcon icon = readCompatibleImageIcon(f);
				if (icon == null) {
					thumbnail = null;
				} else {
					dimension = icon.getIconWidth() + " x " + icon.getIconHeight();
					int width = icon.getIconWidth();
					int height = icon.getIconHeight();
					if (height > 75) {
						width = width * 75 / height;
						height = 75;
					}
					if (width > 90) {
						height = height * 90 / width;
						width = 90;
					}
					BufferedImage image = (BufferedImage) icon.getImage();
					image = (BufferedImage) MSwingUtilities.getScaledInstance(image, width, height);
					thumbnail = new ImageIcon(image);
				}
			}
		}

		/**
		 * Retourne une imageIcon avec un contenu et une palette compatible avec la configuration vidéo en paramètre
		 * (ou avec celle de l'écran par défaut si celle en paramètre est nulle).
		 * L'imageIcon est la plus proche de la configuration vidéo et pourra être idéalement affichée sur celle-ci.
		 * @param file File
		 * @return ImageIcon
		 * @throws IOException   Exception d'entrée/sortie
		 */
		public ImageIcon readCompatibleImageIcon(File file) throws IOException {
			final BufferedImage image = ImageIO.read(file);
			if (image == null) {
				return null;
			}
			final BufferedImage compatibleImage = getGraphicsConfiguration().createCompatibleImage(
					image.getWidth(), image.getHeight());
			final Graphics g = compatibleImage.getGraphics();
			g.drawImage(image, 0, 0, null);
			g.dispose();
			return new ImageIcon(compatibleImage);
		}

		/** {@inheritDoc} */
		@Override
		public void propertyChange(PropertyChangeEvent event) {
			final String prop = event.getPropertyName();
			if (JFileChooser.SELECTED_FILE_CHANGED_PROPERTY.equals(prop) && isShowing()) {
				try {
					loadImage((File) event.getNewValue());
					repaint();
				} catch (final IOException ex) {
					MSwingUtilities.showException(ex);
				}
			}
		}

		/** {@inheritDoc} */
		@Override
		public void paint(Graphics g) {
			super.paint(g);
			if (thumbnail != null) {
				int x = getWidth() / 2 - thumbnail.getIconWidth() / 2;
				int y = getHeight() / 2 - thumbnail.getIconHeight() / 2;
				if (y < 0) {
					y = 0;
				}
				if (x < 5) {
					x = 5;
				}
				thumbnail.paintIcon(this, g, x, y);
				final FontMetrics fontMetrics = getFontMetrics(getFont());
				g.setColor(Color.BLACK);
				g.drawString(dimension, (getWidth() - fontMetrics.stringWidth(dimension)) / 2,
						(getHeight() + thumbnail.getIconHeight()) / 2 + fontMetrics.getHeight());
			}
		}
	}
}
