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
package net.bull.javamelody.util;

import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;

import javax.swing.ImageIcon;
import javax.swing.JOptionPane;
import javax.swing.UIManager;

/**
 * Classe utilitaire pour Swing.
 * @author Emeric Vernat
 */
public final class MSwingUtilities {
	private MSwingUtilities() {
		super();
	}

	/**
	 * Affiche la trace de l'exception dans la console d'erreur et affiche une boîte de dialogue pour afficher l'exception.
	 * @param throwable Throwable
	 */
	public static void showException(Throwable throwable) {
		throwable.printStackTrace(System.err);
		JOptionPane.showMessageDialog(null, throwable.toString(),
				UIManager.getString("OptionPane.messageDialogTitle"), JOptionPane.ERROR_MESSAGE);
		// on pourrait affichage une boîte de dialogue plus évoluée pour permettre d'afficher la stack trace en détail
	}

	/**
	 * Redimensionne une ImageIcon.
	 * @param icon ImageIcon
	 * @param targetWidth int
	 * @param targetHeight int
	 * @return ImageIcon
	 */
	public static ImageIcon getScaledInstance(ImageIcon icon, int targetWidth, int targetHeight) {
		return new ImageIcon(getScaledInstance(icon.getImage(), targetWidth, targetHeight));
	}

	/**
	 * Redimensionne une image.
	 * @param img Image
	 * @param targetWidth int
	 * @param targetHeight int
	 * @return Image
	 */
	public static Image getScaledInstance(Image img, int targetWidth, int targetHeight) {
		final int type = BufferedImage.TYPE_INT_ARGB;
		Image ret = img;
		final int width = ret.getWidth(null);
		final int height = ret.getHeight(null);
		if (width != targetWidth && height != targetHeight) {
			// a priori plus performant que Image.getScaledInstance
			final BufferedImage scratchImage = new BufferedImage(targetWidth, targetHeight, type);
			final Graphics2D g2 = scratchImage.createGraphics();
			g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION,
					RenderingHints.VALUE_INTERPOLATION_BILINEAR);
			g2.drawImage(ret, 0, 0, targetWidth, targetHeight, 0, 0, width, height, null);
			g2.dispose();
			ret = scratchImage;
		}
		return ret;
	}
}
