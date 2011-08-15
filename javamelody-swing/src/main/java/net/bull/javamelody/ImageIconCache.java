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

import java.net.URL;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import javax.swing.ImageIcon;

import net.bull.javamelody.util.MSwingUtilities;

/**
 * Cache d'objets ImageIcon.
 * @author Emeric Vernat
 */
public final class ImageIconCache {
	private static final Map<String, ImageIcon> CACHE = new ConcurrentHashMap<String, ImageIcon>();

	/**
	 * Constructeur.
	 * (private : pas d'instance)
	 */
	private ImageIconCache() {
		super();
	}

	/**
	 * Retourne une imageIcon à partir du cache, en la chargeant auparavant si elle n'y est pas déjà.
	 * @return ImageIcon
	 * @param fileName String
	 */
	public static ImageIcon getImageIcon(String fileName) {
		if (fileName == null) {
			return null;
		}

		if (CACHE.size() > 150) {
			CACHE.clear(); // pour éviter une fuite mémoire potentielle
		}
		ImageIcon imageIcon = CACHE.get(fileName);
		if (imageIcon == null) {
			final URL url = ImageIconCache.class.getResource(Parameters.getResourcePath(fileName));
			imageIcon = new ImageIcon(url);
			CACHE.put(fileName, imageIcon);
		}
		return imageIcon;
	}

	/**
	 * Retourne une imageIcon à partir du cache, redimensionnée.
	 * @param fileName String
	 * @param targetWidth int
	 * @param targetHeight int
	 * @return ImageIcon
	 */
	public static ImageIcon getScaledImageIcon(String fileName, int targetWidth, int targetHeight) {
		return MSwingUtilities.getScaledInstance(getImageIcon(fileName), targetWidth, targetHeight);
	}
}
