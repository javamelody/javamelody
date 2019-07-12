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

import java.net.URL;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import javax.swing.ImageIcon;

import net.bull.javamelody.internal.common.Parameters;
import net.bull.javamelody.swing.util.MSwingUtilities;

/**
 * Cache d'objets ImageIcon.
 * @author Emeric Vernat
 */
public final class ImageIconCache {
	private static final Map<String, ImageIcon> CACHE = new ConcurrentHashMap<>();

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
