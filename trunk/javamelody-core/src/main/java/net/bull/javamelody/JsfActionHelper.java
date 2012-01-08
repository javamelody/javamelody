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

import javax.faces.context.FacesContext;

/**
 * Helper pour l'ActionListener JSF RI (Mojarra).
 * @author Emeric Vernat
 */
final class JsfActionHelper {

	/**
	 * Constructeur.
	 */
	private JsfActionHelper() {
		super();
	}

	static void initJsfActionListener() {
		// cette indirection entre FilterContext et JsfActionListener est probablement nécessaire pour la JVM IBM J9
		// afin de ne pas dépendre des classes FacesContext et ActionListenerImpl et afin de ne pas avoir de ClassNotFound dans J9
		final FacesContext facesContext = FacesContext.getCurrentInstance();
		if (facesContext != null && facesContext.getApplication() != null) {
			// ceci est a priori équivalent à l'ajout d'un action-listener dans WEB-INF/faces-config.xml de l'application :
			// <application><action-listener>net.bull.javamelody.JsfActionListener</action-listener></application>
			// et on ne peut pas avoir un fichier META-INF/faces-config.xml dans le jar de javamelody avec cet action-listener
			// car dans Apache MyFaces, cela ferait certainement une ClassNotFoundException rendant javamelody inutilisable
			final JsfActionListener jsfActionListener = new JsfActionListener();
			facesContext.getApplication().setActionListener(jsfActionListener);
		}
	}
}
