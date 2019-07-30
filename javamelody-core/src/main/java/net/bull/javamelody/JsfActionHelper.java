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

import javax.faces.context.FacesContext;
import javax.faces.event.ActionListener;

import net.bull.javamelody.internal.common.LOG;

/**
 * Helper pour l'{@link ActionListener} JSF RI (Mojarra).
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
		try {
			final FacesContext facesContext = FacesContext.getCurrentInstance();
			if (facesContext != null && facesContext.getApplication() != null) {
				// ceci est a priori équivalent à l'ajout d'un action-listener dans WEB-INF/faces-config.xml de l'application :
				// <application><action-listener>net.bull.javamelody.JsfActionListener</action-listener></application>
				// et on ne peut pas avoir un fichier META-INF/faces-config.xml dans le jar de javamelody avec cet action-listener
				// car dans Apache MyFaces, cela ferait certainement une ClassNotFoundException rendant javamelody inutilisable
				final ActionListener delegateActionListener = facesContext.getApplication()
						.getActionListener();
				final JsfActionListener jsfActionListener = new JsfActionListener(
						delegateActionListener);
				facesContext.getApplication().setActionListener(jsfActionListener);
			}
		} catch (final Exception e) {
			// issue 204: initialisation du JsfActionListener échouée, tant pis, il n'y aura pas les statistiques pour JSF.
			// no stack-trace, because in JBoss 7 because in some cases the class com.sun.faces.application.ActionListenerImpl is available but the ApplicationFactory isn't (issue 393)
			LOG.info("initialization of jsf action listener failed, skipping");
		}
	}
}
