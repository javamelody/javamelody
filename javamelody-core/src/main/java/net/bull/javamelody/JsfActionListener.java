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

import javax.faces.component.ActionSource2;
import javax.faces.context.FacesContext;
import javax.faces.event.ActionEvent;

import com.sun.faces.application.ActionListenerImpl;

/**
 * ActionListener JSF RI (Mojarra) pour avoir les temps moyens des actions JSF.
 * @author Emeric Vernat
 */
public class JsfActionListener extends ActionListenerImpl {
	private static final Counter JSF_COUNTER = MonitoringProxy.getJsfCounter();
	private static final boolean COUNTER_HIDDEN = Parameters.isCounterHidden(JSF_COUNTER.getName());
	private static final boolean DISABLED = Boolean.parseBoolean(Parameters
			.getParameter(Parameter.DISABLED));

	/**
	 * Constructeur.
	 */
	public JsfActionListener() {
		super();
		// quand cet ActionListener est utilisé, le compteur est affiché
		// sauf si le paramètre displayed-counters dit le contraire
		JSF_COUNTER.setDisplayed(!COUNTER_HIDDEN);
		JSF_COUNTER.setUsed(true);
		LOG.debug("jsf action listener initialized");
	}

	static void initJsfActionListener() {
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

	/** {@inheritDoc} */
	@Override
	public void processAction(ActionEvent event) { // throws FacesException
		// cette méthode est appelée par JSF RI (Mojarra)
		if (DISABLED || !JSF_COUNTER.isDisplayed()) {
			super.processAction(event);
		}

		boolean systemError = false;
		try {
			final String actionName;
			if (event.getComponent() instanceof ActionSource2) {
				// actionSource est une UICommand en général
				final ActionSource2 actionSource = (ActionSource2) event.getComponent();
				if (actionSource.getActionExpression() != null) {
					actionName = actionSource.getActionExpression().getExpressionString();
				} else {
					actionName = actionSource.getClass().getName();
				}
			} else {
				actionName = event.getComponent().getClass().getName();
			}

			JSF_COUNTER.bindContextIncludingCpu(actionName);

			super.processAction(event);

		} catch (final Error e) {
			// on catche Error pour avoir les erreurs systèmes
			// mais pas Exception qui sont fonctionnelles en général
			systemError = true;
			throw e;
		} finally {
			// on enregistre la requête dans les statistiques
			JSF_COUNTER.addRequestForCurrentContext(systemError);
		}
	}
}
