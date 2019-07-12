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

import javax.faces.component.ActionSource2;
import javax.faces.event.ActionEvent;
import javax.faces.event.ActionListener;

import net.bull.javamelody.internal.common.LOG;
import net.bull.javamelody.internal.common.Parameters;
import net.bull.javamelody.internal.model.Counter;

/**
 * {@link ActionListener} JSF RI (Mojarra) pour avoir les temps moyens des actions JSF.
 * @author Emeric Vernat
 */
public class JsfActionListener implements ActionListener {
	private static final Counter JSF_COUNTER = MonitoringProxy.getJsfCounter();
	private static final boolean COUNTER_HIDDEN = Parameters.isCounterHidden(JSF_COUNTER.getName());
	private static final boolean DISABLED = Parameter.DISABLED.getValueAsBoolean();
	private final ActionListener delegateActionListener;

	/**
	 * Constructeur.
	 * @param delegateActionListener ActionListener
	 */
	public JsfActionListener(ActionListener delegateActionListener) {
		super();
		// quand cet ActionListener est utilisé, le compteur est affiché
		// sauf si le paramètre displayed-counters dit le contraire
		JSF_COUNTER.setDisplayed(!COUNTER_HIDDEN);
		JSF_COUNTER.setUsed(true);
		LOG.debug("jsf action listener initialized");
		this.delegateActionListener = delegateActionListener;
	}

	/** {@inheritDoc} */
	@Override
	public void processAction(ActionEvent event) { // throws FacesException
		// cette méthode est appelée par JSF RI (Mojarra)
		if (DISABLED || !JSF_COUNTER.isDisplayed()) {
			delegateActionListener.processAction(event);
			return;
		}

		boolean systemError = false;
		try {
			final String actionName = getRequestName(event);

			JSF_COUNTER.bindContextIncludingCpu(actionName);

			delegateActionListener.processAction(event);
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

	protected String getRequestName(ActionEvent event) {
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
		return actionName;
	}
}
