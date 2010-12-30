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

import com.opensymphony.xwork2.ActionInvocation;
import com.opensymphony.xwork2.interceptor.AbstractInterceptor;

/**
 * Interceptor Struts 2 pour avoir les temps moyens des actions Struts.
 * {@link StrutsInterceptor "http://struts.apache.org/2.1.6/docs/interceptors.html"}
 * @author Emeric Vernat
 */
public final class StrutsInterceptor extends AbstractInterceptor {
	private static final long serialVersionUID = 6536441072950545240L;
	private static final Counter STRUTS_COUNTER = MonitoringProxy.getStrutsCounter();
	private static final boolean COUNTER_HIDDEN = Parameters.isCounterHidden(STRUTS_COUNTER
			.getName());
	private static final boolean DISABLED = Boolean.parseBoolean(Parameters
			.getParameter(Parameter.DISABLED));

	/**
	 * Constructeur.
	 */
	public StrutsInterceptor() {
		super();
		// quand cet intercepteur est utilisé, le compteur est affiché
		// sauf si le paramètre displayed-counters dit le contraire
		STRUTS_COUNTER.setDisplayed(!COUNTER_HIDDEN);
		LOG.debug("struts interceptor initialized");
	}

	/**
	 * Intercepte une exécution d'action struts.
	 * @param invocation ActionInvocation
	 * @return String
	 * @throws Exception e
	 */
	@Override
	public String intercept(ActionInvocation invocation) throws Exception { // NOPMD
		// cette méthode est appelée par struts
		if (DISABLED || !STRUTS_COUNTER.isDisplayed()) {
			return invocation.invoke();
		}
		boolean systemError = false;
		try {
			// Requested action name.
			final String actionName = invocation.getInvocationContext().getName();

			STRUTS_COUNTER.bindContextIncludingCpu(actionName);
			return invocation.invoke();
		} catch (final Error e) {
			// on catche Error pour avoir les erreurs systèmes
			// mais pas Exception qui sont fonctionnelles en général
			systemError = true;
			throw e;
		} finally {
			// on enregistre la requête dans les statistiques
			STRUTS_COUNTER.addRequestForCurrentContext(systemError);
		}
	}
}
