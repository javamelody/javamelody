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

import com.opensymphony.xwork2.ActionInvocation;
import com.opensymphony.xwork2.ActionProxy;
import com.opensymphony.xwork2.interceptor.AbstractInterceptor;

import net.bull.javamelody.internal.common.LOG;
import net.bull.javamelody.internal.common.Parameters;
import net.bull.javamelody.internal.model.Counter;

/**
 * <a href='http://struts.apache.org/2.1.6/docs/interceptors.html'>Interceptor</a> Struts 2
 * pour avoir les temps moyens des actions Struts.
 * @author Emeric Vernat
 */
public final class StrutsInterceptor extends AbstractInterceptor {
	private static final long serialVersionUID = 6536441072950545240L;
	private static final Counter STRUTS_COUNTER = MonitoringProxy.getStrutsCounter();
	private static final boolean COUNTER_HIDDEN = Parameters
			.isCounterHidden(STRUTS_COUNTER.getName());
	private static final boolean DISABLED = Parameter.DISABLED.getValueAsBoolean();

	/**
	 * Constructeur.
	 */
	public StrutsInterceptor() {
		super();
		// quand cet intercepteur est utilisé, le compteur est affiché
		// sauf si le paramètre displayed-counters dit le contraire
		STRUTS_COUNTER.setDisplayed(!COUNTER_HIDDEN);
		STRUTS_COUNTER.setUsed(true);
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
			final String actionName = getRequestName(invocation);

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

	protected String getRequestName(ActionInvocation invocation) {
		// final String actionName = invocation.getInvocationContext().getName();
		final String actionName;
		final ActionProxy proxy = invocation.getProxy();
		final String action = proxy.getActionName();
		final String method = proxy.getMethod();
		final String namespace = proxy.getNamespace();
		if (method == null || "execute".equals(method)) {
			actionName = namespace + '/' + action;
		} else {
			actionName = namespace + '/' + action + '!' + method;
		}
		return actionName;
	}
}
