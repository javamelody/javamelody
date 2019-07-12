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

import java.io.Serializable;
import java.lang.reflect.Method;

import javax.ejb.MessageDriven;
import javax.ejb.Stateful;
import javax.ejb.Stateless;
import javax.interceptor.AroundInvoke;
import javax.interceptor.InvocationContext;

import net.bull.javamelody.internal.common.LOG;
import net.bull.javamelody.internal.common.Parameters;
import net.bull.javamelody.internal.model.Counter;

/**
 * Intercepteur pour EJB 3 (Java EE 5+).
 * Il est destiné à un compteur pour les statistiques d'exécutions de méthodes sur les "façades métiers"
 * ( @{@link Stateless}, @{@link Stateful} ou @{@link MessageDriven} ).
 * Il peut être paramétré dans le fichier ejb-jar.xml pour certains ejb ou pour tous les ejb,
 * ou alors par l'annotation @{@link javax.interceptor.Interceptors} dans les sources java des implémentations d'ejb.
 * @author Emeric Vernat
 */
public class MonitoringInterceptor implements Serializable {
	// MonitoringInterceptor doit être Serializable si ejb a @Stateful (cf issue 137)
	private static final long serialVersionUID = 1L;
	private static final Counter EJB_COUNTER = MonitoringProxy.getEjbCounter();
	private static final boolean COUNTER_HIDDEN = Parameters.isCounterHidden(EJB_COUNTER.getName());
	private static final boolean DISABLED = Parameter.DISABLED.getValueAsBoolean();

	/**
	 * Constructeur.
	 */
	public MonitoringInterceptor() {
		super();
		// quand cet intercepteur est utilisé, le compteur est affiché
		// sauf si le paramètre displayed-counters dit le contraire
		EJB_COUNTER.setDisplayed(!COUNTER_HIDDEN);
		EJB_COUNTER.setUsed(true);
		LOG.debug("ejb interceptor initialized");
	}

	/**
	 * Intercepte une exécution de méthode sur un ejb.
	 * @param context InvocationContext
	 * @return Object
	 * @throws Exception e
	 */
	@AroundInvoke
	public Object intercept(InvocationContext context) throws Exception { // NOPMD
		// cette méthode est appelée par le conteneur ejb grâce à l'annotation AroundInvoke
		if (DISABLED || !EJB_COUNTER.isDisplayed()) {
			return context.proceed();
		}
		// nom identifiant la requête
		final String requestName = getRequestName(context);

		boolean systemError = false;
		try {
			EJB_COUNTER.bindContextIncludingCpu(requestName);
			return context.proceed();
		} catch (final Error e) {
			// on catche Error pour avoir les erreurs systèmes
			// mais pas Exception qui sont fonctionnelles en général
			systemError = true;
			throw e;
		} finally {
			// on enregistre la requête dans les statistiques
			EJB_COUNTER.addRequestForCurrentContext(systemError);
		}
	}

	/**
	 * Determine request name for an invocation context.
	 *
	 * @param context the invocation context (not null)
	 * @return the request name for this invocation
	 */
	protected String getRequestName(InvocationContext context) {
		final Method method = context.getMethod();
		return method.getDeclaringClass().getSimpleName() + '.' + method.getName();
	}
}
