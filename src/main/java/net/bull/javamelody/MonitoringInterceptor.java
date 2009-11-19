/*
 * Copyright 2008-2009 by Emeric Vernat, Bull
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

import java.lang.reflect.Method;

import javax.interceptor.AroundInvoke;
import javax.interceptor.InvocationContext;

/**
 * Intercepteur pour EJB 3 (Java EE 5).
 * Il est destiné à un compteur pour les statistiques d'exécutions de méthodes sur les "façades métiers"
 * ( @Stateless, @Stateful ou @MessageDriven ).
 * Il peut être paramétré dans le fichier ejb-jar.xml pour certains ejb ou pour tous les ejb,
 * ou alors par l'annotation @Interceptors dans les sources java des implémentations d'ejb.
 * @author Emeric Vernat
 */
public class MonitoringInterceptor {
	private static final Counter EJB_COUNTER = new Counter("ejb", "beans.png",
			JdbcWrapper.SINGLETON.getSqlCounter());
	// SPRING_COUNTER déclaré ici pour que l'appel dans MonitoringFilter ne déclenche pas
	// ClassNotFoundException si les classes Spring et AOP alliance ne sont pas présentes
	private static final Counter SPRING_COUNTER = new Counter("spring", "beans.png",
			JdbcWrapper.SINGLETON.getSqlCounter());
	private static final boolean DISABLED = Boolean.parseBoolean(Parameters
			.getParameter(Parameter.DISABLED));

	static Counter getEjbCounter() {
		return EJB_COUNTER;
	}

	static Counter getSpringCounter() {
		return SPRING_COUNTER;
	}

	/**
	 * Intercepte une exécution de méthode sur un ejb.
	 * @param context InvocationContext
	 * @return Object
	 * @throws Exception e
	 */
	@AroundInvoke
	public Object intercept(InvocationContext context) throws Exception { // NOPMD
		if (DISABLED || !EJB_COUNTER.isDisplayed()) {
			return context.proceed();
		}
		// cette méthode est appelée par le conteneur ejb grâce à l'annotation AroundInvoke
		final long start = System.currentTimeMillis();
		final long startCpuTime = ThreadInformations.getCurrentThreadCpuTime();
		// nom identifiant la requête
		final Method method = context.getMethod();
		final String requestName = method.getDeclaringClass().getSimpleName() + '.'
				+ method.getName();

		boolean systemError = false;
		try {
			EJB_COUNTER.bindContext(requestName, requestName, null, startCpuTime);
			return context.proceed();
		} catch (final Error e) {
			// on catche Error pour avoir les erreurs systèmes
			// mais pas Exception qui sont fonctionnelles en général
			systemError = true;
			throw e;
		} finally {
			final long duration = Math.max(System.currentTimeMillis() - start, 0);
			final long cpuUsedMillis = (ThreadInformations.getCurrentThreadCpuTime() - startCpuTime) / 1000000;

			// on enregistre la requête dans les statistiques
			EJB_COUNTER.addRequest(requestName, duration, cpuUsedMillis, systemError, -1);
		}
	}
}
