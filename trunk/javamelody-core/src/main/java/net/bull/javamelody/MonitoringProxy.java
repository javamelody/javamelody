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

import java.io.Serializable;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;

/**
 * Proxy de monitoring pour tout façade ayant une interface.
 * Il est destiné à un compteur pour les statistiques d'exécutions de méthodes sur les "façades métiers"
 * qui ne sont ni EJB 3 ni Spring.
 * Il s'utilise comme ceci (penser aussi à activer le compteur "services" comme celui pour "ejb") :
 * 		facade = MonitoringProxy.createProxy(facade);
 * @author Emeric Vernat
 */
public final class MonitoringProxy implements InvocationHandler, Serializable {
	// cette classe ainsi que JdbcWrapper.DelegatingInvocationHandler sont sérialisables
	// pour qu'un proxy soit sérialisable si la façade est sérialisable
	private static final long serialVersionUID = 1882880665014391301L;

	private static final Counter SERVICES_COUNTER = new Counter("services", "beans.png",
			JdbcWrapper.SINGLETON.getSqlCounter());
	// EJB_COUNTER déclaré ici pour que l'appel dans MonitoringFilter ne déclenche pas
	// ClassNotFoundException si les classes javax.interceptor ne sont pas présentes
	private static final Counter EJB_COUNTER = new Counter("ejb", "beans.png",
			JdbcWrapper.SINGLETON.getSqlCounter());
	// SPRING_COUNTER déclaré ici pour que l'appel dans MonitoringFilter ne déclenche pas
	// ClassNotFoundException si les classes Spring et AOP alliance ne sont pas présentes
	private static final Counter SPRING_COUNTER = new Counter("spring", "beans.png",
			JdbcWrapper.SINGLETON.getSqlCounter());

	private static final boolean COUNTER_HIDDEN = Parameters.isCounterHidden(SERVICES_COUNTER
			.getName());
	private static final boolean DISABLED = Boolean.parseBoolean(Parameters
			.getParameter(Parameter.DISABLED));
	@SuppressWarnings("all")
	private final Object facade;

	/**
	 * Constructeur privé : instanciation pour méthode createProxy ci-dessous.
	 * @param facade Object
	 */
	private MonitoringProxy(Object facade) {
		super();
		this.facade = facade;
		// quand cet intercepteur est utilisé, le compteur est affiché
		// sauf si le paramètre displayed-counters dit le contraire
		SERVICES_COUNTER.setDisplayed(!COUNTER_HIDDEN);
	}

	/**
	 *
	 * @param <T> Type de la façade (une interface en général).
	 * @param facade Instance de la façade
	 * @return Proxy de la façade
	 */
	public static <T> T createProxy(T facade) {
		return JdbcWrapper.createProxy(facade, new MonitoringProxy(facade));
	}

	static Counter getServicesCounter() {
		return SERVICES_COUNTER;
	}

	static Counter getEjbCounter() {
		return EJB_COUNTER;
	}

	static Counter getSpringCounter() {
		return SPRING_COUNTER;
	}

	/**
	 * Intercepte une exécution de méthode sur une façade.
	 * @param proxy Object
	 * @param method Method
	 * @param args Object[]
	 * @return Object
	 * @throws Throwable t
	 */
	public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
		if (DISABLED || !SERVICES_COUNTER.isDisplayed()) {
			return method.invoke(facade, args);
		}
		// nom identifiant la requête
		final String requestName = method.getDeclaringClass().getSimpleName() + '.'
				+ method.getName();

		boolean systemError = false;
		try {
			SERVICES_COUNTER.bindContextIncludingCpu(requestName);
			return method.invoke(facade, args);
		} catch (final Error e) {
			// on catche Error pour avoir les erreurs systèmes
			// mais pas Exception qui sont fonctionnelles en général
			systemError = true;
			throw e;
		} finally {
			// on enregistre la requête dans les statistiques
			SERVICES_COUNTER.addRequestForCurrentContext(systemError);
		}
	}
}
