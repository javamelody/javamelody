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
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import net.bull.javamelody.internal.common.Parameters;
import net.bull.javamelody.internal.model.Counter;

/**
 * Proxy de monitoring pour tout façade ayant une interface.
 * Il est destiné à un compteur pour les statistiques d'exécutions de méthodes sur les "façades métiers"
 * qui ne sont ni EJB 3 ni Spring.
 * Il s'utilise comme ceci :
 * 		{@code facade = MonitoringProxy.createProxy(facade);}
 * @author Emeric Vernat
 */
public class MonitoringProxy implements InvocationHandler, Serializable {
	// cette classe ainsi que JdbcWrapper.DelegatingInvocationHandler sont sérialisables
	// pour qu'un proxy soit sérialisable si la façade est sérialisable
	private static final long serialVersionUID = 1882880665014391301L;

	private static final String BEANS_ICON_NAME = "beans.png";
	private static final Counter SERVICES_COUNTER = new Counter("services", BEANS_ICON_NAME,
			JdbcWrapper.SINGLETON.getSqlCounter());
	// EJB_COUNTER déclaré ici pour que l'appel dans MonitoringFilter ne déclenche pas
	// ClassNotFoundException si les classes javax.interceptor ne sont pas présentes
	private static final Counter EJB_COUNTER = new Counter("ejb", BEANS_ICON_NAME,
			JdbcWrapper.SINGLETON.getSqlCounter());
	// JPA_COUNTER déclaré ici pour que l'appel dans MonitoringFilter ne déclenche pas
	// ClassNotFoundException si les classes javax.persistence ne sont pas présentes
	private static final Counter JPA_COUNTER = new Counter("jpa", "db.png",
			JdbcWrapper.SINGLETON.getSqlCounter());
	// SPRING_COUNTER déclaré ici pour que l'appel dans MonitoringFilter ne déclenche pas
	// ClassNotFoundException si les classes Spring et AOP alliance ne sont pas présentes
	private static final Counter SPRING_COUNTER = new Counter("spring", BEANS_ICON_NAME,
			JdbcWrapper.SINGLETON.getSqlCounter());
	// GUICE_COUNTER déclaré ici pour que l'appel dans MonitoringFilter ne déclenche pas
	// ClassNotFoundException si les classes AOP alliance ne sont pas présentes
	private static final Counter GUICE_COUNTER = new Counter("guice", BEANS_ICON_NAME,
			JdbcWrapper.SINGLETON.getSqlCounter());
	// STRUTS_COUNTER déclaré ici pour que l'appel dans MonitoringFilter ne déclenche pas
	// ClassNotFoundException si les classes xwork de Struts 2 ne sont pas présentes
	private static final Counter STRUTS_COUNTER = new Counter(Counter.STRUTS_COUNTER_NAME,
			"struts.png", JdbcWrapper.SINGLETON.getSqlCounter());
	// JSF_COUNTER déclaré ici pour que l'appel dans MonitoringFilter ne déclenche pas
	// ClassNotFoundException si les classes com.sun.faces de JSF ne sont pas présentes
	private static final Counter JSF_COUNTER = new Counter(Counter.JSF_COUNTER_NAME, "jsp.png",
			JdbcWrapper.SINGLETON.getSqlCounter());

	private static final boolean COUNTER_HIDDEN = Parameters
			.isCounterHidden(SERVICES_COUNTER.getName());
	private static final boolean DISABLED = Parameter.DISABLED.getValueAsBoolean();
	@SuppressWarnings("all")
	private final Object facade;
	private final String name;

	/**
	 * Constructeur privé : instanciation pour méthode createProxy ci-dessous.
	 * @param facade Object
	 */
	protected MonitoringProxy(Object facade) {
		this(facade, null);
	}

	/**
	 * Constructeur privé : instanciation pour méthode createProxy ci-dessous.
	 * @param facade Object
	 * @param name override of the interface name in the statistics
	 */
	protected MonitoringProxy(Object facade, String name) {
		super();
		this.facade = facade;
		// quand cet intercepteur est utilisé, le compteur est affiché
		// sauf si le paramètre displayed-counters dit le contraire
		SERVICES_COUNTER.setDisplayed(!COUNTER_HIDDEN);
		SERVICES_COUNTER.setUsed(true);
		this.name = name;
	}

	/**
	 * Création d'un proxy de monitoring pour une façade.
	 * @param <T> Type de la façade (une interface en général).
	 * @param facade Instance de la façade
	 * @return Proxy de la façade
	 */
	public static <T> T createProxy(T facade) {
		return createProxy(facade, new MonitoringProxy(facade));
	}

	/**
	 * Création d'un proxy de monitoring pour une façade, en spécifiant le nom qui sera affiché dans le monitoring.
	 * @param <T> Type de la façade (une interface en général).
	 * @param facade Instance de la façade
	 * @param name override of the interface name in the statistics
	 * @return Proxy de la façade
	 */
	public static <T> T createProxy(T facade, String name) {
		return createProxy(facade, new MonitoringProxy(facade, name));
	}

	static Counter getServicesCounter() {
		return SERVICES_COUNTER;
	}

	static Counter getEjbCounter() {
		return EJB_COUNTER;
	}

	static Counter getJpaCounter() {
		return JPA_COUNTER;
	}

	static Counter getSpringCounter() {
		return SPRING_COUNTER;
	}

	static Counter getGuiceCounter() {
		return GUICE_COUNTER;
	}

	static Counter getStrutsCounter() {
		return STRUTS_COUNTER;
	}

	static Counter getJsfCounter() {
		return JSF_COUNTER;
	}

	/**
	 * Intercepte une exécution de méthode sur une façade.
	 * @param proxy Object
	 * @param method Method
	 * @param args Object[]
	 * @return Object
	 * @throws Throwable t
	 */
	@Override
	public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
		if (DISABLED || !SERVICES_COUNTER.isDisplayed()) {
			return method.invoke(facade, args);
		}
		// nom identifiant la requête
		final String requestName = getRequestName(method);

		return invokeTarget(facade, method, args, requestName);
	}

	/**
	 * Invoke target.
	 * @param target Instance to call
	 * @param method Method to call
	 * @param args Method arguments
	 * @param requestName Request name to display
	 * @return Result
	 * @throws IllegalAccessException e
	 * @throws InvocationTargetException e
	 */
	public static Object invokeTarget(Object target, Method method, Object[] args,
			final String requestName) throws IllegalAccessException, InvocationTargetException {
		boolean systemError = false;
		try {
			SERVICES_COUNTER.bindContextIncludingCpu(requestName);
			return method.invoke(target, args);
		} catch (final InvocationTargetException e) {
			if (e.getCause() instanceof Error) {
				// on catche Error pour avoir les erreurs systèmes
				// mais pas Exception qui sont fonctionnelles en général
				systemError = true;
			}
			throw e;
		} finally {
			// on enregistre la requête dans les statistiques
			SERVICES_COUNTER.addRequestForCurrentContext(systemError);
		}
	}

	protected String getRequestName(Method method) {
		final String requestName;
		if (name == null) {
			requestName = method.getDeclaringClass().getSimpleName() + '.' + method.getName();
		} else {
			requestName = name + '.' + method.getName();
		}
		return requestName;
	}

	protected String getName() {
		return name;
	}

	protected static <T> T createProxy(T facade, MonitoringProxy monitoringProxy) {
		return JdbcWrapper.createProxy(facade, monitoringProxy);
	}
}
