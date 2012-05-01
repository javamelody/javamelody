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

import java.io.Serializable;

import org.aopalliance.intercept.MethodInterceptor;
import org.aopalliance.intercept.MethodInvocation;
import org.springframework.aop.support.AopUtils;

/**
 * Method interceptor that measures the duration of the intercepted call.
 *
 * Inspired by Erik van Oosten (Java Simon, Licence LGPL)
 * @author Emeric Vernat
 */
public class MonitoringSpringInterceptor implements MethodInterceptor, Serializable {
	private static final long serialVersionUID = -6594338383847482623L;
	private static final Counter SPRING_COUNTER = MonitoringProxy.getSpringCounter();
	private static final boolean COUNTER_HIDDEN = Parameters.isCounterHidden(SPRING_COUNTER
			.getName());
	private static final boolean DISABLED = Boolean.parseBoolean(Parameters
			.getParameter(Parameter.DISABLED));

	/**
	 * Constructeur.
	 */
	public MonitoringSpringInterceptor() {
		super();
		// quand cet intercepteur est utilisé, le compteur est affiché
		// sauf si le paramètre displayed-counters dit le contraire
		SPRING_COUNTER.setDisplayed(!COUNTER_HIDDEN);
		// setUsed(true) nécessaire ici si le contexte spring est initialisé avant FilterContext
		// sinon les statistiques spring ne sont pas affichées
		SPRING_COUNTER.setUsed(true);
		LOG.debug("spring interceptor initialized");
	}

	/**
	 * Performs method invocation.
	 *
	 * @param invocation method invocation
	 * @return return object from the method
	 * @throws Throwable anything thrown by the method
	 */
	@Override
	public Object invoke(MethodInvocation invocation) throws Throwable {
		// cette méthode est appelée par spring aop
		if (DISABLED || !SPRING_COUNTER.isDisplayed()) {
			return invocation.proceed();
		}
		// nom identifiant la requête
		final String requestName = getRequestName(invocation);

		boolean systemError = false;
		try {
			SPRING_COUNTER.bindContextIncludingCpu(requestName);
			return invocation.proceed();
		} catch (final Error e) {
			// on catche Error pour avoir les erreurs systèmes
			// mais pas Exception qui sont fonctionnelles en général
			systemError = true;
			throw e;
		} finally {
			// on enregistre la requête dans les statistiques
			SPRING_COUNTER.addRequestForCurrentContext(systemError);
		}
	}

	/**
	 * Determine request name for a method invocation.
	 *
	 * @param invocation the method invocation (not null)
	 * @return the request name for this invocation
	 */
	protected String getRequestName(MethodInvocation invocation) {
		final String classPart = getClassPart(invocation);
		final String methodPart = getMethodPart(invocation);
		return classPart + '.' + methodPart;
	}

	private static String getClassPart(MethodInvocation invocation) {
		// si guice et pas Spring, alors remplacer AopUtils.getTargetClass() par getMethod().getDeclaringClass()
		// http://ninomartinez.wordpress.com/2010/05/14/guice-caching-interceptors/
		// (faire exemple avec un interceptor static)
		final Class<?> targetClass = AopUtils.getTargetClass(invocation.getThis());
		final MonitoredWithSpring classAnnotation = targetClass
				.getAnnotation(MonitoredWithSpring.class);
		if (classAnnotation == null || classAnnotation.name() == null
				|| classAnnotation.name().length() == 0) {
			final Class<?> declaringClass = invocation.getMethod().getDeclaringClass();
			final MonitoredWithSpring declaringClassAnnotation = declaringClass
					.getAnnotation(MonitoredWithSpring.class);
			if (declaringClassAnnotation == null || declaringClassAnnotation.name() == null
					|| declaringClassAnnotation.name().length() == 0) {
				return targetClass.getSimpleName();
			}
			return declaringClassAnnotation.name();
		}
		return classAnnotation.name();
	}

	private static String getMethodPart(MethodInvocation invocation) {
		final MonitoredWithSpring methodAnnotation = invocation.getMethod().getAnnotation(
				MonitoredWithSpring.class);
		if (methodAnnotation == null || methodAnnotation.name() == null
				|| methodAnnotation.name().length() == 0) {
			return invocation.getMethod().getName();
		}
		return methodAnnotation.name();
	}
}
