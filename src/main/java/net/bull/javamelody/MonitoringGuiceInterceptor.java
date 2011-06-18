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

import org.aopalliance.intercept.MethodInterceptor;
import org.aopalliance.intercept.MethodInvocation;

/**
 * Method interceptor that measures the duration of the intercepted call using Google Guice.
 *
 * Inspired by Erik van Oosten (Java Simon, Licence LGPL)
 * @author Emeric Vernat
 */
public class MonitoringGuiceInterceptor implements MethodInterceptor, Serializable {
	private static final long serialVersionUID = -6594338383847482623L;
	private static final Counter GUICE_COUNTER = MonitoringProxy.getGuiceCounter();
	private static final boolean COUNTER_HIDDEN = Parameters.isCounterHidden(GUICE_COUNTER
			.getName());
	private static final boolean DISABLED = Boolean.parseBoolean(Parameters
			.getParameter(Parameter.DISABLED));

	/**
	 * Constructeur.
	 */
	public MonitoringGuiceInterceptor() {
		super();
		// quand cet intercepteur est utilisé, le compteur est affiché
		// sauf si le paramètre displayed-counters dit le contraire
		GUICE_COUNTER.setDisplayed(!COUNTER_HIDDEN);
		GUICE_COUNTER.setUsed(true);
		LOG.debug("guice interceptor initialized");
	}

	/**
	 * Performs method invocation.
	 *
	 * @param invocation method invocation
	 * @return return object from the method
	 * @throws Throwable anything thrown by the method
	 */
	public Object invoke(MethodInvocation invocation) throws Throwable {
		// cette méthode est appelée par guice aop
		if (DISABLED || !GUICE_COUNTER.isDisplayed()) {
			return invocation.proceed();
		}
		// nom identifiant la requête
		final String requestName = getMonitorName(invocation);

		boolean systemError = false;
		try {
			GUICE_COUNTER.bindContextIncludingCpu(requestName);
			return invocation.proceed();
		} catch (final Error e) {
			// on catche Error pour avoir les erreurs systèmes
			// mais pas Exception qui sont fonctionnelles en général
			systemError = true;
			throw e;
		} finally {
			// on enregistre la requête dans les statistiques
			GUICE_COUNTER.addRequestForCurrentContext(systemError);
		}
	}

	/**
	 * Determine monitor name for a method invocation.
	 *
	 * @param invocation the method invocation (not null)
	 * @return the monitor name for this invocation
	 */
	private static String getMonitorName(MethodInvocation invocation) {
		final String classPart = getClassPart(invocation);
		final String methodPart = getMethodPart(invocation);
		return classPart + '.' + methodPart;
	}

	private static String getClassPart(MethodInvocation invocation) {
		final Class<?> targetClass = invocation.getMethod().getDeclaringClass();
		final MonitoredWithGuice classAnnotation = targetClass
				.getAnnotation(MonitoredWithGuice.class);
		if (classAnnotation == null || classAnnotation.name() == null
				|| classAnnotation.name().length() == 0) {
			return targetClass.getSimpleName();
		}
		return classAnnotation.name();
	}

	private static String getMethodPart(MethodInvocation invocation) {
		final MonitoredWithGuice methodAnnotation = invocation.getMethod().getAnnotation(
				MonitoredWithGuice.class);
		if (methodAnnotation == null || methodAnnotation.name() == null
				|| methodAnnotation.name().length() == 0) {
			return invocation.getMethod().getName();
		}
		return methodAnnotation.name();
	}
}
