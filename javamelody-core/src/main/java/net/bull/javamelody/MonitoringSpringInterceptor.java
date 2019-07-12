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

import org.aopalliance.intercept.MethodInterceptor;
import org.aopalliance.intercept.MethodInvocation;
import org.springframework.aop.support.AopUtils;

import net.bull.javamelody.internal.common.LOG;
import net.bull.javamelody.internal.common.Parameters;
import net.bull.javamelody.internal.model.Counter;

/**
 * Method interceptor that measures the duration of the intercepted call.
 *
 * Inspired by Erik van Oosten (Java Simon, Licence LGPL)
 * @author Emeric Vernat
 */
public class MonitoringSpringInterceptor implements MethodInterceptor, Serializable {
	private static final long serialVersionUID = -6594338383847482623L;
	private static final Counter SPRING_COUNTER = MonitoringProxy.getSpringCounter();
	private static final boolean COUNTER_HIDDEN = Parameters
			.isCounterHidden(SPRING_COUNTER.getName());
	private static final boolean DISABLED = Parameter.DISABLED.getValueAsBoolean();

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
				|| classAnnotation.name().isEmpty()) {
			final Class<?> declaringClass = invocation.getMethod().getDeclaringClass();
			final MonitoredWithSpring declaringClassAnnotation = declaringClass
					.getAnnotation(MonitoredWithSpring.class);
			if (declaringClassAnnotation == null || declaringClassAnnotation.name() == null
					|| declaringClassAnnotation.name().isEmpty()) {
				return targetClass.getSimpleName();
			}
			return declaringClassAnnotation.name();
		}
		return classAnnotation.name();
	}

	private static String getMethodPart(MethodInvocation invocation) {
		final MonitoredWithSpring methodAnnotation = invocation.getMethod()
				.getAnnotation(MonitoredWithSpring.class);
		if (methodAnnotation == null || methodAnnotation.name() == null
				|| methodAnnotation.name().isEmpty()) {
			return invocation.getMethod().getName();
		}
		return methodAnnotation.name();
	}
}
