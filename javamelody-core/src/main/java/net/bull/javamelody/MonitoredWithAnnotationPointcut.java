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

import java.lang.reflect.Method;

import org.springframework.aop.ClassFilter;
import org.springframework.aop.MethodMatcher;
import org.springframework.aop.Pointcut;

/**
 * Pointcut that identifies methods/classes with the {@link MonitoredWithSpring} annotation.
 *
 * Inspired by Erik van Oosten (Java Simon, Licence LGPL)
 * @author Emeric Vernat
 */
public class MonitoredWithAnnotationPointcut implements Pointcut {
	/**
	 * @return a class filter that lets all class through.
	 */
	@Override
	public ClassFilter getClassFilter() {
		return ClassFilter.TRUE;
	}

	/**
	 * @return a method matcher that matches any method that has the {@link MonitoredWithSpring} annotation,
	 *         or is in a class with the {@link MonitoredWithSpring} annotation
	 */
	@Override
	public MethodMatcher getMethodMatcher() {
		return MonitoredMethodMatcher.INSTANCE;
	}

	private enum MonitoredMethodMatcher implements MethodMatcher {
		INSTANCE;

		/** {@inheritDoc} */
		@Override
		public boolean matches(Method method, Class<?> targetClass) {
			return targetClass.isAnnotationPresent(MonitoredWithSpring.class)
					|| method.getDeclaringClass().isAnnotationPresent(MonitoredWithSpring.class)
					|| method.isAnnotationPresent(MonitoredWithSpring.class);
		}

		/** {@inheritDoc} */
		@Override
		public boolean isRuntime() {
			return false;
		}

		/** {@inheritDoc} */
		@Override
		public boolean matches(Method method, Class<?> targetClass, Object... args) {
			throw new UnsupportedOperationException("This is not a runtime method matcher");
		}
	}
}
