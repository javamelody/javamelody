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

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;

import org.springframework.aop.ClassFilter;
import org.springframework.aop.MethodMatcher;
import org.springframework.aop.Pointcut;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.Scheduled;

/**
 * Pointcut that identifies methods/classes with the {@link Async}, {@link Scheduled} and Schedules annotation.
 *
 * @author Emeric Vernat
 */
public class MonitoredSpringAsyncAndScheduledPointcut implements Pointcut {
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

		// use reflection in case classes are not available
		// (in particular, for the Schedules class which exists only since Spring 4.0)
		private static final Class<? extends Annotation> ASYNC_CLASS = getClass(
				"org.springframework.scheduling.annotation.Async");
		private static final Class<? extends Annotation> SCHEDULED_CLASS = getClass(
				"org.springframework.scheduling.annotation.Scheduled");
		private static final Class<? extends Annotation> SCHEDULES_CLASS = getClass(
				"org.springframework.scheduling.annotation.Schedules");

		@SuppressWarnings("unchecked")
		private static <T> Class<T> getClass(String className) {
			try {
				return (Class<T>) Class.forName(className);
			} catch (final ClassNotFoundException e) {
				return null;
			}
		}

		/** {@inheritDoc} */
		@Override
		public boolean matches(Method method, Class<?> targetClass) {
			return matchesAsync(method, targetClass) || matchesScheduled(method);
		}

		private boolean matchesAsync(Method method, Class<?> targetClass) {
			return ASYNC_CLASS != null && (targetClass.isAnnotationPresent(ASYNC_CLASS)
					|| method.getDeclaringClass().isAnnotationPresent(ASYNC_CLASS)
					|| method.isAnnotationPresent(ASYNC_CLASS));
		}

		private boolean matchesScheduled(Method method) {
			return SCHEDULED_CLASS != null && method.isAnnotationPresent(SCHEDULED_CLASS)
					|| SCHEDULES_CLASS != null && method.isAnnotationPresent(SCHEDULES_CLASS);
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
