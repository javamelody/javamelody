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

import org.springframework.aop.ClassFilter;
import org.springframework.aop.MethodMatcher;
import org.springframework.aop.Pointcut;
import org.springframework.stereotype.Controller;
import org.springframework.stereotype.Service;

/**
 * Pointcut that identifies methods/classes with the {@link Controller}, RestController and {@link Service} annotations.
 *
 * @author Emeric Vernat
 */
public class MonitoredSpringControllerAndServicePointcut implements Pointcut {

	private enum MonitoredClassFilter implements ClassFilter {
		INSTANCE;

		// use reflection in case classes are not available
		// (because spring-web may not be in the dependencies or for RestController which exists only since Spring 4.0)
		private static final Class<? extends Annotation> CONTROLLER_CLASS = getClass(
				"org.springframework.stereotype.Controller");
		private static final Class<? extends Annotation> REST_CONTROLLER_CLASS = getClass(
				"org.springframework.web.bind.annotation.RestController");
		private static final Class<? extends Annotation> SERVICE_CLASS = getClass(
				"org.springframework.stereotype.Service");

		@SuppressWarnings("unchecked")
		private static <T> Class<T> getClass(String className) {
			try {
				return (Class<T>) Class.forName(className);
			} catch (final ClassNotFoundException e) {
				return null;
			}
		}

		@Override
		public boolean matches(Class<?> clazz) {
			return CONTROLLER_CLASS != null && clazz.isAnnotationPresent(CONTROLLER_CLASS)
					|| REST_CONTROLLER_CLASS != null
							&& clazz.isAnnotationPresent(REST_CONTROLLER_CLASS)
					|| SERVICE_CLASS != null && clazz.isAnnotationPresent(SERVICE_CLASS);
		}
	}

	/**
	 * @return a class filter that lets all class through.
	 */
	@Override
	public ClassFilter getClassFilter() {
		return MonitoredClassFilter.INSTANCE;
	}

	/**
	 * @return a method matcher that matches any method.
	 */
	@Override
	public MethodMatcher getMethodMatcher() {
		return MethodMatcher.TRUE;
	}
}
