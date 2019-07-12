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

import org.springframework.aop.ClassFilter;
import org.springframework.aop.MethodMatcher;
import org.springframework.aop.Pointcut;

/**
 * Pointcut that identifies methods/classes with an interface.
 * @author Emeric Vernat
 */
public class MonitoredWithInterfacePointcut implements Pointcut {
	Class<?> interfaceClass;

	private final ClassFilter classFilter = new ClassFilter() {
		/** {@inheritDoc} */
		@Override
		public boolean matches(Class<?> clazz) {
			return interfaceClass.isAssignableFrom(clazz);
		}
	};

	/**
	 * @return a class filter based on interfaceName.
	 */
	@Override
	public ClassFilter getClassFilter() {
		return classFilter;
	}

	/**
	 * @return a method matcher that matches any method
	 */
	@Override
	public MethodMatcher getMethodMatcher() {
		return MethodMatcher.TRUE;
	}

	/**
	 * Retourne le nom de l'interface à matcher.
	 * @return String
	 */
	public String getInterfaceName() {
		if (interfaceClass != null) {
			return interfaceClass.getName();
		}
		return null;
	}

	/**
	 * Définit le nom de l'interface à matcher (non null).
	 * @param interfaceName String
	 * @throws ClassNotFoundException Si classe non trouvée
	 */
	public void setInterfaceName(String interfaceName) throws ClassNotFoundException {
		assert interfaceName != null;
		this.interfaceClass = Class.forName(interfaceName);
	}
}
