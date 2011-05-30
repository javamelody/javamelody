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
		@SuppressWarnings("rawtypes")
		public boolean matches(Class clazz) {
			return interfaceClass.isAssignableFrom(clazz);
		}
	};

	/**
	 * Constructeur.
	 */
	public MonitoredWithInterfacePointcut() {
		super();
		this.interfaceClass = null;
	}

	/**
	 * @return a class filter based on interfaceName.
	 */
	public ClassFilter getClassFilter() {
		return classFilter;
	}

	/**
	 * @return a method matcher that matches any method
	 */
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
