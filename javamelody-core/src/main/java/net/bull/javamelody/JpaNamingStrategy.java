/*
 * Copyright 2008-2016 by Emeric Vernat
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

import javax.persistence.EntityManager;

/**
 * Interface to implement jpa method to request name conversion.
 * @see JpaDefaultNamingStrategy
 * @author Christoph Linder
 */
public interface JpaNamingStrategy {

	/**
	 * Implementors must calculate a nonnull String that will get displayed in the JPA section.
	 *
	 * The implementing class <b>must</b> habe a public no-args constructor.
	 *
	 * @param jpaMethod A normalization of the method that got called on the {@link EntityManager}.
	 * 					Corresponds with param javaMethod.
	 * @param javaMethod The method that got called on the {@link EntityManager}.
	 * 					 Corresponds with param jpaMethod.
	 * @param args Nullable, the arguments for javaMethod
	 * @return a non-null String that represents the request name of the JPA-Counter.
	 */
	String getRequestName(JpaMethod jpaMethod, Method javaMethod, Object[] args);
}
