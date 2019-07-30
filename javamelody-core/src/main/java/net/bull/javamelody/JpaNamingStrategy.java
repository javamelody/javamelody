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

import javax.persistence.EntityManager;
import javax.persistence.Query;

/**
 * JPA naming strategy.
 * @author Christoph Linder
 * @author Emeric Vernat
 */
class JpaNamingStrategy {
	private static final Class<?> HIBERNATE_QUERY_CLASS = getClass("org.hibernate.Query");

	private static final Class<?> ECLIPSELINK_QUERY_CLASS = getClass(
			"org.eclipse.persistence.jpa.JpaQuery");

	/**
	 * Calculate a non-null String that will get displayed in the JPA requests.
	 *
	 * @param jpaMethod A normalization of the method that got called on the {@link EntityManager}.
	 * @param javaMethod The method that got called on the {@link EntityManager}.
	 * @param args Nullable, the arguments for javaMethod
	 * @param query JPA query if jpaMethod.isQuery() or null otherwise
	 * @return a non-null String that represents the request name of the JPA-Counter.
	 */
	// CHECKSTYLE:OFF
	public String getRequestName(JpaMethod jpaMethod, Method javaMethod, Object[] args,
			Query query) {
		// CHECKSTYLE:ON
		switch (jpaMethod) {
		case CREATE_QUERY:
		case CREATE_NAMED_QUERY:
		case CREATE_NATIVE_QUERY:
		case CREATE_STORED_PROCEDURE_QUERY:
		case CREATE_NAMED_STORED_PROCEDURE_QUERY:
			return getQueryRequestName(javaMethod, args, query);
		case FIND:
			return getMethodWithClassArgRequestName(javaMethod, args);
		case MERGE:
		case PERSIST:
		case REFRESH:
		case REMOVE:
		case DETACH:
		case LOCK:
			return getMethodWithEntityArgRequestName(javaMethod, args);
		case FLUSH:
			return getNoArgsRequestName(javaMethod);
		case OTHER:
		default:
			return getOtherRequestName(javaMethod, args);
		}
	}

	protected String getOtherRequestName(Method javaMethod, Object[] args) {
		final int argsLen = args == null ? 0 : args.length;
		return "other: " + javaMethod.getName() + "(?" + argsLen + "?)";
	}

	protected String getQueryRequestName(Method javaMethod, Object[] args, Query query) {
		final StringBuilder requestName = new StringBuilder();
		final String methodName = javaMethod.getName();
		requestName.append(methodName, "create".length(), methodName.length());
		appendArgs(requestName, args);

		// with help from Christoph Linder
		// and https://antoniogoncalves.org/2012/05/24/how-to-get-the-jpqlsql-string-from-a-criteriaquery-in-jpa/
		if (HIBERNATE_QUERY_CLASS != null && query.getClass().getName().startsWith("org.hibernate.")
				&& requestName.lastIndexOf("@") != -1) {
			// in order to have only one request name instead of patterns like
			// Query(org.hibernate.jpa.criteria.CriteriaQueryImpl@3b0cc2dc)
			final Object unwrappedQuery = query.unwrap(HIBERNATE_QUERY_CLASS);
			return unwrappedQuery.toString();
		} else if (ECLIPSELINK_QUERY_CLASS != null
				&& query.getClass().getName().startsWith("org.eclipse.")
				&& requestName.lastIndexOf("@") != -1) {
			// in order to have only one request name instead of patterns like
			// Query(org.eclipse.persistence.internal.jpa.querydef.CriteriaQueryImpl@6a55299e)
			final Object unwrappedQuery = query.unwrap(ECLIPSELINK_QUERY_CLASS);
			return unwrappedQuery.toString();
		}

		return requestName.toString();
	}

	protected String getMethodWithClassArgRequestName(Method javaMethod, Object[] args) {
		return javaMethod.getName() + '(' + ((Class<?>) args[0]).getSimpleName() + ')';
	}

	protected String getMethodWithEntityArgRequestName(Method javaMethod, Object[] args) {
		return javaMethod.getName() + '(' + args[0].getClass().getSimpleName() + ')';
	}

	protected String getNoArgsRequestName(Method javaMethod) {
		return javaMethod.getName() + "()";
	}

	protected void appendArgs(StringBuilder requestName, Object[] args) {
		requestName.append('(');
		if (args != null) {
			String separator = "";
			for (final Object arg : args) {
				requestName.append(separator);
				separator = ", ";
				if (arg instanceof Class) {
					requestName.append(((Class<?>) arg).getSimpleName());
				} else {
					requestName.append(arg);
				}
			}
		}
		requestName.append(')');
	}

	private static Class<?> getClass(String className) {
		try {
			return Class.forName(className);
		} catch (final ClassNotFoundException e) {
			return null;
		}
	}
}
