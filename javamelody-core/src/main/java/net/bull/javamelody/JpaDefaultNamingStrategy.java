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

/**
 * Default naming strategy.
 * @author Christoph Linder
 */
public class JpaDefaultNamingStrategy implements JpaNamingStrategy {
	/** {@inheritDoc} */
	// CHECKSTYLE:OFF
	@Override
	public String getRequestName(JpaMethod jpaMethod, Method javaMethod, Object[] args) {
		// CHECKSTYLE:ON
		switch (jpaMethod) {
		case CREATE_QUERY:
		case CREATE_NAMED_QUERY:
		case CREATE_NATIVE_QUERY:
		case CREATE_STORED_PROCEDURE_QUERY:
		case CREATE_NAMED_STORED_PROCEDURE_QUERY:
			return getQueryRequestName(javaMethod, args);
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

	protected String getQueryRequestName(Method javaMethod, Object[] args) {
		final StringBuilder requestName = new StringBuilder();
		final String methodName = javaMethod.getName();
		requestName.append(methodName, "create".length(), methodName.length());
		appendArgs(requestName, args);
		return requestName.toString();
	}

	protected String getMethodWithClassArgRequestName(Method javaMethod, Object[] args) {
		final String requestName = javaMethod.getName() + '(' + ((Class<?>) args[0]).getSimpleName()
				+ ')';
		return requestName;
	}

	protected String getMethodWithEntityArgRequestName(Method javaMethod, Object[] args) {
		final String requestName = javaMethod.getName() + '(' + args[0].getClass().getSimpleName()
				+ ')';
		return requestName;
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
					// in order to have only one request name for pattern like Query(org.hibernate.jpa.criteria.CriteriaQueryImpl@3b0cc2dc):
					// if (arg.toString().endsWith("@3b0cc2dc")) then we could remove "@3b0cc2dc"
					requestName.append(arg);
				}
			}
		}
		requestName.append(')');
	}
}
