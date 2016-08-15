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
import java.io.Serializable;
import java.lang.reflect.Method;

import javax.persistence.Query;

public enum JpaMethod {
	/**
	 * The OTHER enum value is for future compatibility: do not fail even if we don't know the exact method signature...
	 * but do not monitor, either.
	 */
	OTHER(false, false, new NoMethodMatcher()),
	CREATE_QUERY(true, true, new ReturnQueryMethodMatcher("createQuery")),
	CREATE_NAMED_QUERY(true, true, new ReturnQueryMethodMatcher("createNamedQuery")),
	CREATE_NATIVE_QUERY(true, true, new ReturnQueryMethodMatcher("createNativeQuery")),
	CREATE_STORED_PROCEDURE_QUERY(true, true, new ReturnQueryMethodMatcher("createStoredProcedureQuery")),
	CREATE_NAMED_STORED_PROCEDURE_QUERY(true, true, new ReturnQueryMethodMatcher("createNamedStoredProcedureQuery")),
	FIND(true, false, new MethodWithArgsMethodMatcher("find")),
	MERGE(true, false, new MethodWithArgsMethodMatcher("merge")),
	PERSIST(true, false, new MethodWithArgsMethodMatcher("persist")),
	REFRESH(true, false, new MethodWithArgsMethodMatcher("refresh")),
	REMOVE(true, false, new MethodWithArgsMethodMatcher("remove")),
	DETACH(true, false, new MethodWithArgsMethodMatcher("detach")),
	LOCK(true, false, new MethodWithArgsMethodMatcher("lock")),
	FLUSH(true, false, new MethodNameMatcher("flush"));

	private final boolean monitored;
	private final boolean query;
	private final MethodMatcher matcher;


	JpaMethod(boolean monitored, boolean query, MethodMatcher matcher) {
		this.monitored = monitored;
		this.query = query;
		this.matcher = matcher;
	}

	public static JpaMethod forCall(Method method, Object[] args) {
		for (JpaMethod type : JpaMethod.values()) {
			if (type.matcher.matches(type, method, args)) {
				return type;
			}
		}
		return JpaMethod.OTHER;
	}


	public boolean isMonitored() {
		return monitored;
	}

	public boolean isQuery() {
		return query;
	}

	/**
	 * Used by the proxy class to determine if the called method is of relevance for JavaMelody processing.
	 */
	private interface MethodMatcher extends Serializable {
		boolean matches(JpaMethod method, Method javaMethod, Object[] args);
	}

	/**
	 * Does not match anything => method call not monitored by JavaMelody.
	 */
	private static class NoMethodMatcher implements MethodMatcher {
		private static final long serialVersionUID = -7909606883659502100L;

		@Override
		public boolean matches(JpaMethod method, Method javaMethod, Object[] args) {
			return false;
		}
	}

	/**
	 * Matches on exact method name.
	 */
	private static class MethodNameMatcher implements MethodMatcher {
		private static final long serialVersionUID = 3000368936257282142L;
		private final String methodName;

		private MethodNameMatcher(String methodName) {
			this.methodName = methodName;
		}

		private boolean matchesName(Method javaMethod) {
			return methodName.equals(javaMethod.getName());
		}

		@Override
		public boolean matches(JpaMethod method, Method javaMethod, Object[] args) {
			return matchesName(javaMethod);
		}
	}

	/**
	 * Matches on method name and the method must have at least one argument.
	 */
	private static class MethodWithArgsMethodMatcher extends MethodNameMatcher {
		private static final long serialVersionUID = -6626632123950888074L;

		private MethodWithArgsMethodMatcher(String methodName) {
			super(methodName);
		}

		@Override
		public boolean matches(JpaMethod method, Method javaMethod, Object[] args) {
			return super.matches(method, javaMethod, args) && args != null && args.length > 0;
		}
	}

	/**
	 * See {@link MethodWithArgsMethodMatcher}. Also, the called method must return a {@link Query}
	 */
	private static class ReturnQueryMethodMatcher extends MethodWithArgsMethodMatcher {
		private static final long serialVersionUID = 8359075693226965998L;

		public ReturnQueryMethodMatcher(String methodName) {
			super(methodName);
		}

		@Override
		public boolean matches(JpaMethod method, Method javaMethod, Object[] args) {
			return super.matches(method, javaMethod, args)
							&& javaMethod.getReturnType() != null
							&& Query.class.isAssignableFrom(javaMethod.getReturnType());
		}
	}
}
