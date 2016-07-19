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

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Query;
import javax.persistence.criteria.CommonAbstractCriteria;
import javax.persistence.criteria.CriteriaDelete;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.CriteriaUpdate;

/**
 * Cette classe est utile pour construire des proxy pour JPA.
 * @author Emeric Vernat
 */
public final class JpaWrapper {
	private static final boolean DISABLED = Boolean
					.parseBoolean(Parameters.getParameter(Parameter.DISABLED));
	private static final Counter JPA_COUNTER = MonitoringProxy.getJpaCounter();

	private JpaWrapper() {
		super();
	}

	static Counter getJpaCounter() {
		return JPA_COUNTER;
	}

	/**
	 * Create proxy of EntityManagerFactory.
	 * @param entityManagerFactory EntityManagerFactory
	 * @return EntityManagerFactory
	 */
	public static EntityManagerFactory createEntityManagerFactoryProxy(
					final EntityManagerFactory entityManagerFactory) {
		if (DISABLED || !JPA_COUNTER.isDisplayed()) {
			return entityManagerFactory;
		}
		// on veut monitorer seulement les EntityManager retournés par les deux méthodes createEntityManager
		return JdbcWrapper.createProxy(entityManagerFactory,
						new EntityManagerFactoryHandler(entityManagerFactory));
	}

	static EntityManager createEntityManagerProxy(final EntityManager entityManager) {
		if (DISABLED || !JPA_COUNTER.isDisplayed()) {
			return entityManager;
		}
		return JdbcWrapper.createProxy(entityManager, new EntityManagerHandler(entityManager));
	}

	static Query createQueryProxy(final Query query, final String requestName) {
		return JdbcWrapper.createProxy(query, new QueryHandler(query, requestName));
	}

	static Object doInvoke(final Object object, final Method method, final Object[] args,
					final String requestName) throws Throwable {
		boolean systemError = false;
		try {
			JPA_COUNTER.bindContextIncludingCpu(requestName);
			return method.invoke(object, args);
		} catch (final Error e) {
			// on catche Error pour avoir les erreurs systèmes
			// mais pas Exception qui sont fonctionnelles en général
			systemError = true;
			throw e;
		} finally {
			// on enregistre la requête dans les statistiques
			JPA_COUNTER.addRequestForCurrentContext(systemError);
		}
	}

	private static class EntityManagerFactoryHandler implements InvocationHandler {
		private final EntityManagerFactory entityManagerFactory;

		EntityManagerFactoryHandler(final EntityManagerFactory entityManagerFactory) {
			super();
			this.entityManagerFactory = entityManagerFactory;
		}

		/** {@inheritDoc} */
		@Override
		public Object invoke(final Object proxy, final Method method, final Object[] args)
						throws Throwable {
			Object result = method.invoke(entityManagerFactory, args);
			if (result instanceof EntityManager) {
				result = createEntityManagerProxy((EntityManager) result);
			}
			return result;
		}
	}

	private static class EntityManagerHandler implements InvocationHandler {
		private final EntityManager entityManager;

		EntityManagerHandler(final EntityManager entityManager) {
			super();
			this.entityManager = entityManager;
		}

		/** {@inheritDoc} */
		@Override
		public Object invoke(final Object proxy, final Method method, final Object[] args)
						throws Throwable {
			final String methodName = method.getName();
			if (isCreateSomeQuery(method)) {
				Query query = (Query) method.invoke(entityManager, args);
				// (pas besoin de proxy pour getCriteriaBuilder() car cela repasse par entityManager.createQuery(criteriaQuery)
				final String requestName = getQueryRequestName(methodName, args);
				query = createQueryProxy(query, requestName);
				return query;
			} else if (isOneOfFindMethods(args, methodName)) {
				final String requestName = "find(" + ((Class<?>) args[0]).getSimpleName() + ')';
				return doInvoke(entityManager, method, args, requestName);
			} else if (isMergePersistRefreshRemoveDetachOrLockMethod(methodName, args)) {
				final String requestName = method.getName() + '('
								+ args[0].getClass().getSimpleName() + ')';
				return doInvoke(entityManager, method, args, requestName);
			} else if ("flush".equals(methodName)) {
				final String requestName = "flush()";
				return doInvoke(entityManager, method, args, requestName);
			}
			return method.invoke(entityManager, args);
		}

		private boolean isMergePersistRefreshRemoveDetachOrLockMethod(String methodName,
						Object[] args) {
			return args != null && args.length > 0
							&& ("merge".equals(methodName) || "persist".equals(methodName)
							|| "refresh".equals(methodName) || "remove".equals(methodName)
							|| "detach".equals(methodName) || "lock".equals(methodName));
		}

		private boolean isOneOfFindMethods(Object[] args, String methodName) {
			return "find".equals(methodName) && args != null && args.length > 0
							&& args[0] instanceof Class;
		}

		private boolean isCreateSomeQuery(Method method) {
			final String methodName = method.getName();
			final Class<?> returnType = method.getReturnType();
			final boolean methodNameOk = "createQuery".equals(methodName)
							|| "createNamedQuery".equals(methodName)
							|| "createNativeQuery".equals(methodName)
							// JavaEE 7:
							|| "createStoredProcedureQuery".equals(methodName)
							|| "createNamedStoredProcedureQuery".equals(methodName);
			return methodNameOk && returnType != null && Query.class.isAssignableFrom(returnType);
		}

		private String getQueryRequestName(String methodName, Object[] args) {
			// Handle CriteriaQuery stuff differently: else we would get
			// a new generic name (similar to Object.toString()) for every instance of the query
			if ("createQuery".equals(methodName) && args.length == 1 && args[0] instanceof CommonAbstractCriteria) {
				return getCriteriaQueryRequestName(((CommonAbstractCriteria)args[0]).getClass());
			}
			final StringBuilder requestName = new StringBuilder();
			requestName.append(methodName, "create".length(), methodName.length());
			appendArgs(requestName, args);
			return requestName.toString();
		}


		private String getCriteriaCreateQueryArgName(Class<? extends CommonAbstractCriteria> criteriaQueryClass) {
			if (CriteriaQuery.class.isAssignableFrom(criteriaQueryClass)) {
				return CriteriaQuery.class.getSimpleName();
			}
			if (CriteriaUpdate.class.isAssignableFrom(criteriaQueryClass)) {
				return CriteriaUpdate.class.getSimpleName();
			}
			if (CriteriaDelete.class.isAssignableFrom(criteriaQueryClass)) {
				return CriteriaDelete.class.getSimpleName();
			}
			return criteriaQueryClass.getSimpleName();
		}

		/**
		 * Analyze the stacktrace of the current thread and (hopefully) find the fist
		 * entry that is not from JavaMelody itself and is not from other proxies
		 */
		private String getCriteriaQueryRequestName(Class<? extends CommonAbstractCriteria> criteriaQueryClass) {
			String methodName = getCriteriaCreateQueryArgName(criteriaQueryClass);

			StackTraceElement[] stack = Thread.currentThread().getStackTrace();
			for (StackTraceElement element : stack) {
				String stackEntryclassName = element.getClassName();
				if (stackEntryclassName.startsWith(Thread.class.getCanonicalName())
								|| stackEntryclassName.startsWith("net.bull.javamelody")
								// TODO: discover proxy methods in JDKs of other vendors
								// or at least implement a way to let the user provide a method to skip entries
								|| stackEntryclassName.startsWith("com.sun.proxy.$Proxy")) {
					continue;
				}
				//TODO: element.toString() contains line number => a monitor name for a specific query might change
				// although the query itself did not change.
				// Problem: I cannot think of a better way to uniquely identify a query
				return methodName + '(' + element.toString() + ')';
			}
			return methodName + "(<unknown location>)";
		}

		private static void appendArgs(StringBuilder requestName, Object[] args) {
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
	}

	private static class QueryHandler implements InvocationHandler {
		private final Query query;
		private final String requestName;

		QueryHandler(final Query query, final String requestName) {
			super();
			this.query = query;
			this.requestName = requestName;
		}

		/** {@inheritDoc} */
		@Override
		public Object invoke(final Object proxy, final Method method, final Object[] args)
						throws Throwable {
			final String methodName = method.getName();
			if (("getSingleResult".equals(methodName) || "getResultList".equals(methodName)
							|| "executeUpdate".equals(methodName)) && (args == null || args.length == 0)) {
				return doInvoke(query, method, args, requestName);
			} else if (methodName.startsWith("set") && method.getReturnType() != null
							&& Query.class.isAssignableFrom(method.getReturnType())) {
				method.invoke(query, args);
				// on ne récupère pas la query en résultat, car ce doit être la même qu'en entrée.
				// donc on retourne le proxy
				return proxy;
			}
			return method.invoke(query, args);
		}
	}
}
