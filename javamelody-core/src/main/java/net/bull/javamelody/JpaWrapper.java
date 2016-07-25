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
import java.util.Iterator;
import java.util.ServiceLoader;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Query;

import net.bull.javamelody.naming.DefaultJpaNamingStrategy;
import net.bull.javamelody.naming.JpaNamingStrategy;

/**
 * Cette classe est utile pour construire des proxy pour JPA.
 * @author Emeric Vernat
 */
public final class JpaWrapper {
	private static final boolean DISABLED = Boolean
			.parseBoolean(Parameters.getParameter(Parameter.DISABLED));
	private static final Counter JPA_COUNTER = MonitoringProxy.getJpaCounter();
	private static final JpaNamingStrategy JPA_NAMING_STRATEGY;
	private static final String JPA_NAMING_STRATEGY_SYSPROP_NAME = "net.bull.javamelody.naming.JpaNamingStrategy";

	private JpaWrapper() {
		super();
	}

	static {
		JPA_NAMING_STRATEGY = createNamingStrategy();
	}

	private static JpaNamingStrategy createNamingStrategy() {
		// try via system property
		String implClassName = System.getProperty(JPA_NAMING_STRATEGY_SYSPROP_NAME);
		if (implClassName != null && !implClassName.isEmpty()) {
			try {
				return (JpaNamingStrategy) Class.forName(implClassName)
								.getConstructor()
								.newInstance();
			} catch (final Exception e) {
				throw new IllegalStateException("Could not instantiate class: " + implClassName
								+ " defined in system propperty: " + JPA_NAMING_STRATEGY_SYSPROP_NAME
								, e);
			}
		}

		// try ServiceLoader/SPI
		Iterator<JpaNamingStrategy> strategyImplIter = ServiceLoader
						.load(JpaNamingStrategy.class)
						.iterator();
		if (strategyImplIter.hasNext()) {
			return strategyImplIter.next();
		}

		// Fall back to default
		return new DefaultJpaNamingStrategy();
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
			JpaMethod jpaMethod = JpaMethod.forCall(method, args);
			assert jpaMethod != null;
			assert method != null;
			String requestName = JPA_NAMING_STRATEGY.getRequestName(jpaMethod, method, args);
			assert requestName != null;

			if (jpaMethod.isQuery() && jpaMethod.isMonitored()) {
				Query query = (Query) method.invoke(entityManager, args);
				query = createQueryProxy(query, requestName);
				return query;
			}
			if (jpaMethod.isMonitored()) {
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
			final StringBuilder requestName = new StringBuilder();
			requestName.append(methodName, "create".length(), methodName.length());
			appendArgs(requestName, args);
			return requestName.toString();
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
