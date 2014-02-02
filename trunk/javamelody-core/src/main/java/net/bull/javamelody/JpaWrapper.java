/*
 * Copyright 2008-2014 by Emeric Vernat
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

/**
 * Cette classe est utile pour construire des proxy pour JPA.
 * @author Emeric Vernat
 */
final class JpaWrapper {
	private static final Counter JPA_COUNTER = MonitoringProxy.getJpaCounter();

	private JpaWrapper() {
		super();
	}

	static Counter getJpaCounter() {
		return JPA_COUNTER;
	}

	static EntityManagerFactory createEntityManagerFactoryProxy(
			final EntityManagerFactory entityManagerFactory) {
		// on veut monitorer seulement les EntityManager retournés par les deux méthodes createEntityManager
		return JdbcWrapper.createProxy(entityManagerFactory, new EntityManagerFactoryHandler(
				entityManagerFactory));
	}

	static EntityManager createEntityManagerProxy(final EntityManager entityManager) {
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
			final Class<?> returnType = method.getReturnType();
			if (returnType != null && Query.class.isAssignableFrom(returnType)) {
				Query query = (Query) method.invoke(entityManager, args);
				// (pas besoin de proxy pour getCriteriaBuilder() car cela repasse par entityManager.createQuery(criteriaQuery)
				if ("createQuery".equals(methodName) || "createNamedQuery".equals(methodName)
						|| "createNativeQuery".equals(methodName)) {
					final String requestName = getQueryRequestName(methodName, args);
					query = createQueryProxy(query, requestName);
				}
				return query;
			} else if ("find".equals(methodName) && args != null && args.length > 0
					&& args[0] instanceof Class) {
				final String requestName = "find(" + ((Class<?>) args[0]).getSimpleName() + ')';
				return doInvoke(entityManager, method, args, requestName);
			} else if (("merge".equals(methodName) || "persist".equals(methodName)
					|| "refresh".equals(methodName) || "remove".equals(methodName)
					|| "flush".equals(methodName) || "lock".equals(methodName))
					&& args != null && args.length > 0) {
				final String requestName = method.getName() + '('
						+ args[0].getClass().getSimpleName() + ')';
				return doInvoke(entityManager, method, args, requestName);
			} else if ("flush".equals(methodName)) {
				final String requestName = "flush()";
				return doInvoke(entityManager, method, args, requestName);
			}
			return method.invoke(entityManager, args);
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
			if (("getSingleResult".equals(methodName) || "getResultList".equals(methodName) || "executeUpdate"
					.equals(methodName)) && (args == null || args.length == 0)) {
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
