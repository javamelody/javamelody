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

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Query;

import net.bull.javamelody.internal.model.Counter;

/**
 * Cette classe est utile pour construire des proxy pour JPA.
 * @author Emeric Vernat
 */
public final class JpaWrapper {
	private static final boolean DISABLED = Parameter.DISABLED.getValueAsBoolean();
	private static final Counter JPA_COUNTER = MonitoringProxy.getJpaCounter();

	private JpaWrapper() {
		super();
	}

	static Counter getJpaCounter() {
		return JPA_COUNTER;
	}

	/**
	 * Create proxy of {@link EntityManagerFactory}.
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
		private static final JpaNamingStrategy JPA_NAMING_STRATEGY = new JpaNamingStrategy();

		private final EntityManager entityManager;

		EntityManagerHandler(final EntityManager entityManager) {
			super();
			this.entityManager = entityManager;
		}

		@Override
		public Object invoke(final Object proxy, final Method method, final Object[] args)
				throws Throwable {
			final JpaMethod jpaMethod = JpaMethod.forCall(method, args);
			assert jpaMethod != null;
			assert method != null;

			if (jpaMethod.isQuery() && jpaMethod.isMonitored()) {
				Query query = (Query) method.invoke(entityManager, args);
				final String requestName = getRequestName(jpaMethod, method, args, query);
				query = createQueryProxy(query, requestName);
				return query;
			}
			if (jpaMethod.isMonitored()) {
				final String requestName = getRequestName(jpaMethod, method, args, null);
				return doInvoke(entityManager, method, args, requestName);
			}
			return method.invoke(entityManager, args);
		}

		private String getRequestName(final JpaMethod jpaMethod, final Method method,
				final Object[] args, Query query) {
			final String requestName = JPA_NAMING_STRATEGY.getRequestName(jpaMethod, method, args,
					query);
			assert requestName != null;
			return requestName;
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
