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

import com.mongodb.MongoNamespace;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.MongoDatabase;

import net.bull.javamelody.internal.common.Parameters;
import net.bull.javamelody.internal.model.Counter;

/**
 * Cette classe est utile pour construire des proxy pour <a href='https://www.mongodb.com/'>MongoDB</a>.
 * @author Emeric Vernat
 */
public final class MongoWrapper {
	private static final Counter SERVICES_COUNTER = MonitoringProxy.getServicesCounter();
	private static final boolean COUNTER_HIDDEN = Parameters
			.isCounterHidden(SERVICES_COUNTER.getName());
	private static final boolean DISABLED = Parameter.DISABLED.getValueAsBoolean();

	private MongoWrapper() {
		super();
	}

	/**
	 * Create proxy of {@link MongoDatabase}.
	 * @param database MongoDatabase
	 * @return MongoDatabase
	 */
	public static MongoDatabase createDatabaseProxy(final MongoDatabase database) {
		if (DISABLED) {
			return database;
		}
		SERVICES_COUNTER.setDisplayed(!COUNTER_HIDDEN);
		SERVICES_COUNTER.setUsed(true);
		return JdbcWrapper.createProxy(database, new MongoDatabaseHandler(database));
	}

	static <T> MongoCollection<T> createCollectionProxy(final MongoCollection<T> collection,
			final String collectionName) {
		return JdbcWrapper.createProxy(collection,
				new MongoCollectionHandler(collection, collectionName));
	}

	static Object doInvoke(final Object object, final Method method, final Object[] args,
			final String requestName) throws Throwable {
		boolean systemError = false;
		try {
			SERVICES_COUNTER.bindContextIncludingCpu(requestName);
			return method.invoke(object, args);
		} catch (final Error e) {
			// on catche Error pour avoir les erreurs systèmes
			// mais pas Exception qui sont fonctionnelles en général
			systemError = true;
			throw e;
		} finally {
			// on enregistre la requête dans les statistiques
			SERVICES_COUNTER.addRequestForCurrentContext(systemError);
		}
	}

	private static class MongoDatabaseHandler implements InvocationHandler {
		private final MongoDatabase database;

		MongoDatabaseHandler(final MongoDatabase database) {
			super();
			this.database = database;
		}

		/** {@inheritDoc} */
		@Override
		public Object invoke(final Object proxy, final Method method, final Object[] args)
				throws Throwable {
			Object result = method.invoke(database, args);
			if (result instanceof MongoCollection && args != null && args.length > 0
					&& args[0] instanceof String) {
				final MongoCollection<?> collection = (MongoCollection<?>) result;
				final MongoNamespace namespace = collection.getNamespace();
				final String name;
				if (namespace != null) {
					name = namespace.getFullName();
				} else {
					name = (String) args[0];
				}
				result = createCollectionProxy(collection, name);
			} else if (result instanceof MongoDatabase) {
				// il faut monitorer la nouvelle instance de MongoDatabase en retour
				result = createDatabaseProxy((MongoDatabase) result);
			}
			return result;
		}
	}

	private static class MongoCollectionHandler implements InvocationHandler {
		private final MongoCollection<?> collection;
		private final String collectionName;

		MongoCollectionHandler(final MongoCollection<?> collection, final String collectionName) {
			super();
			this.collection = collection;
			this.collectionName = collectionName;
		}

		/** {@inheritDoc} */
		@Override
		public Object invoke(final Object proxy, final Method method, final Object[] args)
				throws Throwable {
			final String methodName = method.getName();
			if (methodName.startsWith("with") && method.getReturnType() != null
					&& MongoCollection.class.isAssignableFrom(method.getReturnType())) {
				// inutile de monitorer withDocumentClass(...), etc
				// mais il faut monitorer la nouvelle instance de MongoCollection en retour
				MongoCollection<?> result = (MongoCollection<?>) method.invoke(collection, args);
				final MongoNamespace namespace = collection.getNamespace();
				final String name;
				if (namespace != null) {
					name = namespace.getFullName();
				} else {
					name = methodName;
				}
				result = createCollectionProxy(result, name);
				return result;
			} else if (methodName.startsWith("get")) {
				// inutile de monitorer getDocumentClass(), getNamespace(), etc
				return method.invoke(collection, args);
			}
			final String requestName = collectionName + '.' + method.getName();
			return doInvoke(collection, method, args, requestName);
		}
	}
}
