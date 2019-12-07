/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
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
import java.security.AccessController;
import java.security.PrivilegedAction;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.persistence.EntityManagerFactory;
import javax.persistence.spi.LoadState;
import javax.persistence.spi.PersistenceProvider;
import javax.persistence.spi.PersistenceProviderResolver;
import javax.persistence.spi.PersistenceProviderResolverHolder;
import javax.persistence.spi.PersistenceUnitInfo;
import javax.persistence.spi.ProviderUtil;

import net.bull.javamelody.internal.common.LOG;
import net.bull.javamelody.internal.common.Parameters;
import net.bull.javamelody.internal.model.Counter;

/**
 * Persistence provider pour monitorer JPA.
 * From Sirona, http://sirona.incubator.apache.org/
 */
public class JpaPersistence implements PersistenceProvider {
	private static final Counter JPA_COUNTER = MonitoringProxy.getJpaCounter();
	private static final boolean COUNTER_HIDDEN = Parameters.isCounterHidden(JPA_COUNTER.getName());

	/**
	 * The name of the {@link javax.persistence.spi.PersistenceProvider} implementor
	 * <p/>
	 * See JPA 2 sections 9.4.3 and 8.2.1.4
	 */
	private static final String JPA_PERSISTENCE_PROVIDER = "javax.persistence.provider";
	private static final String OWN_PACKAGE = JpaPersistence.class.getName().substring(0,
			JpaPersistence.class.getName().lastIndexOf('.'));
	private static final String DELEGATE_PROVIDER_KEY = OWN_PACKAGE + ".jpa.provider";

	private static final String[] PROVIDERS = {
			"org.apache.openjpa.persistence.PersistenceProviderImpl",
			"org.hibernate.jpa.HibernatePersistenceProvider",
			"org.hibernate.ejb.HibernatePersistence",
			"org.eclipse.persistence.jpa.PersistenceProvider",
			"oracle.toplink.essentials.ejb.cmp3.EntityManagerFactoryProvider",
			"oracle.toplink.essentials.PersistenceProvider",
			"me.prettyprint.hom.CassandraPersistenceProvider",
			"org.datanucleus.jpa.PersistenceProviderImpl",
			"com.orientechnologies.orient.core.db.object.jpa.OJPAPersistenceProvider",
			"com.orientechnologies.orient.object.jpa.OJPAPersistenceProvider",
			"com.spaceprogram.simplejpa.PersistenceProviderImpl", };

	private static final ProviderUtil DUMMY_PROVIDER_UTIL = new ProviderUtil() {
		@Override
		public LoadState isLoadedWithoutReference(Object entity, String attributeName) {
			return LoadState.UNKNOWN;
		}

		@Override
		public LoadState isLoadedWithReference(Object entity, String attributeName) {
			return LoadState.UNKNOWN;
		}

		@Override
		public LoadState isLoaded(Object entity) {
			return LoadState.UNKNOWN;
		}
	};

	private volatile PersistenceProvider delegate; // NOPMD

	private static class JavaMelodyPersistenceProviderResolver
			implements PersistenceProviderResolver {
		private final PersistenceProviderResolver delegate;

		JavaMelodyPersistenceProviderResolver(PersistenceProviderResolver delegate) {
			super();
			this.delegate = delegate;
		}

		@Override
		public List<PersistenceProvider> getPersistenceProviders() {
			// avant de retourner la liste des persistence providers
			// on met notre JpaPersistence en premier pour qu'il soit toujours choisi
			// et qu'il délègue au persistence provider final
			final List<PersistenceProvider> providers = delegate.getPersistenceProviders();
			final List<PersistenceProvider> result = new ArrayList<PersistenceProvider>();
			for (final PersistenceProvider provider : providers) {
				if (provider instanceof JpaPersistence) {
					result.add(0, provider);
				} else {
					result.add(provider);
				}
			}
			return result;
		}

		@Override
		public void clearCachedProviders() {
			delegate.clearCachedProviders();
		}
	}

	/**
	 * Active le monitoring JPA par défaut,
	 * même si <provider>net.bull.javamelody.JpaPersistence</provider> n'est pas dans META-INF/persistence.xml
	 */
	public static void initPersistenceProviderResolver() {
		try {
			PersistenceProviderResolver resolver = PersistenceProviderResolverHolder
					.getPersistenceProviderResolver();
			if (!(resolver instanceof JavaMelodyPersistenceProviderResolver)) {
				resolver = new JavaMelodyPersistenceProviderResolver(resolver);
				PersistenceProviderResolverHolder.setPersistenceProviderResolver(resolver);
				LOG.debug("JPA persistence provider resolver initialized");
			}
		} catch (final Throwable t) { // NOPMD
			LOG.info("initialization of jpa persistence provider resolver failed, skipping");
		}
	}

	// cette classe est instanciée dès le démarrage (WildFly notamment),
	// il ne faut donc pas appeler initJpaCounter() dans le constructeur

	private void initJpaCounter() {
		// quand cette classe est utilisée, le compteur est affiché
		// sauf si le paramètre displayed-counters dit le contraire
		JPA_COUNTER.setDisplayed(!COUNTER_HIDDEN);
		// setUsed(true) nécessaire ici si le contexte jpa est initialisé avant FilterContext
		// sinon les statistiques jpa ne sont pas affichées
		JPA_COUNTER.setUsed(true);
		LOG.debug("jpa persistence initialized");
	}

	/** {@inheritDoc} */
	@SuppressWarnings({ "rawtypes", "unchecked" })
	@Override
	public EntityManagerFactory createEntityManagerFactory(final String unit, final Map map) {
		initJpaCounter();
		final PersistenceProvider persistenceProvider = findDelegate(map);
		final ClassLoader tccl = tccl();

		final ClassLoader hack = AccessController.doPrivileged(new PrivilegedAction<ClassLoader>() { // pour findbugs
			/** {@inheritDoc} */
			@Override
			public ClassLoader run() {
				return new JpaOverridePersistenceXmlClassLoader(tccl,
						persistenceProvider.getClass().getName());
			}
		});

		Thread.currentThread().setContextClassLoader(hack);
		try {
			final Map overridenMap = new HashMap();
			if (map != null) {
				overridenMap.putAll(map);
			}
			// #869 No Persistence provider for EntityManager, with Hibernate 5.4 & JPA
			// (when JpaOverridePersistenceXmlClassLoader is not enough)
			overridenMap.put(JPA_PERSISTENCE_PROVIDER, persistenceProvider.getClass().getName());
			final EntityManagerFactory entityManagerFactory = persistenceProvider
					.createEntityManagerFactory(unit, overridenMap);
			if (entityManagerFactory == null) {
				return null;
			}
			return JpaWrapper.createEntityManagerFactoryProxy(entityManagerFactory);
		} finally {
			Thread.currentThread().setContextClassLoader(tccl);
		}
	}

	/** {@inheritDoc} */
	@SuppressWarnings({ "rawtypes", "unchecked" })
	@Override
	public EntityManagerFactory createContainerEntityManagerFactory(final PersistenceUnitInfo info,
			final Map map) {
		initJpaCounter();
		final PersistenceProvider persistenceProvider = findDelegate(map);
		// on surcharge PersistenceUnitInfo.getPersistenceProviderClassName()
		// pour retourner le PersistenceProvider délégué et pas nous même
		final PersistenceUnitInfo proxiedInfo = createPersistentUnitInfoProxy(info,
				persistenceProvider);
		final Map overridenMap = new HashMap();
		if (map != null) {
			overridenMap.putAll(map);
		}
		// #869 No Persistence provider for EntityManager, with Hibernate 5.4 & JPA
		// (when JpaOverridePersistenceXmlClassLoader is not enough)
		overridenMap.put(JPA_PERSISTENCE_PROVIDER, persistenceProvider.getClass().getName());
		final EntityManagerFactory entityManagerFactory = persistenceProvider
				.createContainerEntityManagerFactory(proxiedInfo, overridenMap);
		if (entityManagerFactory == null) {
			return null;
		}
		return JpaWrapper.createEntityManagerFactoryProxy(entityManagerFactory);
	}

	private PersistenceUnitInfo createPersistentUnitInfoProxy(final PersistenceUnitInfo info,
			final PersistenceProvider persistenceProvider) {
		final InvocationHandler invocationHandler = new ProviderAwareHandler(
				persistenceProvider.getClass().getName(), info);
		return JdbcWrapper.createProxy(info, invocationHandler);
	}

	/** {@inheritDoc} */
	@Override
	public ProviderUtil getProviderUtil() { // we suppose it is loaded later than createXXXEMF so we'll get the delegate
		if (delegate == null) {
			// delegate not yet loaded and perhaps will never be:
			// this method may be called, even without jpa impl to delegate,
			// for example, by hibernate validator via jpa api but without jpa impl
			// (issue 396, if loadOrGuessDelegate(null) was called here),
			// so return a dumb ProviderUtil in this case or if delegate not yet loaded
			return DUMMY_PROVIDER_UTIL;
		}
		return delegate.getProviderUtil();
	}

	private PersistenceProvider findDelegate(final Map<?, ?> map) {
		if (map == null) {
			return loadOrGuessDelegate(null);
		}
		return loadOrGuessDelegate(String.class.cast(map.get(DELEGATE_PROVIDER_KEY)));
	}

	private PersistenceProvider loadOrGuessDelegate(final String name) {
		if (delegate == null) {
			synchronized (this) {
				if (delegate == null) {
					if (name == null) {
						guessDelegate();
					} else {
						try {
							delegate = newPersistence(name);
						} catch (final Exception e) {
							throw new IllegalStateException(new ClassNotFoundException(
									"Can't instantiate '" + name + "'", e));
						}
					}
				}
			}
		}
		if (name != null && !delegate.getClass().getName().equals(name)) {
			try {
				return newPersistence(name);
			} catch (final Exception e) {
				throw new IllegalStateException(
						new ClassNotFoundException("Can't instantiate '" + name + "'", e));
			}
		}
		return delegate;
	}

	private void guessDelegate() {
		// https://issues.apache.org/jira/browse/SIRONA-44
		// https://github.com/javamelody/javamelody/issues/460
		final List<PersistenceProvider> persistenceProviders = PersistenceProviderResolverHolder
				.getPersistenceProviderResolver().getPersistenceProviders();
		for (final PersistenceProvider persistenceProvider : persistenceProviders) {
			if (!getClass().isInstance(persistenceProvider)) {
				delegate = persistenceProvider;
				break;
			}
		}
		if (delegate == null) {
			for (final String provider : PROVIDERS) {
				try {
					delegate = newPersistence(provider);
					break;
				} catch (final Throwable th2) { // NOPMD
					continue;
				}
			}
			if (delegate == null) {
				throw new IllegalStateException(
						new ClassNotFoundException("Can't find a delegate"));
			}
		}
	}

	private static ClassLoader tccl() {
		return Thread.currentThread().getContextClassLoader();
	}

	private static PersistenceProvider newPersistence(final String name) throws Exception { // NOPMD
		return PersistenceProvider.class.cast(tccl().loadClass(name).newInstance());
	}

	private static class ProviderAwareHandler implements InvocationHandler {
		private final String provider;
		private final PersistenceUnitInfo info;

		ProviderAwareHandler(final String provider, final PersistenceUnitInfo info) {
			super();
			this.provider = provider;
			this.info = info;
		}

		/** {@inheritDoc} */
		@Override
		public Object invoke(final Object proxy, final Method method, final Object[] args)
				throws Throwable {
			if ("getPersistenceProviderClassName".equals(method.getName())) {
				return provider;
			}
			return method.invoke(info, args);
		}
	}

	/** {@inheritDoc} */
	@SuppressWarnings("rawtypes")
	@Override
	public void generateSchema(final PersistenceUnitInfo info, final Map map) {
		final PersistenceProvider persistenceProvider = findDelegate(map);
		persistenceProvider.generateSchema(info, map);
	}

	/** {@inheritDoc} */
	@SuppressWarnings("rawtypes")
	@Override
	public boolean generateSchema(final String persistenceUnitName, final Map map) {
		final PersistenceProvider persistenceProvider = findDelegate(map);
		return persistenceProvider.generateSchema(persistenceUnitName, map);
	}
}
