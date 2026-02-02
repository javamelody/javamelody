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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Collections;
import java.util.List;

import org.apache.openjpa.persistence.PersistenceUnitInfoImpl;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import jakarta.persistence.EntityManager;
import jakarta.persistence.EntityManagerFactory;
import jakarta.persistence.EntityTransaction;
import jakarta.persistence.Persistence;
import jakarta.persistence.Query;
import jakarta.persistence.TypedQuery;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Root;
import jakarta.persistence.spi.PersistenceProvider;
import jakarta.persistence.spi.PersistenceProviderResolverHolder;
import jakarta.persistence.spi.PersistenceUnitInfo;
import jakarta.persistence.spi.ProviderUtil;
import net.bull.javamelody.internal.model.Counter;

/**
 * Test unitaire pour JpaPersistence et JpaWrapper.
 * @author Emeric Vernat
 */
class TestJpa {
	private static final String PERSON_NAME = "emeric";

	/**
	 * (Ré)initialisation.
	 */
	@BeforeEach
	@AfterEach
	void reset() {
		JpaWrapper.getJpaCounter().clear();
	}

	/**
	 * Test EntityManger.find.
	 */
	@Test
	@Disabled
	void simpleFind() throws ClassNotFoundException {
		Class.forName("org.apache.openjpa.persistence.PersistenceProviderImpl");
		final EntityManagerFactory emf = Persistence.createEntityManagerFactory("test-jse");
		assertNotNull(emf, "createEntityManagerFactory");

		try {
			final EntityManager em = emf.createEntityManager();
			assertNotNull(em, "createEntityManager");
			try {
				em.find(Person.class, 0L);
				assertCounter("find(Person)");
			} finally {
				em.close();
			}
		} finally {
			emf.close();
		}
	}

	/**
	 * Tests createNamedQuery, createNativeQuery et createQery.
	 */
	@Test
	@Disabled
	void createAllQuery() throws ClassNotFoundException {
		Class.forName("org.apache.openjpa.persistence.PersistenceProviderImpl");
		final EntityManagerFactory emf = Persistence.createEntityManagerFactory("test-jse");

		try {
			// init
			final EntityManager emInit = emf.createEntityManager();
			try {
				final EntityTransaction transaction = emInit.getTransaction();
				transaction.begin();
				try {
					final Person p = new Person();
					p.setName(PERSON_NAME);

					emInit.persist(p);
					emInit.flush();
					transaction.commit();
				} catch (final Exception e) {
					transaction.rollback();
				}
			} finally {
				emInit.close();
			}

			reset();

			// checks
			final EntityManager em = emf.createEntityManager();
			try {
				final Query namedQuery = em.createNamedQuery("Person.findByName");
				final String nameParameter = "name";
				namedQuery.setParameter(nameParameter, PERSON_NAME).getSingleResult();
				assertCounter("NamedQuery(Person.findByName)");

				final TypedQuery<Person> namedQuery2 = em.createNamedQuery("Person.findByName",
						Person.class);
				namedQuery2.setParameter(nameParameter, PERSON_NAME).getSingleResult();
				assertCounter("NamedQuery(Person.findByName, Person)");

				final Query nativeQuery = em
						.createNativeQuery("select * from Person where name = ?");
				nativeQuery.setParameter(1, PERSON_NAME).getSingleResult();
				assertCounter("NativeQuery(select * from Person where name = ?)");

				final Query nativeQuery2 = em
						.createNativeQuery("select * from Person where name = ?", Person.class);
				nativeQuery2.setParameter(1, PERSON_NAME).getSingleResult();
				assertCounter("NativeQuery(select * from Person where name = ?, Person)");

				final Query query = em.createQuery("select p from Person p where p.name = :name");
				query.setParameter(nameParameter, PERSON_NAME).getSingleResult();
				assertCounter("Query(select p from Person p where p.name = :name)");

				final TypedQuery<Person> query2 = em
						.createQuery("select p from Person p where p.name = :name", Person.class);
				query2.setParameter(nameParameter, PERSON_NAME).getSingleResult();
				assertCounter("Query(select p from Person p where p.name = :name, Person)");

				final CriteriaBuilder criteriaBuilder = em.getCriteriaBuilder();
				final CriteriaQuery<Object> criteriaQuery = criteriaBuilder.createQuery();
				final Root<Person> from = criteriaQuery.from(Person.class);
				criteriaQuery.select(from);
				final CriteriaQuery<Object> criteriaQuery2 = criteriaQuery
						.where(criteriaBuilder.equal(from.get(nameParameter), PERSON_NAME));
				em.createQuery(criteriaQuery2).getSingleResult();
				assertCounter("Query(SELECT p FROM Person p WHERE p.name = '" + PERSON_NAME + "')");
			} finally {
				em.close();
			}
		} finally {
			emf.close();
		}
	}

	private static void assertCounter(String method) {
		final Counter counter = JpaWrapper.getJpaCounter();
		assertEquals(1, counter.getRequestsCount(), "getRequestsCount");
		assertEquals(method, counter.getRequests().get(0).getName(), "requestName");
		counter.clear();
	}

	/**
	 * Test de JpaPersistence.
	 */
	@Test
	// OpenJPA is not yet compatible with JakartaEE9
	@Disabled
	void testJpaPersistence() {
		final PersistenceProvider jpaPersistence = getJpaPersistence();

		final ProviderUtil providerUtil = jpaPersistence.getProviderUtil();
		assertNotNull(providerUtil, "getProviderUtil");
		// providerUtil == JpaPersistence.DUMMY_PROVIDER_UTIL
		providerUtil.isLoadedWithoutReference(null, null);
		providerUtil.isLoadedWithReference(null, null);
		providerUtil.isLoaded(null);
	}

	private PersistenceProvider getJpaPersistence() {
		final List<PersistenceProvider> persistenceProviders = PersistenceProviderResolverHolder
				.getPersistenceProviderResolver().getPersistenceProviders();
		for (final PersistenceProvider persistenceProvider : persistenceProviders) {
			if (persistenceProvider instanceof JpaPersistence) {
				return persistenceProvider;
			}
		}
		throw new IllegalStateException("JpaPersistence not found in PersistenceProviders");
	}

	@Test
	@Disabled
	void testCreateContainerEntityManagerFactory() {
		final PersistenceUnitInfoImpl persistenceUnitInfoImpl = new PersistenceUnitInfoImpl();
		persistenceUnitInfoImpl
				.setPersistenceXmlFileUrl(getClass().getResource("/META-INF/persistence.xml"));
		final EntityManagerFactory entityManagerFactory = getJpaPersistence()
				.createContainerEntityManagerFactory((PersistenceUnitInfo) persistenceUnitInfoImpl,
						Collections.emptyMap());
		assertTrue(JdbcWrapper.isProxyAlready(entityManagerFactory), "proxy");
		JpaWrapper.getJpaCounter().setDisplayed(false);
		JpaWrapper.createEntityManagerFactoryProxy(entityManagerFactory);
		JpaWrapper.getJpaCounter().setDisplayed(true);
	}

	@Test
	@Disabled
	void testCreateEntityManager() {
		final EntityManagerFactory emf = Persistence.createEntityManagerFactory("test-jse");
		emf.createEntityManager();
		JpaWrapper.getJpaCounter().setDisplayed(false);
		emf.createEntityManager();
		JpaWrapper.getJpaCounter().setDisplayed(true);
	}

	@Test
	// OpenJPA is not yet compatible with JakartaEE9
	@Disabled
	void testGenerateSchema() {
		try {
			final PersistenceUnitInfoImpl persistenceUnitInfoImpl = new PersistenceUnitInfoImpl();
			persistenceUnitInfoImpl
					.setPersistenceXmlFileUrl(getClass().getResource("/META-INF/persistence.xml"));
			getJpaPersistence().generateSchema((PersistenceUnitInfo) persistenceUnitInfoImpl,
					Collections.emptyMap());
		} catch (final Exception e) {
			assertNotNull(e, "e");
		}
		try {
			getJpaPersistence().generateSchema("test-jse", Collections.emptyMap());
		} catch (final Exception e) {
			assertNotNull(e, "e");
		}
	}
}
