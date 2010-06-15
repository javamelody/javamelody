/*
 * Copyright 2008-2010 by Emeric Vernat
 *
 *     This file is part of Java Melody.
 *
 * Java Melody is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Java Melody is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Java Melody.  If not, see <http://www.gnu.org/licenses/>.
 */
package net.bull.javamelody;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import net.bull.javamelody.TestMonitoringSpringInterceptor.AnnotatedTest;
import net.bull.javamelody.TestMonitoringSpringInterceptor.AnnotatedTestClass;
import net.bull.javamelody.TestMonitoringSpringInterceptor.AnnotatedTestMethod;
import net.bull.javamelody.TestMonitoringSpringInterceptor.AnnotatedTestOtherClass;

import org.junit.Before;
import org.junit.Test;

import com.google.inject.AbstractModule;
import com.google.inject.Guice;
import com.google.inject.Injector;
import com.google.inject.Key;
import com.google.inject.Module;
import com.google.inject.name.Names;

/**
 * Test unitaire de la classe MonitoringGuiceInterceptor.
 * @author Emeric Vernat
 */
public class TestMonitoringGuiceInterceptor {
	private static final String REQUESTS_COUNT = "requestsCount";

	/** Check. */
	@Before
	public void setUp() {
		Utils.initialize();
	}

	/** Test. */
	@Test
	public void testNewInstance() {
		assertNotNull("new MonitoringGuiceInterceptor", new MonitoringGuiceInterceptor());
	}

	/** Test. */
	@Test
	public void testGetGuiceCounter() {
		assertNotNull("getGuiceCounter", MonitoringProxy.getGuiceCounter());
	}

	/** Test. */
	@Test
	public void testGuiceAOP() {
		final Counter guiceCounter = MonitoringProxy.getGuiceCounter();
		guiceCounter.clear();

		final Key<AnnotatedTest> annotatedTestMethodKey = Key.get(AnnotatedTest.class, Names
				.named("annotatedTestMethod"));
		final Key<AnnotatedTest> annotatedTestOtherClassKey = Key.get(AnnotatedTest.class, Names
				.named("annotatedTestOtherClass"));
		final Module testModule = new AbstractModule() {
			/** {@inheritDoc} */
			@Override
			protected void configure() {
				// configuration du monitoring Guice
				install(new MonitoringGuiceModule());
				// implémentation de test
				bind(SpringTestFacade.class).to(SpringTestFacadeImpl.class);
				bind(AnnotatedTest.class).to(AnnotatedTestClass.class);
				bind(annotatedTestOtherClassKey).to(AnnotatedTestOtherClass.class);
				bind(annotatedTestMethodKey).to(AnnotatedTestMethod.class);
			}
		};
		final Injector injector = Guice.createInjector(testModule);
		final SpringTestFacade springTestFacade = injector.getInstance(SpringTestFacade.class);

		guiceCounter.setDisplayed(false);
		assertNotNull("now()", springTestFacade.now());
		assertSame(REQUESTS_COUNT, 0, guiceCounter.getRequestsCount());

		guiceCounter.setDisplayed(true);
		assertNotNull("now()", springTestFacade.now());
		assertSame(REQUESTS_COUNT, 1, guiceCounter.getRequestsCount());

		try {
			springTestFacade.throwError();
		} catch (final Error e) {
			assertSame(REQUESTS_COUNT, 2, guiceCounter.getRequestsCount());
		}

		final AnnotatedTest annotatedTestClass = injector.getInstance(AnnotatedTestClass.class);
		assertNotNull("annotatedTestClass", annotatedTestClass.myMethod());
		assertSame(REQUESTS_COUNT, 3, guiceCounter.getRequestsCount());

		final AnnotatedTest annotatedTestOtherClass = injector
				.getInstance(annotatedTestOtherClassKey);
		assertNotNull("annotatedTestOtherClass", annotatedTestOtherClass.myMethod());
		assertSame(REQUESTS_COUNT, 4, guiceCounter.getRequestsCount());

		final AnnotatedTest annotatedTestMethod = injector.getInstance(annotatedTestMethodKey);
		assertNotNull("annotatedTestMethod", annotatedTestMethod.myMethod());
		assertNotNull("annotatedTestMethod", annotatedTestMethod.myOtherMethod());
		assertSame(REQUESTS_COUNT, 6, guiceCounter.getRequestsCount());
	}
}
