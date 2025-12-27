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

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.google.inject.AbstractModule;
import com.google.inject.Guice;
import com.google.inject.Injector;
import com.google.inject.Key;
import com.google.inject.Module;
import com.google.inject.name.Names;

import net.bull.javamelody.TestMonitoringSpringInterceptor.AnnotatedTest;
import net.bull.javamelody.TestMonitoringSpringInterceptor.AnnotatedTestClass;
import net.bull.javamelody.TestMonitoringSpringInterceptor.AnnotatedTestMethod;
import net.bull.javamelody.TestMonitoringSpringInterceptor.AnnotatedTestOtherClass;
import net.bull.javamelody.internal.model.Counter;

/**
 * Test unitaire de la classe MonitoringGuiceInterceptor.
 * @author Emeric Vernat
 */
class TestMonitoringGuiceInterceptor {
	private static final String REQUESTS_COUNT = "requestsCount";

	/** Check. */
	@BeforeEach
	void setUp() {
		Utils.initialize();
	}

	/** Test. */
	@Test
	void testNewInstance() {
		assertNotNull(new MonitoringGuiceInterceptor(), "new MonitoringGuiceInterceptor");
	}

	/** Test. */
	@Test
	void testGetGuiceCounter() {
		assertNotNull(MonitoringProxy.getGuiceCounter(), "getGuiceCounter");
	}

	/** Test. */
	@Test
	void testGuiceAOP() {
		final Counter guiceCounter = MonitoringProxy.getGuiceCounter();
		guiceCounter.clear();

		final Key<AnnotatedTest> annotatedTestMethodKey = Key.get(AnnotatedTest.class,
				Names.named("annotatedTestMethod"));
		final Key<AnnotatedTest> annotatedTestOtherClassKey = Key.get(AnnotatedTest.class,
				Names.named("annotatedTestOtherClass"));
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
		assertNotNull(springTestFacade.now(), "now()");
		assertSame(0, guiceCounter.getRequestsCount(), REQUESTS_COUNT);

		guiceCounter.setDisplayed(true);
		assertNotNull(springTestFacade.now(), "now()");
		assertSame(1, guiceCounter.getRequestsCount(), REQUESTS_COUNT);

		try {
			springTestFacade.throwError();
		} catch (final Error e) {
			assertSame(2, guiceCounter.getRequestsCount(), REQUESTS_COUNT);
		}

		final AnnotatedTest annotatedTestClass = injector.getInstance(AnnotatedTestClass.class);
		assertNotNull(annotatedTestClass.myMethod(), "annotatedTestClass");
		assertSame(3, guiceCounter.getRequestsCount(), REQUESTS_COUNT);

		final AnnotatedTest annotatedTestOtherClass = injector
				.getInstance(annotatedTestOtherClassKey);
		assertNotNull(annotatedTestOtherClass.myMethod(), "annotatedTestOtherClass");
		assertSame(4, guiceCounter.getRequestsCount(), REQUESTS_COUNT);

		final AnnotatedTest annotatedTestMethod = injector.getInstance(annotatedTestMethodKey);
		assertNotNull(annotatedTestMethod.myMethod(), "annotatedTestMethod");
		assertNotNull(annotatedTestMethod.myOtherMethod(), "annotatedTestMethod");
		assertSame(6, guiceCounter.getRequestsCount(), REQUESTS_COUNT);
	}
}
