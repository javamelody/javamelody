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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;

import org.junit.Before;
import org.junit.Test;

import net.bull.javamelody.internal.model.Counter;

/**
 * Test unitaire de la classe MonitoringProxy.
 * @author Emeric Vernat
 */
public class TestMonitoringProxy {
	/** Check. */
	@Before
	public void setUp() {
		Utils.initialize();
	}

	/** Test. */
	@Test
	public void testGetServicesCounter() {
		assertNotNull("getServicesCounter", MonitoringProxy.getServicesCounter());
	}

	/** Test. */
	@Test
	public void testProxy() {
		final Counter servicesCounter = MonitoringProxy.getServicesCounter();
		servicesCounter.clear();
		// proxy sans spring aop
		final SpringTestFacade springTestFacade = MonitoringProxy
				.createProxy(new SpringTestFacadeImpl());

		servicesCounter.setDisplayed(false);
		assertNotNull("now()", springTestFacade.now());
		final String requestsCount = "requestsCount";
		assertSame(requestsCount, 0, servicesCounter.getRequestsCount());

		servicesCounter.setDisplayed(true);
		assertNotNull("now()", springTestFacade.now());
		assertSame(requestsCount, 1, servicesCounter.getRequestsCount());

		try {
			springTestFacade.throwError();
		} catch (final Error e) {
			assertSame(requestsCount, 2, servicesCounter.getRequestsCount());
		}
		try {
			springTestFacade.throwException();
		} catch (final Exception e) {
			assertSame(requestsCount, 3, servicesCounter.getRequestsCount());
		}

		final SpringTestFacade springTestFacade2 = MonitoringProxy
				.createProxy(new SpringTestFacadeImpl(), "my facade name");
		assertNotNull("now()", springTestFacade2.now());
		assertSame(requestsCount, 4, servicesCounter.getRequestsCount());

		assertEquals("getName", "facade name",
				new MonitoringProxy(new SpringTestFacadeImpl(), "facade name").getName());
	}
}
