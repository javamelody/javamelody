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

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import net.bull.javamelody.internal.model.Counter;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Test unitaire de la classe MonitoringProxy.
 * @author Emeric Vernat
 */
class TestMonitoringProxy {
	/** Check. */
	@BeforeEach
	void setUp() {
		Utils.initialize();
	}

	/** Test. */
	@Test
	void testGetServicesCounter() {
		assertNotNull(MonitoringProxy.getServicesCounter(), "getServicesCounter");
	}

	/** Test. */
	@Test
	void testProxy() {
		final Counter servicesCounter = MonitoringProxy.getServicesCounter();
		servicesCounter.clear();
		// proxy sans spring aop
		final SpringTestFacade springTestFacade = MonitoringProxy
				.createProxy(new SpringTestFacadeImpl());

		servicesCounter.setDisplayed(false);
		assertNotNull(springTestFacade.now(), "now()");
		final String requestsCount = "requestsCount";
		assertSame(0, servicesCounter.getRequestsCount(), requestsCount);

		servicesCounter.setDisplayed(true);
		assertNotNull(springTestFacade.now(), "now()");
		assertSame(1, servicesCounter.getRequestsCount(), requestsCount);

		assertThrows(Error.class, () -> springTestFacade.throwError());
		assertSame(2, servicesCounter.getRequestsCount(), requestsCount);
		assertThrows(Exception.class, () -> springTestFacade.throwException());
		assertSame(3, servicesCounter.getRequestsCount(), requestsCount);

		final SpringTestFacade springTestFacade2 = MonitoringProxy
				.createProxy(new SpringTestFacadeImpl(), "my facade name");
		assertNotNull(springTestFacade2.now(), "now()");
		assertSame(4, servicesCounter.getRequestsCount(), requestsCount);

		assertEquals("facade name",
				new MonitoringProxy(new SpringTestFacadeImpl(), "facade name").getName(), "getName");
	}
}
