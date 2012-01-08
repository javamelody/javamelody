/*
 * Copyright 2008-2012 by Emeric Vernat
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

import org.junit.Before;
import org.junit.Test;

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

		final SpringTestFacade springTestFacade2 = MonitoringProxy.createProxy(
				new SpringTestFacadeImpl(), "my facade name");
		assertNotNull("now()", springTestFacade2.now());
		assertSame(requestsCount, 4, servicesCounter.getRequestsCount());
	}
}
