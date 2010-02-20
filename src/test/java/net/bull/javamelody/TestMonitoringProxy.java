/*
 * Copyright 2008-2009 by Emeric Vernat, Bull
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
import static org.junit.Assert.assertTrue;

import org.junit.Test;

/**
 * Test unitaire de la classe MonitoringProxy.
 * @author Emeric Vernat
 */
public class TestMonitoringProxy {
	/** Test. */
	@Test
	public void testGetServicesCounter() {
		assertNotNull("getServicesCounter", MonitoringProxy.getServicesCounter());
	}

	/** Test.
	 * @throws Exception e */
	@Test
	public void testProxy() throws Exception {
		final Counter servicesCounter = MonitoringProxy.getServicesCounter();
		servicesCounter.clear();
		// proxy sans spring aop
		final SpringTestFacade springTestFacade = MonitoringProxy
				.createProxy(new SpringTestFacadeImpl());

		servicesCounter.setDisplayed(false);
		assertNotNull("now()", springTestFacade.now());
		assertTrue("requestsCount", servicesCounter.getRequestsCount() == 0);

		servicesCounter.setDisplayed(true);
		assertNotNull("now()", springTestFacade.now());
		assertTrue("requestsCount", servicesCounter.getRequestsCount() == 1);

		try {
			springTestFacade.throwError();
		} catch (final Error e) {
			assertTrue("requestsCount", servicesCounter.getRequestsCount() == 2);
		}
	}
}
