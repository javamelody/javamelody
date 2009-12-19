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
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.springframework.context.ApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;

/**
 * Test unitaire de la classe MonitoringSpringInterceptor.
 * @author Emeric Vernat
 */
public class TestMonitoringSpringInterceptor {
	/** Test. */
	@Test
	public void testNewInstance() {
		assertNotNull("new MonitoringSpringInterceptor", new MonitoringSpringInterceptor());
	}

	/** Test. */
	@Test
	public void testGetSpringCounter() {
		assertNotNull("getSpringCounter", MonitoringProxy.getSpringCounter());
	}

	/** Test. */
	@Test
	public void testMonitoredWithAnnotationPointcut() {
		final MonitoredWithAnnotationPointcut pointcut = new MonitoredWithAnnotationPointcut();
		assertNotNull("new MonitoredWithAnnotationPointcut", pointcut);
		assertNotNull("classFilter", pointcut.getClassFilter());
		assertNotNull("methodMatcher", pointcut.getMethodMatcher());
	}

	/** Test. 
	 * @throws ClassNotFoundException e */
	@Test
	public void testMonitoredWithInterfacePointcut() throws ClassNotFoundException {
		final MonitoredWithInterfacePointcut pointcut = new MonitoredWithInterfacePointcut();
		assertNotNull("new MonitoredWithInterfacePointcut", pointcut);
		assertNotNull("classFilter", pointcut.getClassFilter());
		assertNotNull("methodMatcher", pointcut.getMethodMatcher());
		assertNull("interfaceName", pointcut.getInterfaceName());
		pointcut.setInterfaceName(SpringTestFacade.class.getName());
		assertNotNull("interfaceName", pointcut.getInterfaceName());
	}

	/** Test.
	 * @throws Exception e */
	@Test
	public void testSpringAOP() throws Exception {
		final Counter springCounter = MonitoringProxy.getSpringCounter();
		springCounter.clear();
		final ApplicationContext context = new ClassPathXmlApplicationContext(new String[] {
				"net/bull/javamelody/monitoring-spring.xml", "spring-context.xml", });
		final SpringTestFacade springTestFacade = (SpringTestFacade) context
				.getBean("springTestFacade");

		springCounter.setDisplayed(false);
		assertNotNull("now()", springTestFacade.now());
		assertTrue("requestsCount", springCounter.getRequestsCount() == 0);

		springCounter.setDisplayed(true);
		assertNotNull("now()", springTestFacade.now());
		assertTrue("requestsCount", springCounter.getRequestsCount() == 1);

		try {
			springTestFacade.throwError();
		} catch (final Error e) {
			assertTrue("requestsCount", springCounter.getRequestsCount() == 2);
		}
	}
}
