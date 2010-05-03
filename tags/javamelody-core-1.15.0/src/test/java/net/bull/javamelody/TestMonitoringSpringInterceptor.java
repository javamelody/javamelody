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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;

import java.util.Date;

import org.junit.Test;
import org.springframework.context.ApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;

/**
 * Test unitaire de la classe MonitoringSpringInterceptor.
 * @author Emeric Vernat
 */
public class TestMonitoringSpringInterceptor {
	private static final String REQUESTS_COUNT = "requestsCount";

	/**
	 * Test.
	 */
	public interface AnnotatedTest {
		/**
		 * Test.
		 * @return Date
		 */
		@MonitoredWithSpring(name = "test method")
		Date myMethod();
	}

	/**
	 * Test.
	 */
	@MonitoredWithSpring(name = "test class")
	public static class AnnotatedTestClassSpring implements AnnotatedTest {
		/**
		 * Test.
		 * @return Date
		 */
		public Date myMethod() {
			return new Date();
		}
	}

	/**
	 * Test.
	 */
	public static class AnnotatedTestMethodSpring implements AnnotatedTest {
		/**
		 * Test.
		 * @return Date
		 */
		public Date myMethod() {
			return new Date();
		}
	}

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
		assertFalse("methodMatcher.isRuntime", pointcut.getMethodMatcher().isRuntime());
		try {
			assertFalse("methodMatcher.matches", pointcut.getMethodMatcher().matches(null, null,
					null));
		} catch (final UnsupportedOperationException e) {
			assertNotNull("ok", e);
		}
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

	/** Test. */
	@Test
	public void testSpringAOP() {
		final Counter springCounter = MonitoringProxy.getSpringCounter();
		springCounter.clear();
		final ApplicationContext context = new ClassPathXmlApplicationContext(new String[] {
				"net/bull/javamelody/monitoring-spring.xml", "spring-context.xml", });
		final SpringTestFacade springTestFacade = (SpringTestFacade) context
				.getBean("springTestFacade");

		springCounter.setDisplayed(false);
		assertNotNull("now()", springTestFacade.now());
		assertSame(REQUESTS_COUNT, 0, springCounter.getRequestsCount());

		springCounter.setDisplayed(true);
		assertNotNull("now()", springTestFacade.now());
		assertSame(REQUESTS_COUNT, 1, springCounter.getRequestsCount());

		try {
			springTestFacade.throwError();
		} catch (final Error e) {
			assertSame(REQUESTS_COUNT, 2, springCounter.getRequestsCount());
		}

		final AnnotatedTest annotatedTestClassSpring = (AnnotatedTest) context
				.getBean("annotatedTestClassSpring");
		assertNotNull("annotatedTestClassSpring", annotatedTestClassSpring.myMethod());
		assertSame(REQUESTS_COUNT, 3, springCounter.getRequestsCount());

		final AnnotatedTest annotatedTestMethodSpring = (AnnotatedTest) context
				.getBean("annotatedTestMethodSpring");
		assertNotNull("annotatedTestMethodSpring", annotatedTestMethodSpring.myMethod());
		assertSame(REQUESTS_COUNT, 4, springCounter.getRequestsCount());

		// utilisation de l'InvocationHandler dans SpringDataSourceBeanPostProcessor
		context.getType("dataSource2");
	}
}
