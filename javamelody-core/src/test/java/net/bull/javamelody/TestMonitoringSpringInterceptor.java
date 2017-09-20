/*
 * Copyright 2008-2017 by Emeric Vernat
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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;

import java.util.Date;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Controller;
import org.springframework.stereotype.Service;

import net.bull.javamelody.internal.model.Counter;

/**
 * Test unitaire de la classe MonitoringSpringInterceptor.
 * @author Emeric Vernat
 */
public class TestMonitoringSpringInterceptor {
	private static final String TEST_CONTEXT_FILENAME = "spring-context.xml";
	private static final String MONITORING_CONTEXT_FILENAME = "net/bull/javamelody/monitoring-spring.xml";
	private static final String MONITORING_CONTEXT_FILENAME2 = "net/bull/javamelody/monitoring-spring-scheduled.xml";
	private static final String REQUESTS_COUNT = "requestsCount";

	private ConfigurableApplicationContext context;

	/** Check. */
	@Before
	public void setUp() {
		Utils.initialize();
		this.context = new ClassPathXmlApplicationContext(
				new String[] { MONITORING_CONTEXT_FILENAME, MONITORING_CONTEXT_FILENAME2,
						TEST_CONTEXT_FILENAME, });
	}

	@After
	public void destroy() {
		this.context.close();
	}

	/**
	 * Test.
	 */
	public interface ITest {
		/**
		 * Test.
		 * @return Date
		 */
		Date myMethod();

		/**
		 * Test.
		 * @return Date
		 */
		Date myOtherMethod();
	}

	/**
	 * Test.
	 */
	public interface ScheduledTest {
		/**
		 * Test.
		 * @return Date
		 */
		@Scheduled
		Date myMethod();

		/**
		 * Test.
		 * @return Date
		 */
		Date myOtherMethod();
	}

	/**
	 * Test.
	 */
	public interface AnnotatedTest {
		/**
		 * Test.
		 * @return Date
		 */
		@MonitoredWithSpring
		Date myMethod();

		/**
		 * Test.
		 * @return Date
		 */
		@MonitoredWithSpring(name = "test method")
		Date myOtherMethod();
	}

	/**
	 * Test.
	 */
	@MonitoredWithSpring(name = "test interface")
	public interface AnnotatedTest2 {
		/**
		 * Test.
		 * @return Date
		 */
		Date myMethod();

		/**
		 * Test.
		 * @return Date
		 */
		Date myOtherMethod();
	}

	/**
	 * Test.
	 */
	@MonitoredWithSpring
	@MonitoredWithGuice
	public static class AnnotatedTestClass implements AnnotatedTest {
		/**
		 * {@inheritDoc}
		 */
		@Override
		public Date myMethod() {
			return new Date();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public Date myOtherMethod() {
			return new Date();
		}
	}

	/**
	 * Test.
	 */
	public static class AnnotatedTestClass2 implements AnnotatedTest2 {
		/**
		 * {@inheritDoc}
		 */
		@Override
		public Date myMethod() {
			return new Date();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public Date myOtherMethod() {
			return new Date();
		}
	}

	/**
	 * Test.
	 */
	@MonitoredWithSpring(name = "test class")
	@MonitoredWithGuice(name = "test class")
	public static class AnnotatedTestOtherClass implements AnnotatedTest {
		/**
		 * {@inheritDoc}
		 */
		@Override
		public Date myMethod() {
			return new Date();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public Date myOtherMethod() {
			return new Date();
		}
	}

	/**
	 * Test.
	 */
	public static class AnnotatedTestMethod implements AnnotatedTest {
		/**
		 * {@inheritDoc}
		 */
		@Override
		@MonitoredWithGuice
		public Date myMethod() {
			return new Date();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		@MonitoredWithGuice(name = "test method")
		public Date myOtherMethod() {
			return new Date();
		}
	}

	/**
	 * Test.
	 */
	@Controller
	public static class AnnotatedTestController implements ITest {
		/**
		 * {@inheritDoc}
		 */
		@Override
		public Date myMethod() {
			return new Date();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public Date myOtherMethod() {
			return new Date();
		}
	}

	/**
	 * Test.
	 */
	@Service
	public static class AnnotatedTestService implements ITest {
		/**
		 * {@inheritDoc}
		 */
		@Override
		public Date myMethod() {
			return new Date();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public Date myOtherMethod() {
			return new Date();
		}
	}

	/**
	 * Test.
	 */
	@Async
	public static class AnnotatedTestAsync implements ITest {
		/**
		 * {@inheritDoc}
		 */
		@Override
		public Date myMethod() {
			return new Date();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public Date myOtherMethod() {
			return new Date();
		}
	}

	/**
	 * Test.
	 */
	public static class AnnotatedTestScheduled implements ScheduledTest {
		/**
		 * {@inheritDoc}
		 */
		@Override
		public Date myMethod() {
			return new Date();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public Date myOtherMethod() {
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
			assertFalse("methodMatcher.matches",
					pointcut.getMethodMatcher().matches(null, null, (Object[]) null));
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

		final AnnotatedTest2 annotatedTestClassSpring2 = (AnnotatedTest2) context
				.getBean("annotatedTestClassSpring2");
		assertNotNull("annotatedTestClassSpring2", annotatedTestClassSpring2.myMethod());
		assertSame(REQUESTS_COUNT, 4, springCounter.getRequestsCount());

		final AnnotatedTest annotatedTestOtherClassSpring = (AnnotatedTest) context
				.getBean("annotatedTestOtherClassSpring");
		assertNotNull("annotatedTestOtherClassSpring", annotatedTestOtherClassSpring.myMethod());
		assertSame(REQUESTS_COUNT, 5, springCounter.getRequestsCount());

		final AnnotatedTest annotatedTestMethodSpring = (AnnotatedTest) context
				.getBean("annotatedTestMethodSpring");
		assertNotNull("annotatedTestMethodSpring", annotatedTestMethodSpring.myMethod());
		assertNotNull("annotatedTestMethodSpring", annotatedTestMethodSpring.myOtherMethod());
		assertSame(REQUESTS_COUNT, 7, springCounter.getRequestsCount());

		final ITest annotatedTestController = (ITest) context.getBean("annotatedTestController");
		assertNotNull("annotatedTestController", annotatedTestController.myMethod());
		assertNotNull("annotatedTestController", annotatedTestController.myOtherMethod());
		assertSame(REQUESTS_COUNT, 9, springCounter.getRequestsCount());

		final ITest annotatedTestService = (ITest) context.getBean("annotatedTestService");
		assertNotNull("annotatedTestService", annotatedTestService.myMethod());
		assertNotNull("annotatedTestService", annotatedTestService.myOtherMethod());
		assertSame(REQUESTS_COUNT, 11, springCounter.getRequestsCount());

		final ITest annotatedTestAsync = (ITest) context.getBean("annotatedTestAsync");
		assertNotNull("annotatedTestAsync", annotatedTestAsync.myMethod());
		assertNotNull("annotatedTestAsync", annotatedTestAsync.myOtherMethod());
		assertSame(REQUESTS_COUNT, 13, springCounter.getRequestsCount());

		final ScheduledTest annotatedTestScheduled = (ScheduledTest) context
				.getBean("annotatedTestScheduled");
		assertNotNull("annotatedTestScheduled", annotatedTestScheduled.myMethod());
		assertNotNull("annotatedTestScheduled", annotatedTestScheduled.myOtherMethod());
		assertSame(REQUESTS_COUNT, 14, springCounter.getRequestsCount());
	}

	/** Test. */
	@Test
	public void testSpringDataSourceBeanPostProcessor() {
		// utilisation de l'InvocationHandler dans SpringDataSourceBeanPostProcessor
		context.getType("dataSource2");
		context.getBean("dataSource2");

		Utils.setProperty(Parameter.NO_DATABASE, "true");
		assertNotNull("no database context", context);
	}

	/** Test. */
	@Test
	public void testSpringDataSourceFactoryBean() {
		// utilisation de l'InvocationHandler dans SpringDataSourceFactoryBean
		context.getType("wrappedDataSource");
		context.getBean("wrappedDataSource");

		try {
			new SpringDataSourceFactoryBean().createInstance();
		} catch (final IllegalStateException e) {
			assertNotNull("ok", e);
		}
	}
}
