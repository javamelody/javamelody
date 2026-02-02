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

import java.util.Date;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;
import org.springframework.core.Ordered;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Controller;
import org.springframework.stereotype.Service;

import net.bull.javamelody.internal.model.Counter;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Test unitaire de la classe MonitoringSpringInterceptor.
 * @author Emeric Vernat
 */
class TestMonitoringSpringInterceptor {
	private static final String TEST_CONTEXT_FILENAME = "spring-context.xml";
	private static final String MONITORING_CONTEXT_FILENAME = "net/bull/javamelody/monitoring-spring.xml";
	private static final String MONITORING_CONTEXT_FILENAME2 = "net/bull/javamelody/monitoring-spring-scheduled.xml";
	private static final String REQUESTS_COUNT = "requestsCount";

	private ConfigurableApplicationContext context;

	/** Check. */
	@BeforeEach
	void setUp() {
		Utils.initialize();
		this.context = new ClassPathXmlApplicationContext(MONITORING_CONTEXT_FILENAME,
				MONITORING_CONTEXT_FILENAME2, TEST_CONTEXT_FILENAME);
	}

	@AfterEach
	void destroy() {
		this.context.close();
	}

	/**
	 * Test.
	 */
	interface ITest {
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
	interface ScheduledTest {
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
	interface AnnotatedTest {
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
	interface AnnotatedTest2 {
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
	static class AnnotatedTestClass implements AnnotatedTest {
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
	static class AnnotatedTestClass2 implements AnnotatedTest2 {
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
	static class AnnotatedTestOtherClass implements AnnotatedTest {
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
	static class AnnotatedTestMethod implements AnnotatedTest {
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
	static class AnnotatedTestController implements ITest {
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
	static class AnnotatedTestService implements ITest {
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
	static class AnnotatedTestAsync implements ITest {
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
	static class AnnotatedTestScheduled implements ScheduledTest {
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
	void testNewInstance() {
		assertNotNull(new MonitoringSpringInterceptor(), "new MonitoringSpringInterceptor");
	}

	/** Test. */
	@Test
	void testGetSpringCounter() {
		assertNotNull(MonitoringProxy.getSpringCounter(), "getSpringCounter");
	}

	/** Test. */
	@Test
	void testMonitoredWithAnnotationPointcut() {
		final MonitoredWithAnnotationPointcut pointcut = new MonitoredWithAnnotationPointcut();
		assertNotNull(pointcut, "new MonitoredWithAnnotationPointcut");
		assertNotNull(pointcut.getClassFilter(), "classFilter");
		assertNotNull(pointcut.getMethodMatcher(), "methodMatcher");
		assertFalse(pointcut.getMethodMatcher().isRuntime(), "methodMatcher.isRuntime");
	}

	/** Test.
	 * @throws ClassNotFoundException e */
	@Test
	void testMonitoredWithInterfacePointcut() throws ClassNotFoundException {
		final MonitoredWithInterfacePointcut pointcut = new MonitoredWithInterfacePointcut();
		assertNotNull(pointcut, "new MonitoredWithInterfacePointcut");
		assertNotNull(pointcut.getClassFilter(), "classFilter");
		assertNotNull(pointcut.getMethodMatcher(), "methodMatcher");
		assertNull(pointcut.getInterfaceName(), "interfaceName");
		pointcut.setInterfaceName(SpringTestFacade.class.getName());
		assertNotNull(pointcut.getInterfaceName(), "interfaceName");
	}

	/** Test. */
	@Test
	void testSpringAOP() {
		final Counter springCounter = MonitoringProxy.getSpringCounter();
		springCounter.clear();
		final SpringTestFacade springTestFacade = (SpringTestFacade) context
				.getBean("springTestFacade");

		springCounter.setDisplayed(false);
		assertNotNull(springTestFacade.now(), "now()");
		assertSame(0, springCounter.getRequestsCount(), REQUESTS_COUNT);

		springCounter.setDisplayed(true);
		assertNotNull(springTestFacade.now(), "now()");
		assertSame(1, springCounter.getRequestsCount(), REQUESTS_COUNT);

		assertThrows(Error.class, () ->	springTestFacade.throwError());
		assertSame(2, springCounter.getRequestsCount(), REQUESTS_COUNT);

		final AnnotatedTest annotatedTestClassSpring = (AnnotatedTest) context
				.getBean("annotatedTestClassSpring");
		assertNotNull(annotatedTestClassSpring.myMethod(), "annotatedTestClassSpring");
		assertSame(3, springCounter.getRequestsCount(), REQUESTS_COUNT);

		final AnnotatedTest2 annotatedTestClassSpring2 = (AnnotatedTest2) context
				.getBean("annotatedTestClassSpring2");
		assertNotNull(annotatedTestClassSpring2.myMethod(), "annotatedTestClassSpring2");
		assertSame(4, springCounter.getRequestsCount(), REQUESTS_COUNT);

		final AnnotatedTest annotatedTestOtherClassSpring = (AnnotatedTest) context
				.getBean("annotatedTestOtherClassSpring");
		assertNotNull(annotatedTestOtherClassSpring.myMethod(), "annotatedTestOtherClassSpring");
		assertSame(5, springCounter.getRequestsCount(), REQUESTS_COUNT);

		final AnnotatedTest annotatedTestMethodSpring = (AnnotatedTest) context
				.getBean("annotatedTestMethodSpring");
		assertNotNull(annotatedTestMethodSpring.myMethod(), "annotatedTestMethodSpring");
		assertNotNull(annotatedTestMethodSpring.myOtherMethod(), "annotatedTestMethodSpring");
		assertSame(7, springCounter.getRequestsCount(), REQUESTS_COUNT);

		final ITest annotatedTestController = (ITest) context.getBean("annotatedTestController");
		assertNotNull(annotatedTestController.myMethod(), "annotatedTestController");
		assertNotNull(annotatedTestController.myOtherMethod(), "annotatedTestController");
		assertSame(9, springCounter.getRequestsCount(), REQUESTS_COUNT);

		final ITest annotatedTestService = (ITest) context.getBean("annotatedTestService");
		assertNotNull(annotatedTestService.myMethod(), "annotatedTestService");
		assertNotNull(annotatedTestService.myOtherMethod(), "annotatedTestService");
		assertSame(11, springCounter.getRequestsCount(), REQUESTS_COUNT);

		final ITest annotatedTestAsync = (ITest) context.getBean("annotatedTestAsync");
		assertNotNull(annotatedTestAsync.myMethod(), "annotatedTestAsync");
		assertNotNull(annotatedTestAsync.myOtherMethod(), "annotatedTestAsync");
		assertSame(13, springCounter.getRequestsCount(), REQUESTS_COUNT);

		final ScheduledTest annotatedTestScheduled = (ScheduledTest) context
				.getBean("annotatedTestScheduled");
		assertNotNull(annotatedTestScheduled.myMethod(), "annotatedTestScheduled");
		assertNotNull(annotatedTestScheduled.myOtherMethod(), "annotatedTestScheduled");
		assertSame(14, springCounter.getRequestsCount(), REQUESTS_COUNT);
	}

	/** Test. */
	@Test
	void testSpringDataSourceBeanPostProcessor() {
		// utilisation de l'InvocationHandler dans SpringDataSourceBeanPostProcessor
		context.getType("dataSource2");
		context.getBean("dataSource2");

		final SpringDataSourceBeanPostProcessor springDataSourceBeanPostProcessor = (SpringDataSourceBeanPostProcessor) context
				.getBean("springDataSourceBeanPostProcessor");
		assertEquals(Ordered.LOWEST_PRECEDENCE, springDataSourceBeanPostProcessor.getOrder(), "getOrder");
		springDataSourceBeanPostProcessor.setOrder(1);
		assertEquals(1, springDataSourceBeanPostProcessor.getOrder(), "getOrder");

		Utils.setProperty(Parameter.NO_DATABASE, "true");
		assertNotNull(context, "no database context");
	}

	/** Test. */
	@Test
	void testSpringDataSourceFactoryBean() {
		// utilisation de l'InvocationHandler dans SpringDataSourceFactoryBean
		context.getType("wrappedDataSource");
		context.getBean("wrappedDataSource");

		assertThrows(IllegalStateException.class, () ->	new SpringDataSourceFactoryBean().createInstance());
	}
}
