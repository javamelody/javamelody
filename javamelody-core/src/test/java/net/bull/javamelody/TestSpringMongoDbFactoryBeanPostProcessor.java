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

import org.junit.Before;
import org.junit.Test;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;
import org.springframework.core.Ordered;
import org.springframework.data.mongodb.MongoDbFactory;

import net.bull.javamelody.internal.model.Counter;

/**
 * Test unitaire de la classe SpringMongoDbFactoryBeanPostProcessor.
 * @author Emeric Vernat
 */
public class TestSpringMongoDbFactoryBeanPostProcessor {
	private static final String TEST_CONTEXT_FILENAME = "spring-context.xml";
	private static final String MONITORING_CONTEXT_FILENAME = "net/bull/javamelody/monitoring-spring.xml";

	/** Check. */
	@Before
	public void setUp() {
		Utils.initialize();
	}

	/** Test. */
	@Test
	public void testSpringAOP() {
		final Counter servicesCounter = MonitoringProxy.getServicesCounter();
		servicesCounter.clear();
		final ConfigurableApplicationContext context = new ClassPathXmlApplicationContext(
				MONITORING_CONTEXT_FILENAME, TEST_CONTEXT_FILENAME);
		try {
			final MongoDbFactory mongoDbFactory = context.getBean(MongoDbFactory.class);
			assertNotNull("toString", mongoDbFactory.toString());
			assertNotNull("getDb", mongoDbFactory.getDb());

			final SpringMongoDbFactoryBeanPostProcessor springMongoDbFactoryBeanPostProcessor = context
					.getBean(SpringMongoDbFactoryBeanPostProcessor.class);
			assertEquals("order", Ordered.LOWEST_PRECEDENCE,
					springMongoDbFactoryBeanPostProcessor.getOrder());
			springMongoDbFactoryBeanPostProcessor.setOrder(1);
			assertEquals("order", 1, springMongoDbFactoryBeanPostProcessor.getOrder());
		} finally {
			context.close();
		}
	}
}
