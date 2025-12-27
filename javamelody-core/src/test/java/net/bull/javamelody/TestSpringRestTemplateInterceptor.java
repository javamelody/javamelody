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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;
import org.springframework.core.Ordered;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.RestTemplate;

import net.bull.javamelody.internal.model.Counter;

/**
 * Test unitaire de la classe SpringRestTemplateInterceptor.
 * @author Emeric Vernat
 */
class TestSpringRestTemplateInterceptor {
	private static final String TEST_CONTEXT_FILENAME = "spring-context.xml";
	private static final String MONITORING_CONTEXT_FILENAME = "net/bull/javamelody/monitoring-spring.xml";

	/** Check. */
	@BeforeEach
	void setUp() {
		Utils.initialize();
	}

	/** Test. */
	@Test
	void testSpringAOP() {
		final Counter springCounter = MonitoringProxy.getSpringCounter();
		springCounter.clear();
		try (ConfigurableApplicationContext context = new ClassPathXmlApplicationContext(
				MONITORING_CONTEXT_FILENAME, TEST_CONTEXT_FILENAME)) {
			final RestTemplate restTemplate = context.getBean(RestTemplate.class);
			final String url = "https://gturnquist-quoters.cfapps.io/api/random";
			springCounter.setDisplayed(false);
			try {
				restTemplate.getForObject(url, Object.class);
			} catch (final RestClientException e) {
				assertSame(0, springCounter.getRequestsCount(), "requestsCount");
			}

			springCounter.setDisplayed(true);
			try {
				restTemplate.getForObject(url, Object.class);
			} catch (final RestClientException e) {
				assertSame(1, springCounter.getRequestsCount(), "requestsCount");
			}
			try {
				restTemplate.getForObject(url + "?var={0}", Object.class, "var value");
			} catch (final RestClientException e) {
				assertSame(1, springCounter.getRequestsCount(), "requestsCount");
			}

			final SpringRestTemplateBeanPostProcessor springRestTemplateBeanPostProcessor = context
					.getBean(SpringRestTemplateBeanPostProcessor.class);
			assertEquals(Ordered.LOWEST_PRECEDENCE,
					springRestTemplateBeanPostProcessor.getOrder(),
					"order");
			springRestTemplateBeanPostProcessor.setOrder(1);
			assertEquals(1, springRestTemplateBeanPostProcessor.getOrder(), "order");
		}
	}
}
