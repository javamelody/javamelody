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

import static org.assertj.core.api.Assertions.assertThat;

import java.util.EventListener;
import java.util.Map;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.aop.support.annotation.AnnotationMatchingPointcut;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.boot.web.servlet.ServletListenerRegistrationBean;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Configuration;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.context.web.WebAppConfiguration;

import net.bull.javamelody.TestJavaMelodyAutoConfiguration.TestContextConfiguration;

/**
 * Test JavaMelodyAutoConfiguration.
 * @author Georg Wittberger
 */
@RunWith(SpringRunner.class)
@ContextConfiguration(classes = TestContextConfiguration.class)
@WebAppConfiguration
@TestPropertySource(properties = { "javamelody.init-parameters.log=true",
		"javamelody.init-parameters.monitoring-path=/test/path",
		"javamelody.excluded-datasources=ds1,ds2" })
public class TestJavaMelodyAutoConfiguration {
	@Autowired
	private ApplicationContext context;

	@Configuration
	@EnableAutoConfiguration
	static class TestContextConfiguration {
		// nothing
	}

	/**
	 * test.
	 */
	@SuppressWarnings("unchecked")
	@Test
	public void testJavaMelodyAutoConfigurationIsCreated() {
		// It should create a session listener.
		final ServletListenerRegistrationBean<EventListener> sessionListenerRegistrationBean = context
				.getBean(ServletListenerRegistrationBean.class);
		assertThat(sessionListenerRegistrationBean).isNotNull();

		// It should create a registration bean named "javamelody-registration".
		final Object registrationBean = context
				.getBean(JavaMelodyAutoConfiguration.REGISTRATION_BEAN_NAME);
		assertThat(registrationBean).isNotNull();
		assertThat(registrationBean).isInstanceOf(FilterRegistrationBean.class);

		// It should create a filter registration bean with the appropriately configured monitoring filter.
		final FilterRegistrationBean<MonitoringFilter> filterRegistrationBean = (FilterRegistrationBean<MonitoringFilter>) registrationBean;
		assertThat(filterRegistrationBean.getFilter()).isNotNull();
		assertThat(filterRegistrationBean.getFilter()).isInstanceOf(MonitoringFilter.class);
		assertThat(filterRegistrationBean.getInitParameters()).containsEntry("log", "true");
		assertThat(filterRegistrationBean.getInitParameters()).containsEntry("monitoring-path",
				"/test/path");
		assertThat(filterRegistrationBean.getUrlPatterns()).containsExactly("/*");

		// It should create the monitoring filter with the application type "Spring Boot".
		final MonitoringFilter monitoringFilter = filterRegistrationBean.getFilter();
		assertThat(monitoringFilter.getApplicationType()).isEqualTo("Spring Boot");

		// It should not create anymore an auto-proxy creator.
		//		final DefaultAdvisorAutoProxyCreator autoProxyCreator = context
		//				.getBean(DefaultAdvisorAutoProxyCreator.class);
		//		assertThat(autoProxyCreator).isNotNull();

		// It should create a bean post-processor for data sources.
		final SpringDataSourceBeanPostProcessor dataSourcePostProcessor = context
				.getBean(SpringDataSourceBeanPostProcessor.class);
		assertThat(dataSourcePostProcessor).isNotNull();
		// Cannot test excluded data sources since there is no public accessor for that.

		// It should create interceptors to monitor Spring services and controllers.
		final Map<String, MonitoringSpringAdvisor> springAdvisors = context
				.getBeansOfType(MonitoringSpringAdvisor.class);
		boolean monitoredWithAdvisorFound = false;
		int stereotypeAdvisorsCount = 0;
		for (final MonitoringSpringAdvisor springAdvisor : springAdvisors.values()) {
			if (springAdvisor.getPointcut() instanceof MonitoredWithAnnotationPointcut) {
				monitoredWithAdvisorFound = true;
			} else if (springAdvisor.getPointcut() instanceof AnnotationMatchingPointcut) {
				stereotypeAdvisorsCount++;
				// Maybe use synthetic @Service, @Controller and @RestController classes to check if point cuts match.
			}
		}
		assertThat(monitoredWithAdvisorFound).isTrue();
		assertThat(stereotypeAdvisorsCount).isEqualTo(3);

		// It should create a bean post-processor for rest templates.
		final SpringRestTemplateBeanPostProcessor restTemplatePostProcessor = context
				.getBean(SpringRestTemplateBeanPostProcessor.class);
		assertThat(restTemplatePostProcessor).isNotNull();
	}
}
