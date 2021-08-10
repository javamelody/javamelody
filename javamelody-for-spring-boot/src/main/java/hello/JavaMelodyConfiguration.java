/*
 * Copyright 2008-2018 by Emeric Vernat
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
package hello;

import java.util.Arrays;
import java.util.EventListener;
import java.util.HashSet;
import java.util.Map;

import javax.servlet.DispatcherType;
import javax.servlet.FilterRegistration;
import javax.servlet.ServletContext;
import javax.sql.DataSource;

import org.springframework.aop.framework.autoproxy.DefaultAdvisorAutoProxyCreator;
import org.springframework.aop.support.Pointcuts;
import org.springframework.aop.support.annotation.AnnotationMatchingPointcut;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnWebApplication;
import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.boot.web.servlet.ServletListenerRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Controller;
import org.springframework.stereotype.Service;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.client.RestTemplate;

import net.bull.javamelody.MonitoredWithAnnotationPointcut;
import net.bull.javamelody.MonitoredWithSpring;
import net.bull.javamelody.MonitoringFilter;
import net.bull.javamelody.MonitoringSpringAdvisor;
import net.bull.javamelody.Parameter;
import net.bull.javamelody.SessionListener;
import net.bull.javamelody.SpringContext;
import net.bull.javamelody.SpringDataSourceBeanPostProcessor;
import net.bull.javamelody.SpringRestTemplateBeanPostProcessor;

/**
 * Spring Boot configuration for JavaMelody.
 *
 * @author Georg Wittberger
 * @since 1.64.0
 */
@Configuration
@ConditionalOnWebApplication
public class JavaMelodyConfiguration {
	/**
	 * Name of the FilterRegistrationBean.
	 */
	public static final String REGISTRATION_BEAN_NAME = "javamelody-registration";

	/**
	 * Registers the JavaMelody {@link SessionListener}.
	 * @param servletContext ServletContext
	 * @return ServletListenerRegistrationBean
	 */
	@Bean
	public ServletListenerRegistrationBean<EventListener> monitoringSessionListener(
			ServletContext servletContext) {
		final ServletListenerRegistrationBean<EventListener> servletListenerRegistrationBean = new ServletListenerRegistrationBean<>(
				new SessionListener());
		if (servletContext.getFilterRegistration("javamelody") != null) {
			// if webapp deployed as war in a container with MonitoringFilter and SessionListener already added by web-fragment.xml,
			// do not add again
			servletListenerRegistrationBean.setEnabled(false);
		}
		return servletListenerRegistrationBean;
	}

	/**
	 * Registers the JavaMelody {@link MonitoringFilter}.
	 * @param servletContext ServletContext
	 * @return FilterRegistrationBean
	 */
	@Bean(name = REGISTRATION_BEAN_NAME)
	public FilterRegistrationBean monitoringFilter(ServletContext servletContext) {
		final FilterRegistrationBean registrationBean = new FilterRegistrationBean();

		// Create the monitoring filter and set its configuration parameters.
		final MonitoringFilter filter = new MonitoringFilter();
		filter.setApplicationType("Spring Boot");

		// Wrap the monitoring filter in the registration bean.
		registrationBean.setFilter(filter);
		registrationBean.setAsyncSupported(true);
		registrationBean.setName("javamelody");
		registrationBean.setDispatcherTypes(DispatcherType.REQUEST, DispatcherType.ASYNC);

		// Set the initialization parameter for the monitoring filter.
		// see the list of parameters:
		// https://github.com/javamelody/javamelody/wiki/UserGuide#6-optional-parameters
		registrationBean.addInitParameter(Parameter.LOG.getCode(), Boolean.toString(true));
		// to exclude images, css, fonts and js urls from the monitoring:
		// registrationBean.addInitParameter(Parameter.URL_EXCLUDE_PATTERN.getCode(), "(/webjars/.*|/css/.*|/images/.*|/fonts/.*|/js/.*)");
		// to add basic auth:
		// registrationBean.addInitParameter(Parameter.AUTHORIZED_USERS.getCode(), "admin:pwd");
		// to change the default storage directory:
		// registrationBean.addInitParameter(Parameter.STORAGE_DIRECTORY.getCode(), "/tmp/javamelody");

		// Set the URL patterns to activate the monitoring filter for.
		registrationBean.addUrlPatterns("/*");

		final FilterRegistration filterRegistration = servletContext
				.getFilterRegistration("javamelody");
		if (filterRegistration != null) {
			// if webapp deployed as war in a container with MonitoringFilter already added by web-fragment.xml,
			// do not try to add it again
			registrationBean.setEnabled(false);
			for (final Map.Entry<String, String> entry : registrationBean.getInitParameters()
					.entrySet()) {
				filterRegistration.setInitParameter(entry.getKey(), entry.getValue());
			}
		}
		return registrationBean;
	}

	// Note: if you have auto-proxy issues, you can add the following dependency in your pom.xml:
	// <dependency>
	//   <groupId>org.springframework.boot</groupId>
	//   <artifactId>spring-boot-starter-aop</artifactId>
	// </dependency> 
	@Bean
	@ConditionalOnMissingBean(DefaultAdvisorAutoProxyCreator.class)
	public DefaultAdvisorAutoProxyCreator defaultAdvisorAutoProxyCreator() {
		return new DefaultAdvisorAutoProxyCreator();
	}

	/**
	 * Monitoring of JDBC {@link DataSource}s
	 * @param excludedDatasources Comma separated list of excluded datasources
	 * @return SpringDataSourceBeanPostProcessor
	 */
	@Bean
	public SpringDataSourceBeanPostProcessor monitoringDataSourceBeanPostProcessor(
			@Value("${javamelody.excluded-datasources:}") String excludedDatasources) {
		final SpringDataSourceBeanPostProcessor processor = new SpringDataSourceBeanPostProcessor();
		if (excludedDatasources != null && excludedDatasources.trim().length() > 0) {
			processor.setExcludedDatasources(
					new HashSet<>(Arrays.asList(excludedDatasources.split(","))));
		}
		return processor;
	}

	/**
	 * Monitoring of beans and methods having the {@link MonitoredWithSpring} annotation.
	 * @return MonitoringSpringAdvisor
	 */
	@Bean
	public MonitoringSpringAdvisor monitoringSpringAdvisor() {
		return new MonitoringSpringAdvisor(new MonitoredWithAnnotationPointcut());
	}

	/**
	 * Monitoring of beans having the {@link Service} annotation.
	 * @return MonitoringSpringAdvisor
	 */
	@Bean
	public MonitoringSpringAdvisor monitoringSpringServiceAdvisor() {
		return new MonitoringSpringAdvisor(new AnnotationMatchingPointcut(Service.class));
	}

	/**
	 * Monitoring of beans having the {@link Controller} annotation.
	 * @return MonitoringSpringAdvisor
	 */
	@Bean
	public MonitoringSpringAdvisor monitoringSpringControllerAdvisor() {
		return new MonitoringSpringAdvisor(new AnnotationMatchingPointcut(Controller.class));
	}

	/**
	 * Monitoring of beans having the {@link RestController} annotation.
	 * @return MonitoringSpringAdvisor
	 */
	@Bean
	public MonitoringSpringAdvisor monitoringSpringRestControllerAdvisor() {
		return new MonitoringSpringAdvisor(new AnnotationMatchingPointcut(RestController.class));
	}

	/**
	 * Monitoring of beans or methods having the {@link Async} annotation.
	 * @return MonitoringSpringAdvisor
	 */
	@Bean
	public MonitoringSpringAdvisor monitoringSpringAsyncAdvisor() {
		return new MonitoringSpringAdvisor(
				Pointcuts.union(new AnnotationMatchingPointcut(Async.class),
						new AnnotationMatchingPointcut(null, Async.class)));
	}

	/**
	 * Monitoring of beans methods having the {@link Scheduled} or {@link Schedules} annotations.
	 * @return MonitoringSpringAdvisor
	 */
	//	@Bean
	//	public MonitoringSpringAdvisor monitoringSpringScheduledAdvisor() {
	//		return new MonitoringSpringAdvisor(
	//				Pointcuts.union(new AnnotationMatchingPointcut(null, Scheduled.class),
	//						new AnnotationMatchingPointcut(null, Schedules.class)));
	//	}

	/**
	 * Monitoring of {@link RestTemplate} beans.
	 * @return SpringRestTemplateBeanPostProcessor
	 */
	@Bean
	public SpringRestTemplateBeanPostProcessor monitoringRestTemplateBeanPostProcessor() {
		return new SpringRestTemplateBeanPostProcessor();
	}

	/**
	 * @return Reference to the Spring context.
	 */
	@Bean
	public SpringContext javamelodySpringContext() {
		return new SpringContext();
	}
}
