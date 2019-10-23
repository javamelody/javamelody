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

import java.io.IOException;
import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.EventListener;
import java.util.HashSet;
import java.util.Map;
import java.util.Map.Entry;

import javax.servlet.DispatcherType;
import javax.servlet.FilterRegistration;
import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.sql.DataSource;

import org.aopalliance.intercept.MethodInvocation;
import org.springframework.aop.Pointcut;
import org.springframework.aop.framework.autoproxy.DefaultAdvisorAutoProxyCreator;
import org.springframework.aop.support.Pointcuts;
import org.springframework.aop.support.annotation.AnnotationMatchingPointcut;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.autoconfigure.condition.ConditionalOnWebApplication;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.boot.web.servlet.ServletListenerRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.scheduling.annotation.Schedules;
import org.springframework.stereotype.Controller;
import org.springframework.stereotype.Service;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.client.RestTemplate;

/**
 * Spring Boot auto-configuration for JavaMelody.
 *
 * <p>
 * This class is picked up by the Spring Boot auto-configuration mechanism and creates the beans required to set up JavaMelody.
 * Configuration values are injected using {@link JavaMelodyConfigurationProperties}.
 * </p>
 *
 * <p>
 * The auto-configured filter can be overridden by defining a custom {@link FilterRegistrationBean} with the name
 * "javamelody-registration" in the application context.
 * </p>
 *
 * <p>
 * The configuration is enabled for web applications by default. It is possible to opt-out of the auto-configuration by
 * setting the application configuration "javamelody.enabled" to the value "false".
 * </p>
 *
 * @author Georg Wittberger, Emeric Vernat
 * @since 1.64.0
 */
@Configuration
@EnableConfigurationProperties(JavaMelodyConfigurationProperties.class)
@ConditionalOnWebApplication
@ConditionalOnProperty(prefix = JavaMelodyConfigurationProperties.PREFIX, name = "enabled", matchIfMissing = true)
public class JavaMelodyAutoConfiguration {
	/**
	 * Name of the FilterRegistrationBean.
	 */
	public static final String REGISTRATION_BEAN_NAME = "javamelody-registration";

	private final MonitoredWithAnnotationPointcut monitoredWithSpringAnnotationPointcut = new MonitoredWithAnnotationPointcut();

	private final Pointcut asyncAnnotationPointcut = Pointcuts.union(
			new AnnotationMatchingPointcut(Async.class),
			new AnnotationMatchingPointcut(null, Async.class));

	private final Pointcut scheduledAnnotationPointcut = Pointcuts.union(
			new AnnotationMatchingPointcut(null, Scheduled.class),
			new AnnotationMatchingPointcut(null, Schedules.class));

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
	 * Registers the JavaMelody {@link MonitoringFilter}. The filter can be overridden completely by creating a custom
	 * {@link FilterRegistrationBean} with the name "javamelody-registration" in the application context.
	 * @param properties JavaMelodyConfigurationProperties
	 * @param servletContext ServletContext
	 * @return FilterRegistrationBean
	 */
	@Bean(name = REGISTRATION_BEAN_NAME)
	@ConditionalOnMissingBean(name = REGISTRATION_BEAN_NAME)
	public FilterRegistrationBean<MonitoringFilter> monitoringFilter(
			JavaMelodyConfigurationProperties properties, ServletContext servletContext) {
		final FilterRegistrationBean<MonitoringFilter> registrationBean = new FilterRegistrationBean<>();

		// Create the monitoring filter and set its configuration parameters.
		final MonitoringFilter filter;
		if (properties.isManagementEndpointMonitoringEnabled()) {
			// if the management endpoint is enabled, disable the /monitoring reports on the application port
			filter = new MonitoringFilter() {
				@Override
				protected boolean isAllowed(HttpServletRequest request,
						HttpServletResponse response) throws IOException {
					response.sendError(HttpServletResponse.SC_FORBIDDEN, "Forbidden access");
					return false;
				}
			};
		} else {
			filter = new MonitoringFilter();
		}
		filter.setApplicationType("Spring Boot");

		// Wrap the monitoring filter in the registration bean.
		registrationBean.setFilter(filter);
		registrationBean.setAsyncSupported(true);
		registrationBean.setName("javamelody");
		registrationBean.setDispatcherTypes(DispatcherType.REQUEST, DispatcherType.ASYNC);

		// Set the initialization parameter for the monitoring filter.
		for (final Entry<String, String> parameter : properties.getInitParameters().entrySet()) {
			registrationBean.addInitParameter(parameter.getKey(), parameter.getValue());
		}

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

	/**
	 * When enabled, management endpoint for /monitoring reports on the management http port instead of the application http port.
	 * @param servletContext ServletContext
	 * @return MonitoringEndpoint
	 */
	@Bean
	@ConditionalOnProperty(prefix = JavaMelodyConfigurationProperties.PREFIX, name = "management-endpoint-monitoring-enabled", matchIfMissing = false)
	public MonitoringEndpoint monitoringEndpoint(final ServletContext servletContext) {
		return new MonitoringEndpoint(servletContext);
	}

	/**
	 * Now disabled by default, since dependency spring-boot-starter-aop was added in 1.76.
	 * @return DefaultAdvisorAutoProxyCreator
	 */
	@Bean
	@ConditionalOnMissingBean(DefaultAdvisorAutoProxyCreator.class)
	@ConditionalOnProperty(prefix = JavaMelodyConfigurationProperties.PREFIX, name = "advisor-auto-proxy-creator-enabled", matchIfMissing = false)
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
		// IMPORTANT: We cannot inject JavaMelodyConfigurationProperties here because of bean load order! Therefore we have
		// to use that rather dirty way to inject the configuration value.
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
	@ConditionalOnProperty(prefix = JavaMelodyConfigurationProperties.PREFIX, name = "spring-monitoring-enabled", matchIfMissing = true)
	public MonitoringSpringAdvisor monitoringSpringAdvisor() {
		return new MonitoringSpringAdvisor(monitoredWithSpringAnnotationPointcut);
	}

	/**
	 * Monitoring of beans having the {@link Service} annotation.
	 * @return MonitoringSpringAdvisor
	 */
	@Bean
	@ConditionalOnProperty(prefix = JavaMelodyConfigurationProperties.PREFIX, name = "spring-monitoring-enabled", matchIfMissing = true)
	public MonitoringSpringAdvisor monitoringSpringServiceAdvisor() {
		return createMonitoringSpringAdvisorWithExclusions(
				new AnnotationMatchingPointcut(Service.class),
				monitoredWithSpringAnnotationPointcut, asyncAnnotationPointcut,
				scheduledAnnotationPointcut);
	}

	/**
	 * Monitoring of beans having the {@link Controller} annotation.
	 * @return MonitoringSpringAdvisor
	 */
	@Bean
	@ConditionalOnProperty(prefix = JavaMelodyConfigurationProperties.PREFIX, name = "spring-monitoring-enabled", matchIfMissing = true)
	public MonitoringSpringAdvisor monitoringSpringControllerAdvisor() {
		return createMonitoringSpringAdvisorWithExclusions(
				new AnnotationMatchingPointcut(Controller.class),
				monitoredWithSpringAnnotationPointcut, asyncAnnotationPointcut,
				scheduledAnnotationPointcut);
	}

	/**
	 * Monitoring of beans having the {@link RestController} annotation.
	 * @return MonitoringSpringAdvisor
	 */
	@Bean
	@ConditionalOnProperty(prefix = JavaMelodyConfigurationProperties.PREFIX, name = "spring-monitoring-enabled", matchIfMissing = true)
	public MonitoringSpringAdvisor monitoringSpringRestControllerAdvisor() {
		return createMonitoringSpringAdvisorWithExclusions(
				new AnnotationMatchingPointcut(RestController.class),
				monitoredWithSpringAnnotationPointcut, asyncAnnotationPointcut,
				scheduledAnnotationPointcut);
	}

	/**
	 * Monitoring of beans or methods having the {@link Async} annotation.
	 * @return MonitoringSpringAdvisor
	 */
	@Bean
	@ConditionalOnProperty(prefix = JavaMelodyConfigurationProperties.PREFIX, name = "spring-monitoring-enabled", matchIfMissing = true)
	public MonitoringSpringAdvisor monitoringSpringAsyncAdvisor() {
		return createMonitoringSpringAdvisorWithExclusions(asyncAnnotationPointcut,
				monitoredWithSpringAnnotationPointcut, scheduledAnnotationPointcut);
	}

	/**
	 * Monitoring of beans methods having the {@link Scheduled} or {@link Schedules} annotations.
	 * @return MonitoringSpringAdvisor
	 */
	@Bean
	@ConditionalOnProperty(prefix = JavaMelodyConfigurationProperties.PREFIX, name = "scheduled-monitoring-enabled", matchIfMissing = true)
	@ConditionalOnMissingBean(DefaultAdvisorAutoProxyCreator.class)
	public MonitoringSpringAdvisor monitoringSpringScheduledAdvisor() {
		return createMonitoringSpringAdvisorWithExclusions(scheduledAnnotationPointcut,
				monitoredWithSpringAnnotationPointcut, asyncAnnotationPointcut);
	}

	private MonitoringSpringAdvisor createMonitoringSpringAdvisorWithExclusions(Pointcut pointcut,
			Pointcut... excludedPointcuts) {
		final Pointcut myPointcut;
		if (excludedPointcuts.length == 0) {
			myPointcut = pointcut;
		} else {
			Pointcut excludedPointcut = excludedPointcuts[0];
			if (excludedPointcuts.length > 1) {
				for (int i = 1; i < excludedPointcuts.length; i++) {
					excludedPointcut = Pointcuts.union(excludedPointcut, excludedPointcuts[i]);
				}
			}
			myPointcut = new ExcludingPointcut(pointcut).exclude(excludedPointcut);
		}
		return new MonitoringSpringAdvisor(myPointcut);
	}

	/**
	 * Monitoring of Feign clients.
	 * @return MonitoringSpringAdvisor
	 * @throws ClassNotFoundException should not happen
	 */
	@SuppressWarnings("unchecked")
	@Bean
	@ConditionalOnClass(name = "org.springframework.cloud.openfeign.FeignClient")
	@ConditionalOnProperty(prefix = JavaMelodyConfigurationProperties.PREFIX, name = "spring-monitoring-enabled", matchIfMissing = true)
	// we check that the DefaultAdvisorAutoProxyCreator above is not enabled, because if it's enabled then feign calls are counted twice
	@ConditionalOnMissingBean(DefaultAdvisorAutoProxyCreator.class)
	public MonitoringSpringAdvisor monitoringFeignClientAdvisor() throws ClassNotFoundException {
		final Class<? extends Annotation> feignClientClass = (Class<? extends Annotation>) Class
				.forName("org.springframework.cloud.openfeign.FeignClient");
		final MonitoringSpringAdvisor advisor = new MonitoringSpringAdvisor(
				new AnnotationMatchingPointcut(feignClientClass, true));
		advisor.setAdvice(new MonitoringSpringInterceptor() {
			private static final long serialVersionUID = 1L;

			@Override
			protected String getRequestName(MethodInvocation invocation) {
				final StringBuilder sb = new StringBuilder();
				final Method method = invocation.getMethod();
				final RequestMapping requestMapping = method.getAnnotation(RequestMapping.class);
				if (requestMapping != null) {
					String[] path = requestMapping.value();
					if (path.length == 0) {
						path = requestMapping.path();
					}
					if (path.length > 0) {
						sb.append(path[0]);
						sb.append(' ');
						if (requestMapping.method().length > 0) {
							sb.append(requestMapping.method()[0].name());
						} else {
							sb.append("GET");
						}
						sb.append('\n');
					}
				}
				final Class<?> declaringClass = method.getDeclaringClass();
				final String classPart = declaringClass.getSimpleName();
				final String methodPart = method.getName();
				sb.append(classPart).append('.').append(methodPart);
				return sb.toString();
			}
		});
		return advisor;
	}

	/**
	 * Monitoring of {@link RestTemplate} beans.
	 * @return SpringRestTemplateBeanPostProcessor
	 */
	@Bean
	@ConditionalOnProperty(prefix = JavaMelodyConfigurationProperties.PREFIX, name = "spring-monitoring-enabled", matchIfMissing = true)
	public SpringRestTemplateBeanPostProcessor monitoringRestTemplateBeanPostProcessor() {
		return new SpringRestTemplateBeanPostProcessor();
	}

	/**
	 * Monitoring of MongoDbFactory beans.
	 * @return SpringMongoDbFactoryBeanPostProcessor
	 */
	@Bean
	@ConditionalOnProperty(prefix = JavaMelodyConfigurationProperties.PREFIX, name = "spring-monitoring-enabled", matchIfMissing = true)
	public SpringMongoDbFactoryBeanPostProcessor monitoringMongoDbFactoryBeanPostProcessor() {
		return new SpringMongoDbFactoryBeanPostProcessor();
	}

	/**
	 * Monitoring of ElasticsearchOperations beans.
	 * @return SpringElasticsearchOperationsBeanPostProcessor
	 */
	@Bean
	@ConditionalOnProperty(prefix = JavaMelodyConfigurationProperties.PREFIX, name = "spring-monitoring-enabled", matchIfMissing = true)
	public SpringElasticsearchOperationsBeanPostProcessor monitoringElasticsearchOperationsBeanPostProcessor() {
		return new SpringElasticsearchOperationsBeanPostProcessor();
	}

	/**
	 * @return Enregistrement du context Spring.
	 */
	@Bean
	public SpringContext javamelodySpringContext() {
		return new SpringContext();
	}
}
