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

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.core.PriorityOrdered;
import org.springframework.http.client.ClientHttpRequestInterceptor;
import org.springframework.web.client.RestTemplate;

import net.bull.javamelody.internal.common.LOG;

/**
 * Post-processor Spring pour un éventuel {@link RestTemplate} défini dans le fichier xml Spring.
 * @author Emeric Vernat
 */
public class SpringRestTemplateBeanPostProcessor implements BeanPostProcessor, PriorityOrdered {
	private static final boolean REST_TEMPLATE_INTERCEPTOR_AVAILABLE = isRestTemplateInterceptorAvailable();

	// l'interface PriorityOrdered place la priorité assez haute dans le contexte Spring
	// quelle que soit la valeur de order
	private int order = LOWEST_PRECEDENCE;

	/** {@inheritDoc} */
	@Override
	public int getOrder() {
		return order;
	}

	/**
	 * Définit la priorité dans le contexte Spring.
	 * @param order int
	 */
	public void setOrder(int order) {
		this.order = order;
	}

	/** {@inheritDoc} */
	@Override
	public Object postProcessBeforeInitialization(Object bean, String beanName) {
		return bean;
	}

	/** {@inheritDoc} */
	@Override
	public Object postProcessAfterInitialization(Object bean, String beanName) {
		// RestTemplate et getInterceptors() existent depuis spring-web 3.1.0.RELEASE
		if (REST_TEMPLATE_INTERCEPTOR_AVAILABLE && bean instanceof RestTemplate) {
			final RestTemplate restTemplate = (RestTemplate) bean;
			final List<ClientHttpRequestInterceptor> interceptors = new ArrayList<ClientHttpRequestInterceptor>(
					restTemplate.getInterceptors());
			for (final ClientHttpRequestInterceptor interceptor : interceptors) {
				if (interceptor instanceof SpringRestTemplateInterceptor) {
					return bean;
				}
			}
			interceptors.add(SpringRestTemplateInterceptor.SINGLETON);
			restTemplate.setInterceptors(interceptors);
			LOG.debug("rest template interceptor initialized");
		}

		return bean;
	}

	private static boolean isRestTemplateInterceptorAvailable() {
		try {
			final Class<?> clazz = Class.forName("org.springframework.web.client.RestTemplate");
			clazz.getMethod("getInterceptors");
			return true;
		} catch (final ClassNotFoundException e) {
			return false;
		} catch (final NoSuchMethodException e) {
			return false;
		} catch (final SecurityException e) {
			return false;
		}
	}
}
