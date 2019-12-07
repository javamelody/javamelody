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

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;

import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.core.PriorityOrdered;
import org.springframework.data.elasticsearch.core.ElasticsearchOperations;

import net.bull.javamelody.internal.common.LOG;
import net.bull.javamelody.internal.common.Parameters;
import net.bull.javamelody.internal.model.Counter;

/**
 * Post-processor Spring pour une éventuelle interface {@link ElasticsearchOperations} définie dans le fichier xml Spring.
 * @author Emeric Vernat
 */
public class SpringElasticsearchOperationsBeanPostProcessor
		implements BeanPostProcessor, PriorityOrdered {
	private static final boolean ELASTICSEARCH_OPERATIONS_AVAILABLE = isElasticsearchOperationsAvailable();
	private static final Counter SERVICES_COUNTER = MonitoringProxy.getServicesCounter();
	private static final boolean COUNTER_HIDDEN = Parameters
			.isCounterHidden(SERVICES_COUNTER.getName());
	private static final boolean DISABLED = Parameter.DISABLED.getValueAsBoolean();

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
		if (ELASTICSEARCH_OPERATIONS_AVAILABLE && bean instanceof ElasticsearchOperations) {
			final ElasticsearchOperations elasticsearchOperations = (ElasticsearchOperations) bean;
			if (DISABLED) {
				return elasticsearchOperations;
			}
			SERVICES_COUNTER.setDisplayed(!COUNTER_HIDDEN);
			SERVICES_COUNTER.setUsed(true);

			final InvocationHandler invocationHandler = new InvocationHandler() {
				/** {@inheritDoc} */
				@Override
				public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
					final StringBuilder requestName = new StringBuilder();
					requestName.append("elasticsearch.").append(method.getName()).append('(');
					if (args != null) {
						boolean first = true;
						for (final Object arg : args) {
							if (first) {
								first = false;
							} else {
								requestName.append(", ");
							}
							if (arg == null) {
								requestName.append("null");
							} else if (arg instanceof Class) {
								requestName.append(((Class<?>) arg).getSimpleName());
							} else {
								requestName.append(arg.getClass().getSimpleName());
							}
						}
					}
					requestName.append(')');
					return doInvoke(elasticsearchOperations, method, args, requestName.toString());
				}
			};
			final ElasticsearchOperations ops = JdbcWrapper.createProxy(elasticsearchOperations,
					invocationHandler);
			LOG.debug("elasticsearch operations monitoring initialized");
			return ops;
		}

		return bean;
	}

	static Object doInvoke(final Object object, final Method method, final Object[] args,
			final String requestName) throws Throwable {
		boolean systemError = false;
		try {
			SERVICES_COUNTER.bindContextIncludingCpu(requestName);
			return method.invoke(object, args);
		} catch (final Error e) {
			// on catche Error pour avoir les erreurs systèmes
			// mais pas Exception qui sont fonctionnelles en général
			systemError = true;
			throw e;
		} finally {
			// on enregistre la requête dans les statistiques
			SERVICES_COUNTER.addRequestForCurrentContext(systemError);
		}
	}

	private static boolean isElasticsearchOperationsAvailable() {
		try {
			Class.forName("org.springframework.data.elasticsearch.core.ElasticsearchOperations");
			return true;
		} catch (final ClassNotFoundException e) {
			return false;
		}
	}
}
