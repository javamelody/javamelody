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
import org.springframework.data.mongodb.MongoDbFactory;

import com.mongodb.client.MongoDatabase;

import net.bull.javamelody.internal.common.LOG;

/**
 * Post-processor Spring pour une éventuelle {@link MongoDbFactory} définie dans le fichier xml Spring.
 * @author Emeric Vernat
 */
public class SpringMongoDbFactoryBeanPostProcessor implements BeanPostProcessor, PriorityOrdered {
	private static final boolean MONGO_DB_FACTORY_AVAILABLE = isMongoDbFactoryAvailable();

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
		if (MONGO_DB_FACTORY_AVAILABLE && bean instanceof MongoDbFactory) {
			final MongoDbFactory mongoDbFactory = (MongoDbFactory) bean;
			final InvocationHandler invocationHandler = new InvocationHandler() {
				/** {@inheritDoc} */
				@Override
				public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
					Object result = method.invoke(mongoDbFactory, args);
					if (result instanceof MongoDatabase) {
						result = MongoWrapper.createDatabaseProxy((MongoDatabase) result);
					}
					return result;
				}
			};
			final MongoDbFactory factory = JdbcWrapper.createProxy(mongoDbFactory,
					invocationHandler);
			LOG.debug("mongodb monitoring initialized");
			return factory;
		}

		return bean;
	}

	private static boolean isMongoDbFactoryAvailable() {
		try {
			Class.forName("org.springframework.data.mongodb.MongoDbFactory");
			return true;
		} catch (final ClassNotFoundException e) {
			return false;
		}
	}
}
