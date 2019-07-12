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
import java.util.Set;

import javax.sql.DataSource;

import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.core.PriorityOrdered;
import org.springframework.jndi.JndiObjectFactoryBean;

import net.bull.javamelody.internal.common.LOG;
import net.bull.javamelody.internal.common.Parameters;

/**
 * Post-processor Spring pour une éventuelle {@link DataSource} défini dans le fichier xml Spring.
 * @author Emeric Vernat
 */
public class SpringDataSourceBeanPostProcessor implements BeanPostProcessor, PriorityOrdered {
	private Set<String> excludedDatasources;
	// l'interface PriorityOrdered place la priorité assez haute dans le contexte Spring
	// quelle que soit la valeur de order
	private int order = LOWEST_PRECEDENCE;

	private final Class<?> delegatingDataSourceClass = getDelegatingDataSourceClass();

	/**
	 * Définit les noms des datasources Spring exclues.
	 * @param excludedDatasources Set
	 */
	public void setExcludedDatasources(Set<String> excludedDatasources) {
		this.excludedDatasources = excludedDatasources;

		// exemple:
		//	<bean id="springDataSourceBeanPostProcessor" class="net.bull.javamelody.SpringDataSourceBeanPostProcessor">
		//		<property name="excludedDatasources">
		//			<set>
		//				<value>excludedDataSourceName</value>
		//			</set>
		//		</property>
		// 	</bean>
	}

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

	private boolean isExcludedDataSource(String beanName) {
		if (excludedDatasources != null && excludedDatasources.contains(beanName)) {
			LOG.debug("Spring datasource excluded: " + beanName);
			return true;
		}
		return false;
	}

	/** {@inheritDoc} */
	@Override
	public Object postProcessAfterInitialization(Object bean, String beanName) {
		if (bean instanceof DataSource) {
			// on ne teste isExcludedDataSource que si on est sur une datasource
			if (isExcludedDataSource(beanName) || Parameters.isNoDatabase()
					|| isDelegatingDataSourceAndAlreadyProxied(bean, beanName)) {
				return bean;
			}

			final DataSource dataSource = (DataSource) bean;
			JdbcWrapper.registerSpringDataSource(beanName, dataSource);
			final DataSource result = JdbcWrapper.SINGLETON.createDataSourceProxy(beanName,
					dataSource);
			LOG.debug("Spring datasource wrapped: " + beanName);
			return result;
		} else if (bean instanceof JndiObjectFactoryBean) {
			// ou sur un JndiObjectFactoryBean
			if (isExcludedDataSource(beanName) || Parameters.isNoDatabase()) {
				return bean;
			}

			// fix issue 20
			final Object result = createProxy(bean, beanName);
			LOG.debug("Spring JNDI factory wrapped: " + beanName);
			return result;
		}

		// I tried here in the post-processor to fix "quartz jobs which are scheduled with spring
		// are not displayed in javamelody, except if there is the following property for
		// SchedulerFactoryBean in spring xml:
		// <property name="exposeSchedulerInRepository" value="true" /> ",

		// but I had some problem with Spring creating the scheduler
		// twice and so registering the scheduler in SchedulerRepository with the same name
		// as the one registered below (and Quartz wants not)
		//		else if (bean != null
		//				&& "org.springframework.scheduling.quartz.SchedulerFactoryBean".equals(bean
		//						.getClass().getName())) {
		//			try {
		//				// Remarque: on ajoute nous même le scheduler de Spring dans le SchedulerRepository
		//				// de Quartz, car l'appel ici de schedulerFactoryBean.setExposeSchedulerInRepository(true)
		//				// est trop tard et ne fonctionnerait pas
		//				final Method method = bean.getClass().getMethod("getScheduler", (Class<?>[]) null);
		//				final Scheduler scheduler = (Scheduler) method.invoke(bean, (Object[]) null);
		//
		//				final SchedulerRepository schedulerRepository = SchedulerRepository.getInstance();
		//				synchronized (schedulerRepository) {
		//					if (schedulerRepository.lookup(scheduler.getSchedulerName()) == null) {
		//						schedulerRepository.bind(scheduler);
		//						scheduler.addGlobalJobListener(new JobGlobalListener());
		//					}
		//				}
		//			} catch (final NoSuchMethodException e) {
		//				// si la méthode n'existe pas (avant spring 2.5.6), alors cela marche sans rien faire
		//				return bean;
		//			} catch (final InvocationTargetException e) {
		//				// tant pis
		//				return bean;
		//			} catch (final IllegalAccessException e) {
		//				// tant pis
		//				return bean;
		//			} catch (SchedulerException e) {
		//				// tant pis
		//				return bean;
		//			}
		//		}

		return bean;
	}

	private Object createProxy(final Object bean, final String beanName) {
		final InvocationHandler invocationHandler = new InvocationHandler() {
			/** {@inheritDoc} */
			@Override
			public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
				Object result = method.invoke(bean, args);
				if (result instanceof DataSource) {
					result = JdbcWrapper.SINGLETON.createDataSourceProxy(beanName,
							(DataSource) result);
				}
				return result;
			}
		};
		return JdbcWrapper.createProxy(bean, invocationHandler);
	}

	private boolean isDelegatingDataSourceAndAlreadyProxied(Object bean, String beanName) {
		// bean instanceof DelegatingDataSource ?
		// use reflection in case spring-jdbc is not available
		if (delegatingDataSourceClass != null && delegatingDataSourceClass.isInstance(bean)) {
			final DataSource targetDataSource;
			try {
				targetDataSource = (DataSource) delegatingDataSourceClass
						.getMethod("getTargetDataSource").invoke(bean);
			} catch (final Exception e) {
				// call to ((DelegatingDataSource) bean).getTargetDataSource() is not supposed to fail
				throw new IllegalStateException(e);
			}
			if (JdbcWrapper.isProxyAlready(targetDataSource)) {
				LOG.debug("Spring delegating datasource excluded: " + beanName);
				return true;
			}
		}
		return false;
	}

	private static Class<?> getDelegatingDataSourceClass() {
		try {
			return Class.forName("org.springframework.jdbc.datasource.DelegatingDataSource");
		} catch (final ClassNotFoundException e) {
			return null;
		}
	}
}
