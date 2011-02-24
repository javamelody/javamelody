/*
 * Copyright 2008-2010 by Emeric Vernat
 *
 *     This file is part of Java Melody.
 *
 * Java Melody is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Java Melody is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Java Melody.  If not, see <http://www.gnu.org/licenses/>.
 */
package net.bull.javamelody;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.util.Set;

import javax.sql.DataSource;

import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.jndi.JndiObjectFactoryBean;

/**
 * Post-processor Spring pour une éventuelle DataSource défini dans le fichier xml Spring.
 * @author Emeric Vernat
 */
public class SpringDataSourceBeanPostProcessor implements BeanPostProcessor,
		ApplicationContextAware {
	private Set<String> excludedDatasources;
	private ApplicationContext applicationContext;

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
	public void setApplicationContext(ApplicationContext applicationContext) {
		this.applicationContext = applicationContext;
	}

	/** {@inheritDoc} */
	public Object postProcessBeforeInitialization(Object bean, String beanName) {
		return bean;
	}

	/** {@inheritDoc} */
	public Object postProcessAfterInitialization(final Object bean, final String beanName) {
		if (excludedDatasources != null && excludedDatasources.contains(beanName)) {
			LOG.debug("Spring datasource excluded: " + beanName);
			return bean;
		}
		if (isBeanDefinitionAbstract(beanName)) {
			LOG.debug("Ignoring abstract bean: " + beanName);
			return bean;
		}
		if (bean instanceof DataSource && !Parameters.isNoDatabase()) {
			final DataSource dataSource = (DataSource) bean;
			JdbcWrapper.registerSpringDataSource(beanName, dataSource);
			final DataSource result = JdbcWrapper.SINGLETON.createDataSourceProxy(beanName,
					dataSource);
			LOG.debug("Spring datasource wrapped: " + beanName);
			return result;
		} else if (bean instanceof JndiObjectFactoryBean && !Parameters.isNoDatabase()) {
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

	private boolean isBeanDefinitionAbstract(String beanName) {
		if (applicationContext instanceof ConfigurableApplicationContext) {
			final ConfigurableListableBeanFactory beanFactory = ((ConfigurableApplicationContext) applicationContext)
					.getBeanFactory();
			// beanFactory ne contient pas forcément beanName, cf issue 80 comment 7
			return beanFactory.containsBeanDefinition(beanName)
					&& beanFactory.getBeanDefinition(beanName).isAbstract();
		}
		return false;
	}

	private Object createProxy(final Object bean, final String beanName) {
		final InvocationHandler invocationHandler = new InvocationHandler() {
			/** {@inheritDoc} */
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
}
