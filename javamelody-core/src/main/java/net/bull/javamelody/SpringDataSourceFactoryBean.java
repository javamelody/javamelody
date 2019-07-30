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

import javax.sql.DataSource;

import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.config.AbstractFactoryBean;

import net.bull.javamelody.internal.common.LOG;

/**
 * Spring {@link FactoryBean} for wrapping datasources with monitoring proxy.
 *
 * @see AbstractFactoryBean
 * @see JdbcWrapper
 *
 * @author David J. M. Karlsen (davidkarlsen at gmail.com), Emeric Vernat
 */
public class SpringDataSourceFactoryBean extends AbstractFactoryBean<DataSource> {
	private String targetName;

	// exemple :
	//	<bean id="wrappedDataSource" class="net.bull.javamelody.SpringDataSourceFactoryBean">
	//		<property name="targetName" value="targetDataSource" />
	//	</bean>
	//
	//	<bean id="targetDataSource" ...
	//	</bean>

	/**
	 * Name of target bean to instrument.
	 * This should implement {@linkplain DataSource}.
	 * @param targetName name of bean, not null.
	 */
	public void setTargetName(String targetName) {
		assert targetName != null;
		this.targetName = targetName;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	protected DataSource createInstance() {
		if (targetName == null) {
			throw new IllegalStateException("targetName must not be null");
		}
		final BeanFactory beanFactory = getBeanFactory();
		assert beanFactory != null;
		final DataSource dataSource = beanFactory.getBean(targetName, DataSource.class);
		JdbcWrapper.registerSpringDataSource(targetName, dataSource);
		final DataSource result = JdbcWrapper.SINGLETON.createDataSourceProxy(targetName,
				dataSource);
		LOG.debug("Spring target datasource wrapped: " + targetName);
		return result;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public Class<?> getObjectType() {
		return DataSource.class;
	}
}
