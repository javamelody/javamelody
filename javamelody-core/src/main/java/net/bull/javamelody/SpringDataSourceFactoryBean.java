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

import javax.sql.DataSource;

import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.config.AbstractFactoryBean;

/**
 * Spring {@link FactoryBean} for wrapping datasources with monitoring proxy.
 *
 * @see AbstractFactoryBean
 * @see JdbcWrapper
 *
 * @author David J. M. Karlsen (davidkarlsen at gmail.com), Emeric Vernat
 */
public class SpringDataSourceFactoryBean extends AbstractFactoryBean {
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
		final DataSource dataSource = (DataSource) getBeanFactory().getBean(targetName,
				DataSource.class);
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
