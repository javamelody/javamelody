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

import org.springframework.beans.factory.config.BeanPostProcessor;

/**
 * Post-processor Spring pour une éventuelle DataSource défini dans le fichier xml Spring.
 * @author Emeric Vernat
 */
public class SpringDataSourceBeanPostProcessor implements BeanPostProcessor {
	/** {@inheritDoc} */
	public Object postProcessBeforeInitialization(Object bean, String beanName) {
		return bean;
	}

	/** {@inheritDoc} */
	public Object postProcessAfterInitialization(Object bean, String beanName) {
		if (bean instanceof DataSource) {
			final DataSource dataSource = (DataSource) bean;
			JdbcWrapperHelper.registerSpringDataSource(beanName, dataSource);
			return JdbcWrapper.SINGLETON.createDataSourceProxy(beanName, dataSource);
		}
		return bean;
	}
}
