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

import java.sql.SQLException;
import java.sql.Statement;
import java.util.Date;

import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;

/**
 * Implémentation test de bean Spring.
 * (implémente une interface pour permettre l'AOP Spring car sinon il faut CGLIB)
 * @author Emeric Vernat
 */
// on pourrait utiliser l'annotation @MonitoredWithSpring,
// mais MonitoredWithInterfacePointcut suffit dans spring-context.xml
@MonitoredWithGuice
public class SpringTestFacadeImpl implements SpringTestFacade {
	/**
	 * {@inheritDoc}
	 */
	@Override
	public Date nowWithSql() throws SQLException {
		//		final javax.sql.DataSource dataSource = (javax.sql.DataSource) new javax.naming.InitialContext()
		//				.lookup("java:comp/env/jdbc/TestDB");
		final ConfigurableApplicationContext context = new ClassPathXmlApplicationContext(
				"net/bull/javamelody/monitoring-spring.xml", "spring-context.xml");
		try {
			final javax.sql.DataSource dataSource = (javax.sql.DataSource) context
					.getBean("dataSource");
			final java.sql.Connection connection = dataSource.getConnection();
			connection.setAutoCommit(false);
			try {
				// test pour explain plan en oracle
				//			final PreparedStatement statement = connection
				//					.prepareStatement("select * from v$session where user# = ?");
				final Statement statement = connection.createStatement();
				try {
					//				statement.setInt(1, 36);
					//				statement.executeQuery();

					statement.execute(
							"DROP ALIAS if exists SLEEP; CREATE ALIAS SLEEP FOR \"java.lang.Thread.sleep(long)\"");
					statement.execute("call sleep(.01)");
					for (int i = 0; i < 5; i++) {
						statement.execute("call sleep(.02)");
					}
				} finally {
					statement.close();
				}
			} finally {
				connection.rollback();
				connection.close();
			}
		} finally {
			context.close();
		}

		return new Date();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public Date now() {
		return new Date();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void throwError() {
		throw new OutOfMemoryError("test");
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void throwException() {
		throw new IllegalStateException("test");
	}
}
