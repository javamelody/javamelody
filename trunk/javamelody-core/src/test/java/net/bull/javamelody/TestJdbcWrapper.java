/*
 * Copyright 2008-2009 by Emeric Vernat, Bull
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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.PrintWriter;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;

import javax.naming.InitialContext;
import javax.naming.NamingException;
import javax.sql.DataSource;

import org.apache.tomcat.dbcp.dbcp.BasicDataSource;
import org.junit.Before;
import org.junit.Test;

/**
 * Test unitaire de la classe JdbcWrapper.
 * @author Emeric Vernat
 */
public class TestJdbcWrapper {
	private static final String H2_DATABASE_URL = "jdbc:h2:~/.h2/test";
	private JdbcDriver driver;
	private JdbcWrapper jdbcWrapper;

	/** Test.
	 * @throws SQLException e */
	@Before
	public void setUp() throws SQLException {
		driver = new JdbcDriver();
		DriverManager.registerDriver(driver);
		jdbcWrapper = driver.getJdbcWrapper();
	}

	/** Test. */
	@Test
	public void testRebindDataSources() {
		// test rebind et stop (sans conteneur)
		jdbcWrapper.rebindDataSources();
		jdbcWrapper.stop();
	}

	/** Test.
	 * @throws NamingException e */
	@Test
	public void testCreateContextProxy() throws NamingException {
		assertNotNull("createContextProxy", jdbcWrapper.createContextProxy(new InitialContext()));
		jdbcWrapper.createContextProxy(new InitialContext()).close();
	}

	/** Test.
	 * @throws SQLException e */
	@Test
	public void testCreateDataSourceProxy() throws SQLException {
		final BasicDataSource dataSource = new BasicDataSource();
		dataSource.setUrl("jdbc:h2:~/.h2/test");
		final DataSource proxy = jdbcWrapper.createDataSourceProxy(dataSource);
		assertNotNull("createDataSourceProxy", proxy);
		assertNotNull("getLogWriter", proxy.getLogWriter());
		proxy.getConnection().close();
		JdbcWrapper.getTomcatBasicDataSourceProperties();

		final DataSource dataSource2 = new DataSource() {
			/** {@inheritDoc} */
			public <T> T unwrap(Class<T> iface) throws SQLException {
				return null;
			}

			/** {@inheritDoc} */
			public boolean isWrapperFor(Class<?> iface) throws SQLException {
				return false;
			}

			/** {@inheritDoc} */
			public void setLoginTimeout(int seconds) throws SQLException {
				dataSource.setLoginTimeout(seconds);
			}

			/** {@inheritDoc} */
			public void setLogWriter(PrintWriter out) throws SQLException {
				dataSource.setLogWriter(out);
			}

			/** {@inheritDoc} */
			public int getLoginTimeout() throws SQLException {
				return dataSource.getLoginTimeout();
			}

			/** {@inheritDoc} */
			public PrintWriter getLogWriter() throws SQLException {
				return dataSource.getLogWriter();
			}

			/** {@inheritDoc} */
			public Connection getConnection(String username, String password) throws SQLException {
				return dataSource.getConnection();
			}

			/** {@inheritDoc} */
			public Connection getConnection() throws SQLException {
				return dataSource.getConnection();
			}
		};
		jdbcWrapper.createDataSourceProxy(dataSource2);
	}

	/** Test.
	 * @throws SQLException e */
	@Test
	public void testCreateConnectionProxy() throws SQLException {
		DriverManager.registerDriver(driver);
		// nécessite la dépendance vers la base de données H2
		Connection connection = DriverManager.getConnection(H2_DATABASE_URL);
		try {
			connection = jdbcWrapper.createConnectionProxy(connection);
			assertNotNull("createConnectionProxy", connection);
			assertFalse("equals", connection.equals(connection));
			connection.hashCode();

			connection.prepareStatement("select 1").close();
			connection.prepareCall("select 1").close();

			connection.rollback();

			assertTrue("proxy of proxy",
					jdbcWrapper.createConnectionProxy(connection) == connection);

			jdbcWrapper.getSqlCounter().setDisplayed(false);
			connection = jdbcWrapper.createConnectionProxy(connection);
			jdbcWrapper.getSqlCounter().setDisplayed(true);
			System.setProperty(Parameters.PARAMETER_SYSTEM_PREFIX + Parameter.DISABLED.getCode(),
					"true");
			try {
				connection = jdbcWrapper.createConnectionProxy(connection);
			} finally {
				System.setProperty(Parameters.PARAMETER_SYSTEM_PREFIX
						+ Parameter.DISABLED.getCode(), "false");
			}
		} finally {
			connection.close();
		}
	}

	/** Test.
	 * @throws SQLException e */
	@Test
	public void testStatementProxy() throws SQLException {
		DriverManager.registerDriver(driver);
		// nécessite la dépendance vers la base de données H2
		Connection connection = DriverManager.getConnection(H2_DATABASE_URL);
		try {
			connection = jdbcWrapper.createConnectionProxy(connection);
			final Statement statement = connection.createStatement();
			try {
				assertFalse("equals", statement.equals(statement));
				statement.hashCode();

				statement.executeQuery("select 1").close();
				statement.execute("select 1");
				statement.addBatch("select 1");
				jdbcWrapper.getSqlCounter().setDisplayed(false);
				statement.execute("select 1");
				jdbcWrapper.getSqlCounter().setDisplayed(true);
			} finally {
				statement.close();
			}
		} finally {
			connection.close();
		}
	}

	/** Test. */
	@Test
	public void testGetSqlCounter() {
		assertNotNull("getSqlCounter", jdbcWrapper.getSqlCounter());
	}
}
