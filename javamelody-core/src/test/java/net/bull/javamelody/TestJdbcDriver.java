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
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.PrintWriter;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Properties;

import javax.naming.InitialContext;
import javax.naming.NamingException;
import javax.sql.DataSource;

import net.bull.javamelody.JdbcDriver;
import net.bull.javamelody.JdbcWrapper;

import org.apache.tomcat.dbcp.dbcp.BasicDataSource;
import org.junit.Before;
import org.junit.Test;


/**
 * Test unitaire de la classe JdbcDriver.
 * @author Emeric Vernat
 */
public class TestJdbcDriver {
	private static final String H2_DATABASE_URL = "jdbc:h2:~/.h2/test";
	private JdbcDriver driver;

	/** Test. */
	@Before
	public void setUp() {
		driver = new JdbcDriver();
	}

	/** Test. */
	@Test
	public void testDeregister() {
		try {
			DriverManager.registerDriver(driver);
			driver.deregister();
		} catch (final Exception e) {
			fail(e.toString());
		}
	}

	/** Test. */
	@Test
	public void testGetJdbcWrapper() {
		assertNotNull("getJdbcWrapper", driver.getJdbcWrapper());
		// test rebind et stop (sans conteneur)
		driver.getJdbcWrapper().rebindDataSources();
		driver.getJdbcWrapper().stop();
	}

	/** Test.
	 * @throws NamingException e */
	@Test
	public void testCreateContextProxy() throws NamingException {
		assertNotNull("createContextProxy", driver.getJdbcWrapper().createContextProxy(
				new InitialContext()));
		driver.getJdbcWrapper().createContextProxy(new InitialContext()).close();
	}

	/** Test.
	 * @throws SQLException e */
	@Test
	public void testCreateDataSourceProxy() throws SQLException {
		final BasicDataSource dataSource = new BasicDataSource();
		dataSource.setUrl("jdbc:h2:~/.h2/test");
		final DataSource proxy = driver.getJdbcWrapper().createDataSourceProxy(dataSource);
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
		driver.getJdbcWrapper().createDataSourceProxy(dataSource2);
	}

	/** Test.
	 * @throws SQLException e */
	@Test
	public void testCreateConnectionProxy() throws SQLException {
		DriverManager.registerDriver(driver);
		// nécessite la dépendance vers la base de données H2
		Connection connection = DriverManager.getConnection(H2_DATABASE_URL);
		try {
			connection = driver.getJdbcWrapper().createConnectionProxy(connection);
			assertNotNull("createConnectionProxy", connection);
			assertFalse("equals", connection.equals(connection));
			connection.hashCode();

			connection.prepareStatement("select 1").close();
			connection.prepareCall("select 1").close();

			connection.rollback();

			assertTrue("proxy of proxy",
					driver.getJdbcWrapper().createConnectionProxy(connection) == connection);

			driver.getJdbcWrapper().getSqlCounter().setDisplayed(false);
			connection = driver.getJdbcWrapper().createConnectionProxy(connection);
			driver.getJdbcWrapper().getSqlCounter().setDisplayed(true);
			System.setProperty("monitoring.disabled", "true");
			try {
				connection = driver.getJdbcWrapper().createConnectionProxy(connection);
			} finally {
				System.setProperty("monitoring.disabled", "false");
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
			connection = driver.getJdbcWrapper().createConnectionProxy(connection);
			final Statement statement = connection.createStatement();
			try {
				assertFalse("equals", statement.equals(statement));
				statement.hashCode();

				statement.executeQuery("select 1").close();
				statement.execute("select 1");
				statement.addBatch("select 1");
				driver.getJdbcWrapper().getSqlCounter().setDisplayed(false);
				statement.execute("select 1");
				driver.getJdbcWrapper().getSqlCounter().setDisplayed(true);
			} finally {
				statement.close();
			}
		} finally {
			connection.close();
		}
	}

	/** Test. */
	@Test
	public void testGetLastConnectUrl() {
		assertNull("getLastConnectUrl", driver.getLastConnectUrl());
	}

	/** Test. */
	@Test
	public void testGetLastConnectInfo() {
		assertNull("getLastConnectInfo", driver.getLastConnectInfo());
	}

	/** Test.
	 * @throws SQLException e */
	@Test
	public void testConnect() throws SQLException {
		final Properties info = new Properties();
		assertNull("connect", driver.connect(null, info));
		info.put("driver", "sun.jdbc.odbc.JdbcOdbcDriver");
		try {
			driver.connect(null, info);
		} catch (final SQLException e) {
			// SQLException normale : The url cannot be null
			assertNotNull("connect", e);
		}
		info.put("driver", "nimporte.quoi");
		try {
			driver.connect(null, info);
		} catch (final SQLException e) {
			// SQLException normale : class not found
			assertNotNull("connect", e);
		}
	}

	/** Test.
	 * @throws SQLException e */
	@Test
	public void testAcceptsURL() throws SQLException {
		assertTrue("acceptsURL", driver.acceptsURL(null));
	}

	/** Test. */
	@Test
	public void testGetMajorVersion() {
		if (driver.getMajorVersion() >= 0) {
			fail("getMajorVersion");
		}
	}

	/** Test. */
	@Test
	public void testGetMinorVersion() {
		if (driver.getMinorVersion() >= 0) {
			fail("getMinorVersion");
		}
	}

	/** Test.
	 * @throws SQLException e */
	@Test
	public void testGetPropertyInfo() throws SQLException {
		assertNotNull("getPropertyInfo", driver.getPropertyInfo(null, null));
	}

	/** Test. */
	@Test
	public void testJdbcCompliant() {
		assertTrue("jdbcCompliant", driver.jdbcCompliant());
	}

	/** Test. */
	@Test
	public void testToString() {
		final String string = driver.toString();
		assertNotNull("toString not null", string);
		assertFalse("toString not empty", string.isEmpty());
	}

	/** Test. */
	@Test
	public void testGetSqlCounter() {
		assertNotNull("getSqlCounter", JdbcDriver.SINGLETON.getJdbcWrapper().getSqlCounter());
	}
}
