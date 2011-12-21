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

import static org.easymock.EasyMock.createNiceMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.PrintWriter;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.SQLFeatureNotSupportedException;
import java.sql.Statement;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.NamingException;
import javax.servlet.ServletContext;
import javax.sql.DataSource;

import org.apache.tomcat.dbcp.dbcp.BasicDataSource;
import org.junit.Before;
import org.junit.Test;

/**
 * Test unitaire de la classe JdbcWrapper.
 * @author Emeric Vernat
 */
public class TestJdbcWrapper {
	private static final class MyDataSource implements DataSource {
		private final BasicDataSource tomcatDataSource;

		MyDataSource(BasicDataSource tomcatDataSource) {
			this.tomcatDataSource = tomcatDataSource;
		}

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
			tomcatDataSource.setLoginTimeout(seconds);
		}

		/** {@inheritDoc} */
		public void setLogWriter(PrintWriter out) throws SQLException {
			tomcatDataSource.setLogWriter(out);
		}

		/** {@inheritDoc} */
		public int getLoginTimeout() throws SQLException {
			return tomcatDataSource.getLoginTimeout();
		}

		/** {@inheritDoc} */
		public PrintWriter getLogWriter() throws SQLException {
			return tomcatDataSource.getLogWriter();
		}

		/** {@inheritDoc} */
		public Connection getConnection(String username, String password) throws SQLException {
			return tomcatDataSource.getConnection();
		}

		/** {@inheritDoc} */
		public Connection getConnection() throws SQLException {
			return tomcatDataSource.getConnection();
		}

		/**
		 * Définition de la méthode getParentLogger ajoutée dans l'interface Driver en jdk 1.7.
		 * @return Logger
		 * @throws SQLFeatureNotSupportedException e
		 */
		public Logger getParentLogger() throws SQLFeatureNotSupportedException {
			return Logger.getLogger(Logger.GLOBAL_LOGGER_NAME);
		}
	}

	static final String H2_DATABASE_URL = "jdbc:h2:~/.h2/test";
	private static final String EQUALS = "equals";
	private JdbcDriver driver;
	private JdbcWrapper jdbcWrapper;

	/** Test.
	 * @throws SQLException e */
	@Before
	public void setUp() throws SQLException {
		Utils.initialize();
		Utils.setProperty(Parameter.SYSTEM_ACTIONS_ENABLED, "true");
		driver = new JdbcDriver();
		DriverManager.registerDriver(driver);
		jdbcWrapper = JdbcWrapper.SINGLETON;
	}

	/** Test. */
	@Test
	public void testRebindDataSources() {
		// test rebind et stop (sans conteneur)
		jdbcWrapper.rebindDataSources();
		Utils.setProperty(Parameter.REWRAP_DATASOURCES, "true");
		jdbcWrapper.rebindDataSources();
		jdbcWrapper.stop();
	}

	/** Test.
	 * @throws NamingException e */
	@Test
	public void testCreateContextProxy() throws NamingException {
		final Context context = jdbcWrapper.createContextProxy(new InitialContext());
		assertNotNull("createContextProxy", context);
		context.close();

		final Context mockContext = createNiceMock(Context.class);
		final Context proxyContext = JdbcWrapper.SINGLETON.createContextProxy(mockContext);
		final DataSource dataSource = createNiceMock(DataSource.class);
		final String jndiName = "java:comp/env/jdbc/DataSource";
		expect(mockContext.lookup(jndiName)).andReturn(dataSource).anyTimes();
		replay(mockContext);
		proxyContext.lookup(jndiName);
		verify(mockContext);
	}

	/** Test.
	 * @throws Exception e */
	@Test
	public void testCreateDataSourceProxy() throws Exception { // NOPMD
		// on fait le ménage au cas où TestMonitoringSpringInterceptor ait été exécuté juste avant
		cleanUp();

		assertTrue("getBasicDataSourceProperties", JdbcWrapper.getBasicDataSourceProperties()
				.isEmpty());
		assertEquals("getMaxConnectionCount", -1, JdbcWrapper.getMaxConnectionCount());

		final org.apache.commons.dbcp.BasicDataSource dbcpDataSource = new org.apache.commons.dbcp.BasicDataSource();
		dbcpDataSource.setUrl("jdbc:h2:~/.h2/test");
		dbcpDataSource.setMaxActive(123);
		final DataSource dbcpProxy = jdbcWrapper.createDataSourceProxy(dbcpDataSource);
		assertNotNull("createDataSourceProxy", dbcpProxy);
		assertFalse("getBasicDataSourceProperties", JdbcWrapper.getBasicDataSourceProperties()
				.isEmpty());
		assertEquals("getMaxConnectionCount", 123, JdbcWrapper.getMaxConnectionCount());

		final BasicDataSource tomcatDataSource = new BasicDataSource();
		tomcatDataSource.setUrl("jdbc:h2:~/.h2/test");
		tomcatDataSource.setMaxActive(456);
		final DataSource tomcatProxy = jdbcWrapper.createDataSourceProxy("test", tomcatDataSource);
		assertNotNull("createDataSourceProxy", tomcatProxy);
		assertNotNull("getLogWriter", tomcatProxy.getLogWriter());
		tomcatProxy.getConnection().close();
		assertFalse("getBasicDataSourceProperties", JdbcWrapper.getBasicDataSourceProperties()
				.isEmpty());
		assertEquals("getMaxConnectionCount", 456, JdbcWrapper.getMaxConnectionCount());

		final DataSource dataSource2 = new MyDataSource(tomcatDataSource);
		jdbcWrapper.createDataSourceProxy(dataSource2);
	}

	private static void cleanUp() throws NoSuchFieldException, IllegalAccessException {
		final Field tomcatField = JdbcWrapper.class
				.getDeclaredField("TOMCAT_BASIC_DATASOURCES_PROPERTIES");
		tomcatField.setAccessible(true);
		Object dsProperties = tomcatField.get(null);
		final Field propertiesField = dsProperties.getClass().getDeclaredField("properties");
		propertiesField.setAccessible(true);
		((Map<?, ?>) propertiesField.get(dsProperties)).clear();
		final Field dbcpField = JdbcWrapper.class
				.getDeclaredField("DBCP_BASIC_DATASOURCES_PROPERTIES");
		dbcpField.setAccessible(true);
		dsProperties = dbcpField.get(null);
		((Map<?, ?>) propertiesField.get(dsProperties)).clear();
	}

	/** Test.
	 * @throws SQLException e
	 * @throws IllegalAccessException e */
	@Test
	public void testCreateConnectionProxy() throws SQLException, IllegalAccessException {
		DriverManager.registerDriver(driver);
		// nécessite la dépendance vers la base de données H2
		Connection connection = DriverManager.getConnection(H2_DATABASE_URL);
		final int usedConnectionCount = JdbcWrapper.getUsedConnectionCount();
		try {
			jdbcWrapper.rewrapConnection(connection);
			connection = jdbcWrapper.createConnectionProxy(connection);
			assertEquals("getUsedConnectionCount1", usedConnectionCount + 1,
					JdbcWrapper.getUsedConnectionCount());
			assertNotNull("createConnectionProxy", connection);
			assertEquals(EQUALS, connection, connection);
			connection.hashCode();

			final int activeConnectionCount = JdbcWrapper.getActiveConnectionCount();

			connection.prepareStatement("select 1").close();
			connection.prepareCall("select 2").close();

			assertEquals("getActiveConnectionCount", activeConnectionCount,
					JdbcWrapper.getActiveConnectionCount());

			connection.rollback();

			jdbcWrapper.getSqlCounter().setDisplayed(false);
			connection = jdbcWrapper.createConnectionProxy(connection);
			assertEquals("getUsedConnectionCount2", usedConnectionCount + 2,
					JdbcWrapper.getUsedConnectionCount());
			jdbcWrapper.getSqlCounter().setDisplayed(true);
			Utils.setProperty(Parameter.DISABLED, "true");
			try {
				connection = jdbcWrapper.createConnectionProxy(connection);
				assertEquals("getUsedConnectionCount3", usedConnectionCount + 2,
						JdbcWrapper.getUsedConnectionCount());
			} finally {
				Utils.setProperty(Parameter.DISABLED, "false");
			}

			// il peut arriver que getConnectionInformationsList retourne une liste vide
			// si la classe JdbcWrapper a été initialisée alors que system-actions-enabled=false
			// ou que no-database=true ce est le cas vu l'ordre des tests dans le script ant
			assertNotNull("getConnectionInformationsList",
					JdbcWrapper.getConnectionInformationsList());
		} finally {
			connection.close();
			assertEquals("getUsedConnectionCount4", usedConnectionCount + 1,
					JdbcWrapper.getUsedConnectionCount());
		}

		assertEquals("proxy of proxy", connection, jdbcWrapper.createConnectionProxy(connection));

		final InvocationHandler dummy = new InvocationHandler() {
			public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
				return null;
			}
		};
		final List<Class<?>> interfaces = Arrays.asList(new Class<?>[] { Connection.class });
		connection = DriverManager.getConnection(H2_DATABASE_URL);
		try {
			assertNotNull("createProxy", JdbcWrapper.createProxy(connection, dummy, interfaces));
		} finally {
			connection.close();
		}

		JdbcWrapper.getActiveThreadCount();
	}

	/** Test.
	 * @throws SQLException e
	 * @throws IllegalAccessException e */
	@Test
	public void testRewrapConnection() throws SQLException, IllegalAccessException {
		DriverManager.registerDriver(driver);
		// nécessite la dépendance vers la base de données H2
		final Connection connection = DriverManager.getConnection(H2_DATABASE_URL);
		jdbcWrapper.rewrapConnection(connection);
		connection.close();
	}

	/** Test.
	 * @throws Exception e */
	@Test
	public void testRewrapDataSource() throws Exception { // NOPMD
		final DataSource dataSource = new BasicDataSource();
		// on utilise java.lang.reflect car la méthode est privée mais on veut vraiment la tester un minimum
		final Method rewrapDataSourceMethod = JdbcWrapper.class.getDeclaredMethod(
				"rewrapDataSource", String.class, DataSource.class);
		rewrapDataSourceMethod.setAccessible(true);
		rewrapDataSourceMethod.invoke(jdbcWrapper, "test", dataSource);
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
				assertFalse(EQUALS, statement.equals(statement));
				statement.hashCode();

				statement.executeQuery("select 1").close();
				statement.execute("select 2");
				statement.addBatch("select 3");
				jdbcWrapper.getSqlCounter().setDisplayed(false);
				statement.execute("select 4");
				jdbcWrapper.getSqlCounter().setDisplayed(true);
				try {
					statement.execute("invalid sql");
				} catch (final SQLException e) {
					assertNotNull("ok", e);
				}
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

	/** Test. */
	@Test
	public void testIsEqualsMethod() {
		assertTrue("isEqualsMethod1", JdbcWrapper.isEqualsMethod(EQUALS, new Object[] { "" }));
		assertFalse("isEqualsMethod2", JdbcWrapper.isEqualsMethod("notequals", new Object[] { "" }));
		assertFalse("isEqualsMethod3", JdbcWrapper.isEqualsMethod(EQUALS, null));
		assertFalse("isEqualsMethod4", JdbcWrapper.isEqualsMethod(EQUALS, new Object[] { "", "" }));
	}

	/** Test. */
	@Test
	public void testIsHashCodeMethod() {
		assertTrue("isHashCodeMethod1", JdbcWrapper.isHashCodeMethod("hashCode", new Object[] {}));
		assertTrue("isHashCodeMethod2", JdbcWrapper.isHashCodeMethod("hashCode", null));
		assertFalse("isHashCodeMethod3",
				JdbcWrapper.isHashCodeMethod("nothashCode", new Object[] {}));
		assertFalse("isHashCodeMethod4",
				JdbcWrapper.isHashCodeMethod("hashCode", new Object[] { "" }));
	}

	/** Test. */
	@Test
	public void testInitServletContext() {
		final String[] servers = { "JBoss", "GlassFish", "Sun Java System Application Server",
				"WebLogic", };
		for (final String serverName : servers) {
			final ServletContext servletContext = createNiceMock(ServletContext.class);
			expect(servletContext.getServerInfo()).andReturn(serverName).anyTimes();
			replay(servletContext);
			jdbcWrapper.initServletContext(servletContext);
			verify(servletContext);
		}
	}
}
