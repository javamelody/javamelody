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
package net.bull.javamelody; // NOPMD

import static org.easymock.EasyMock.createNiceMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.NamingException;
import javax.sql.DataSource;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jakarta.servlet.ServletContext;
import net.bull.javamelody.internal.model.ConnectionInformations;

/**
 * Test unitaire de la classe JdbcWrapper.
 * @author Emeric Vernat
 */
public class TestJdbcWrapper {
	public static final String H2_DATABASE_URL = "jdbc:h2:~/.h2/test;AUTO_SERVER=TRUE";
	private static final String EQUALS = "equals";
	private JdbcDriver driver;
	private JdbcWrapper jdbcWrapper;

	/** Test.
	 * @throws SQLException e */
	@BeforeEach
	void setUp() throws SQLException {
		Utils.initialize();
		Utils.setProperty(Parameter.SYSTEM_ACTIONS_ENABLED, Boolean.TRUE.toString());
		driver = new JdbcDriver();
		DriverManager.registerDriver(driver);
		jdbcWrapper = JdbcWrapper.SINGLETON;
	}

	/** Test. */
	@Test
	void testRebindDataSources() {
		// test rebind et stop (sans conteneur)
		jdbcWrapper.rebindDataSources();
		Utils.setProperty(Parameter.REWRAP_DATASOURCES, "true");
		jdbcWrapper.rebindDataSources();
		jdbcWrapper.stop();
	}

	/** Test.
	 * @throws NamingException e */
	@Test
	void testCreateContextProxy() throws NamingException {
		final Context context = jdbcWrapper.createContextProxy(new InitialContext());
		assertNotNull(context, "createContextProxy");
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
	void testCreateDataSourceProxy() throws Exception {
		// on fait le ménage au cas où TestMonitoringSpringInterceptor ait été exécuté juste avant
		cleanUp();

		assertTrue(JdbcWrapper.getBasicDataSourceProperties().isEmpty(), "getBasicDataSourceProperties0");
		assertEquals(-1, JdbcWrapper.getMaxConnectionCount(), "getMaxConnectionCount0");

		final org.apache.tomcat.jdbc.pool.DataSource tomcatJdbcDataSource = new org.apache.tomcat.jdbc.pool.DataSource();
		tomcatJdbcDataSource.setUrl(H2_DATABASE_URL);
		tomcatJdbcDataSource.setDriverClassName("org.h2.Driver");
		tomcatJdbcDataSource.setMaxActive(123);
		final DataSource tomcatJdbcProxy = jdbcWrapper.createDataSourceProxy("test2",
				tomcatJdbcDataSource);
		assertNotNull(tomcatJdbcProxy, "createDataSourceProxy1");
		tomcatJdbcProxy.getConnection().close();
		assertFalse(JdbcWrapper.getBasicDataSourceProperties().isEmpty(), "getBasicDataSourceProperties1");
		assertEquals(123, JdbcWrapper.getMaxConnectionCount(), "getMaxConnectionCount1");

		final org.apache.commons.dbcp2.BasicDataSource dbcp2DataSource = new org.apache.commons.dbcp2.BasicDataSource();
		dbcp2DataSource.setUrl(H2_DATABASE_URL);
		dbcp2DataSource.setMaxTotal(456);
		final DataSource dbcp2Proxy = jdbcWrapper.createDataSourceProxy(dbcp2DataSource);
		assertNotNull(dbcp2Proxy, "createDataSourceProxy2b");

		final org.apache.tomcat.dbcp.dbcp2.BasicDataSource tomcat2DataSource = new org.apache.tomcat.dbcp.dbcp2.BasicDataSource();
		tomcat2DataSource.setUrl(H2_DATABASE_URL);
		tomcat2DataSource.setMaxTotal(789);
		final DataSource tomcat2Proxy = jdbcWrapper.createDataSourceProxy("test",
				tomcat2DataSource);
		assertNotNull(tomcat2Proxy, "createDataSourceProxy3b");
	}

	private static void cleanUp() throws NoSuchFieldException, IllegalAccessException {
		final Field tomcatField = JdbcWrapperHelper.class
				.getDeclaredField("TOMCAT_BASIC_DATASOURCES_PROPERTIES");
		tomcatField.setAccessible(true);
		Object dsProperties = tomcatField.get(null);
		final Field propertiesField = dsProperties.getClass().getDeclaredField("properties");
		propertiesField.setAccessible(true);
		((Map<?, ?>) propertiesField.get(dsProperties)).clear();
		final Field dbcpField = JdbcWrapperHelper.class
				.getDeclaredField("DBCP_BASIC_DATASOURCES_PROPERTIES");
		dbcpField.setAccessible(true);
		dsProperties = dbcpField.get(null);
		((Map<?, ?>) propertiesField.get(dsProperties)).clear();
	}

	/** Test.
	 * @throws SQLException e
	 * @throws IllegalAccessException e */
	@Test
	void testCreateConnectionProxy() throws SQLException, IllegalAccessException {
		DriverManager.registerDriver(driver);
		final int usedConnectionCount = JdbcWrapper.getUsedConnectionCount();
		// nécessite la dépendance vers la base de données H2
		Connection connection = DriverManager.getConnection(H2_DATABASE_URL);
		assertEquals(usedConnectionCount, JdbcWrapper.getUsedConnectionCount(), "getUsedConnectionCount1");
		try {
			jdbcWrapper.rewrapConnection(connection);
			connection = jdbcWrapper.createConnectionProxy(connection);
			assertEquals(usedConnectionCount + 1, JdbcWrapper.getUsedConnectionCount(), "getUsedConnectionCount1");
			assertNotNull(connection, "createConnectionProxy");
			assertEquals(connection, connection, EQUALS);
			connection.hashCode();

			final int activeConnectionCount = JdbcWrapper.getActiveConnectionCount();

			connection.prepareStatement("select 1").close();
			connection.prepareCall("select 2").close();

			assertEquals(activeConnectionCount, JdbcWrapper.getActiveConnectionCount(), "getActiveConnectionCount");

			connection.rollback();

			jdbcWrapper.getSqlCounter().setDisplayed(false);
			connection = jdbcWrapper.createConnectionProxy(connection);
			assertEquals(usedConnectionCount + 1, JdbcWrapper.getUsedConnectionCount(), "getUsedConnectionCount2");
			jdbcWrapper.getSqlCounter().setDisplayed(true);
			Utils.setProperty(Parameter.DISABLED, "true");
			try {
				connection = jdbcWrapper.createConnectionProxy(connection);
				assertEquals(usedConnectionCount + 1, JdbcWrapper.getUsedConnectionCount(), "getUsedConnectionCount3");
			} finally {
				Utils.setProperty(Parameter.DISABLED, "false");
			}

			// il peut arriver que getConnectionInformationsList retourne une liste vide
			// si la classe JdbcWrapper a été initialisée alors que system-actions-enabled=false
			// ou que no-database=true ce est le cas vu l'ordre des tests dans le script ant
			assertNotNull(JdbcWrapper.getConnectionInformationsList(), "getConnectionInformationsList");
		} finally {
			connection.close();
		}
		assertEquals(usedConnectionCount, JdbcWrapper.getUsedConnectionCount(), "getUsedConnectionCount4");
		connection = DriverManager.getConnection(H2_DATABASE_URL + "?driver=org.h2.Driver");
		try {
			assertEquals(usedConnectionCount + 1, JdbcWrapper.getUsedConnectionCount(), "getUsedConnectionCount1");
		} finally {
			connection.close();
		}

		assertEquals(connection, jdbcWrapper.createConnectionProxy(connection), "proxy of proxy");

		final InvocationHandler dummy = (proxy, method, args) -> null;
		final List<Class<?>> interfaces = List.of(new Class<?>[] { Connection.class });
		connection = DriverManager.getConnection(H2_DATABASE_URL);
		try {
			assertNotNull(JdbcWrapper.createProxy(connection, dummy, interfaces), "createProxy");
		} finally {
			connection.close();
		}

		JdbcWrapper.getActiveThreadCount();
	}

	/** Test.
	 * @throws SQLException e
	 * @throws IllegalAccessException e */
	@Test
	void testCreateConnectionProxyOrRewrapIfJBossOrGlassfish()
			throws SQLException, IllegalAccessException {
		DriverManager.registerDriver(driver);
		// nécessite la dépendance vers la base de données H2
		final Connection connection = DriverManager.getConnection(H2_DATABASE_URL);
		jdbcWrapper.createConnectionProxyOrRewrapIfJBossOrGlassfish(connection);
		connection.close();
	}

	/** Test.
	 * @throws SQLException e
	 * @throws IllegalAccessException e */
	@Test
	void testRewrapConnection() throws SQLException, IllegalAccessException {
		DriverManager.registerDriver(driver);
		// nécessite la dépendance vers la base de données H2
		final Connection connection = DriverManager.getConnection(H2_DATABASE_URL);
		jdbcWrapper.rewrapConnection(connection);
		connection.close();
	}

	/** Test.
	 * @throws Exception e */
	@Test
	void testRewrapDataSource() throws Exception {
		final org.apache.tomcat.dbcp.dbcp2.BasicDataSource tomcat2DataSource = new org.apache.tomcat.dbcp.dbcp2.BasicDataSource();
		tomcat2DataSource.setUrl(H2_DATABASE_URL);
		rewrapDataSource(tomcat2DataSource);
		final org.apache.commons.dbcp2.BasicDataSource dbcp2DataSource = new org.apache.commons.dbcp2.BasicDataSource();
		dbcp2DataSource.setUrl(H2_DATABASE_URL);
		rewrapDataSource(dbcp2DataSource);
		final DataSource dataSource = createNiceMock(DataSource.class);
		rewrapDataSource(dataSource);
	}

	private void rewrapDataSource(DataSource dataSource)
			throws NoSuchMethodException, SecurityException, IllegalAccessException,
			IllegalArgumentException, InvocationTargetException {
		// on utilise java.lang.reflect car la méthode est privée mais on veut vraiment la tester un minimum
		final Method rewrapDataSourceMethod = JdbcWrapper.class
				.getDeclaredMethod("rewrapDataSource", String.class, DataSource.class);
		rewrapDataSourceMethod.setAccessible(true);
		rewrapDataSourceMethod.invoke(jdbcWrapper, "test", dataSource);
	}

	/** Test.
	 * @throws SQLException e */
	@Test
	void testStatementProxy() throws SQLException {
		DriverManager.registerDriver(driver);
		// nécessite la dépendance vers la base de données H2
		Connection connection = DriverManager.getConnection(H2_DATABASE_URL);
		try {
			connection = jdbcWrapper.createConnectionProxy(connection);
			try (Statement statement = connection.createStatement()) {
				assertFalse(statement.equals(statement), EQUALS);
				statement.hashCode();

				statement.executeQuery("select 1").close();
				statement.execute("select 2");
				statement.execute("CREATE TABLE IF NOT EXISTS test (name VARCHAR(50) NOT NULL)");
				statement.addBatch("insert into test (name) values ('test')");
				statement.executeBatch();
				statement.addBatch("/* BATCH */ insert into test (name) values ('test')");
				statement.executeBatch();
				statement.addBatch("insert into test (name) values ('test')");
				statement.executeLargeBatch();
				jdbcWrapper.getSqlCounter().setDisplayed(false);
				statement.execute("select 4");
				jdbcWrapper.getSqlCounter().setDisplayed(true);
				statement.execute("explain select 3");
				try {
					statement.execute("invalid sql");
				} catch (final SQLException e) {
					assertNotNull(e, "ok");
				}
			}
		} finally {
			connection.close();
		}
	}

	/** Test. */
	@Test
	void testGetSqlCounter() {
		assertNotNull(jdbcWrapper.getSqlCounter(), "getSqlCounter");
	}

	/** Test. */
	@Test
	void testIsEqualsMethod() {
		assertTrue(JdbcWrapper.isEqualsMethod(EQUALS, new Object[] { "" }), "isEqualsMethod1");
		assertFalse(JdbcWrapper.isEqualsMethod("notequals", new Object[] { "" }), "isEqualsMethod2");
		assertFalse(JdbcWrapper.isEqualsMethod(EQUALS, null), "isEqualsMethod3");
		assertFalse(JdbcWrapper.isEqualsMethod(EQUALS, new Object[] { "", "" }), "isEqualsMethod4");
	}

	/** Test. */
	@Test
	void testIsHashCodeMethod() {
		assertTrue(JdbcWrapper.isHashCodeMethod("hashCode", new Object[] {}), "isHashCodeMethod1");
		assertTrue(JdbcWrapper.isHashCodeMethod("hashCode", null), "isHashCodeMethod2");
		assertFalse(JdbcWrapper.isHashCodeMethod("nothashCode", new Object[] {}), "isHashCodeMethod3");
		assertFalse(JdbcWrapper.isHashCodeMethod("hashCode", new Object[] { "" }), "isHashCodeMethod4");
	}

	/** Test. */
	@Test
	void testConnectionInformationsComparator() {
		final ConnectionInformations connectionInformations = new ConnectionInformations();
		final ConnectionInformations connectionInformations2 = new ConnectionInformations();
		final List<ConnectionInformations> list = Arrays.asList(connectionInformations,
				connectionInformations2);
		list.sort(JdbcWrapper.CONNECTION_INFORMATIONS_COMPARATOR);
	}

	/** Test. */
	@Test
	void testInitServletContext() {
		final String[] servers = { "JBoss", "WildFly", "GlassFish",
				"Sun Java System Application Server", "WebLogic", };
		for (final String serverName : servers) {
			final ServletContext servletContext = createNiceMock(ServletContext.class);
			expect(servletContext.getServerInfo()).andReturn(serverName).anyTimes();
			replay(servletContext);
			jdbcWrapper.initServletContext(servletContext);
			verify(servletContext);
		}
	}

	/** Test. */
	@Test
	void testIsSqlMonitoringDisabled() {
		Utils.setProperty(Parameter.DISABLED, "false");
		jdbcWrapper.getSqlCounter().setDisplayed(true);
		assertFalse(jdbcWrapper.isSqlMonitoringDisabled(), "isSqlMonitoringDisabled1");
		Utils.setProperty(Parameter.DISABLED, "true");
		assertTrue(jdbcWrapper.isSqlMonitoringDisabled(), "isSqlMonitoringDisabled2");
		Utils.setProperty(Parameter.DISABLED, "false");
		jdbcWrapper.getSqlCounter().setDisplayed(false);
		assertTrue(jdbcWrapper.isSqlMonitoringDisabled(), "isSqlMonitoringDisabled3");
		jdbcWrapper.getSqlCounter().setDisplayed(true);
		assertFalse(jdbcWrapper.isSqlMonitoringDisabled(), "isSqlMonitoringDisabled4");
	}
}
