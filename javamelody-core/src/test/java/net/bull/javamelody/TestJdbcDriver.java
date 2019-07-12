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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.sql.SQLException;
import java.sql.SQLFeatureNotSupportedException;
import java.util.Properties;

import org.junit.Before;
import org.junit.Test;

import net.bull.javamelody.internal.common.Parameters;

/**
 * Test unitaire de la classe JdbcDriver.
 * @author Emeric Vernat
 */
public class TestJdbcDriver {
	private JdbcDriver driver;

	/** Test. */
	@Before
	public void setUp() {
		Utils.initialize();
		driver = new JdbcDriver();
	}

	/** Test. */
	@Test
	public void testGetLastConnectUrl() {
		Parameters.initJdbcDriverParameters(null, null);
		assertNull("getLastConnectUrl", Parameters.getLastConnectUrl());
	}

	/** Test. */
	@Test
	public void testGetLastConnectInfo() {
		Parameters.initJdbcDriverParameters(null, null);
		assertNull("getLastConnectInfo", Parameters.getLastConnectInfo());
	}

	/** Test.
	 * @throws SQLException e */
	@Test
	public void testConnect() throws SQLException {
		final Properties info = new Properties();
		try {
			driver.connect(null, info);
		} catch (final SQLException e) {
			// SQLException normale : The url cannot be null
			assertNotNull("connect", e);
		}
		driver.connect("jdbc:h2:mem:?driver=org.h2.Driver", info);
		info.put("driver", "org.h2.Driver");
		driver.connect("jdbc:h2:mem:", info);
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

	/** Test.
	 * @throws SQLFeatureNotSupportedException e */
	@Test
	public void testGetParentLogger() throws SQLFeatureNotSupportedException {
		assertNotNull("getParentLogger", driver.getParentLogger());
	}
}
