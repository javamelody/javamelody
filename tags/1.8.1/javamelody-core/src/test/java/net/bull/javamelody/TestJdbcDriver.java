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

import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.Properties;

import org.junit.Before;
import org.junit.Test;

/**
 * Test unitaire de la classe JdbcDriver.
 * @author Emeric Vernat
 */
public class TestJdbcDriver {
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
}
