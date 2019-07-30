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
package net.bull.javamelody.internal.model;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.List;
import java.util.Properties;

import javax.naming.NamingException;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import net.bull.javamelody.JdbcDriver;
import net.bull.javamelody.TestJdbcWrapper;
import net.bull.javamelody.Utils;
import net.bull.javamelody.internal.common.Parameters;
import net.bull.javamelody.internal.model.DatabaseInformations.Database;

/**
 * Test unitaire de la classe DatabaseInformations.
 * @author Emeric Vernat
 */
public class TestDatabaseInformations {
	private Connection connection;

	public static Connection initH2() {
		// nécessite la dépendance vers la base de données H2
		final Properties info = new Properties();
		info.put("driver", "org.h2.Driver");
		try {
			return new JdbcDriver().connect(TestJdbcWrapper.H2_DATABASE_URL, info);
		} catch (final SQLException e) {
			throw new IllegalStateException(e);
		}
	}

	public static void initJdbcDriverParameters() {
		Parameters.initJdbcDriverParameters(TestJdbcWrapper.H2_DATABASE_URL, new Properties());
	}

	/** setup. */
	@Before
	public void setUp() {
		Utils.initialize();
		connection = initH2();
	}

	/** tearDown. */
	@After
	public void tearDown() {
		if (connection != null) {
			try {
				connection.close();
			} catch (final SQLException e) {
				throw new IllegalStateException(e);
			}
		}
	}

	/** Test.
	 * @throws NamingException e
	 * @throws SQLException e */
	@Test
	public void testDatabaseInformations() throws SQLException, NamingException {
		final int requestIndex = 0;
		final DatabaseInformations databaseInformations = new DatabaseInformations(requestIndex);
		assertSame("getSelectedRequestIndex", requestIndex,
				databaseInformations.getSelectedRequestIndex());
		assertNotNull("getSelectedRequestName", databaseInformations.getSelectedRequestName());
		assertNotNull("getNbColumns", databaseInformations.getNbColumns());
		assertNotNull("getResult", databaseInformations.getResult());
		assertNotNull("getRequestNames", databaseInformations.getRequestNames());
		assertNotNull("toString", databaseInformations.toString());
	}

	/** Test. */
	@Test
	public void testDatabase() {
		for (final Database database : Database.values()) {
			final List<String> requestNames = database.getRequestNames();
			assertTrue("getRequestNames", requestNames != null && !requestNames.isEmpty());
			for (final String requestName : requestNames) {
				assertNotNull("getRequestByName", database.getRequestByName(requestName));
			}
		}
	}

	/** Test.
	 * @throws NamingException e
	 * @throws SQLException e */
	@Test
	public void testExplainPlanFor() throws SQLException, NamingException {
		DatabaseInformations.explainPlanFor("select 1");
	}
}
