/*
 * Copyright 2008-2009 by Emeric Vernat, Bull
 *
 *     This file is part of Java Melody.
 *fv
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

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import java.sql.SQLException;
import java.util.List;
import java.util.Properties;

import javax.naming.NamingException;

import net.bull.javamelody.DatabaseInformations.Database;

import org.junit.Before;
import org.junit.Test;

/**
 * Test unitaire de la classe DatabaseInformations.
 * @author Emeric Vernat
 */
public class TestDatabaseInformations {
	static void initH2() {
		// nécessite la dépendance vers la base de données H2
		final Properties info = new Properties();
		info.put("driver", "org.h2.Driver");
		try {
			JdbcDriver.SINGLETON.connect(TestJdbcWrapper.H2_DATABASE_URL, info);
		} catch (final SQLException e) {
			throw new IllegalStateException(e);
		}
	}

	/** Test. */
	@Before
	public void setUp() {
		initH2();
	}

	/** Test.
	 * @throws NamingException e
	 * @throws SQLException e */
	@Test
	public void testDatabaseInformations() throws SQLException, NamingException {
		final int requestIndex = 0;
		final DatabaseInformations databaseInformations = new DatabaseInformations(requestIndex);
		assertSame("getSelectedRequestIndex", requestIndex, databaseInformations
				.getSelectedRequestIndex());
		assertNotNull("getSelectedRequestName", databaseInformations.getSelectedRequestName());
		assertNotNull("getNbColumns", databaseInformations.getNbColumns());
		assertNotNull("getResult", databaseInformations.getResult());
		assertNotNull("getRequestNames", databaseInformations.getRequestNames());
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
