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
import static org.junit.Assert.assertTrue;

import java.sql.SQLException;
import java.util.Properties;

import org.junit.Before;
import org.junit.Test;

/**
 * Test unitaire de la classe DatabaseInformations.
 * @author Emeric Vernat
 */
public class TestDatabaseInformations {
	private static final String H2_DATABASE_URL = "jdbc:h2:~/.h2/test";

	static void initH2() {
		// nécessite la dépendance vers la base de données H2
		final Properties info = new Properties();
		info.put("driver", "org.h2.Driver");
		try {
			JdbcDriver.SINGLETON.connect(H2_DATABASE_URL, info);
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
	 * @throws Exception e */
	@Test
	public void testDatabaseInformations() throws Exception {
		final int requestIndex = 0;
		final DatabaseInformations databaseInformations = new DatabaseInformations(requestIndex);
		assertTrue("getRequestIndex", databaseInformations.getRequestIndex() == requestIndex);
		assertNotNull("getResult", databaseInformations.getResult());
		assertNotNull("getResult", databaseInformations.getRequestNames());
	}
}
