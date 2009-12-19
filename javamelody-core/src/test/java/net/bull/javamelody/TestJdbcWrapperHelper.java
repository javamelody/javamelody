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

import static org.junit.Assert.assertNotNull;

import javax.naming.NamingException;

import org.junit.Test;

/**
 * Test unitaire de la classe JdbcWrapperHelper.
 * @author Emeric Vernat
 */
public class TestJdbcWrapperHelper {
	/** Test. */
	@Test
	public void testGetDataSources() {
		getDataSources();
		setProperty(Parameter.DATASOURCES, "");
		getDataSources();
		setProperty(Parameter.DATASOURCES, "testDataSource");
		getDataSources();
		setProperty(Parameter.DATASOURCES, null);
	}

	private void getDataSources() {
		try {
			JdbcWrapperHelper.getDataSources();
		} catch (NamingException e) {
			assertNotNull("ok", e);
		}
	}

	/** Test. */
	@Test
	public void testGetAccessibleField() {
		// sqlCounter est un champ privé qui existe comme un autre
		assertNotNull("getAccessibleField", JdbcWrapperHelper.getAccessibleField(
				JdbcWrapper.SINGLETON, "sqlCounter"));
	}

	private static void setProperty(Parameter parameter, String value) {
		if (value == null) {
			System.getProperties().remove(Parameters.PARAMETER_SYSTEM_PREFIX + parameter.getCode());
		} else {
			System.setProperty(Parameters.PARAMETER_SYSTEM_PREFIX + parameter.getCode(), value);
		}
	}
}
