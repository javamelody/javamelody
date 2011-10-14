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

import static org.junit.Assert.assertNotNull;

import javax.naming.NamingException;

import org.junit.Before;
import org.junit.Test;

/**
 * Test unitaire de la classe JdbcWrapperHelper.
 * @author Emeric Vernat
 */
public class TestJdbcWrapperHelper {
	/** Check. */
	@Before
	public void setUp() {
		Utils.initialize();
	}

	/** Test. */
	@Test
	public void testGetJndiDataSources() {
		getJndiDataSources();
		setProperty(Parameter.DATASOURCES, "");
		getJndiDataSources();
		setProperty(Parameter.DATASOURCES, "testDataSource");
		getJndiDataSources();
		setProperty(Parameter.DATASOURCES, null);
	}

	/** Test. */
	@Test
	public void testGetJndiAndSpringDataSources() {
		getJndiAndSpringDataSources();
	}

	private void getJndiDataSources() {
		try {
			JdbcWrapperHelper.getJndiDataSources();
		} catch (final NamingException e) {
			assertNotNull("ok", e);
		}
	}

	private void getJndiAndSpringDataSources() {
		try {
			JdbcWrapperHelper.getJndiAndSpringDataSources();
		} catch (final NamingException e) {
			assertNotNull("ok", e);
		}
	}

	/** Test.
	 * @throws IllegalAccessException e */
	@Test
	public void testGetFieldValue() throws IllegalAccessException {
		// sqlCounter est un champ privé qui existe comme un autre
		assertNotNull("getFieldValue",
				JdbcWrapperHelper.getFieldValue(JdbcWrapper.SINGLETON, "sqlCounter"));
	}

	/** Test.
	 * @throws IllegalAccessException e */
	@Test
	public void testSetFieldValue() throws IllegalAccessException {
		// sqlCounter est un champ privé qui existe comme un autre
		final Object value = JdbcWrapperHelper.getFieldValue(JdbcWrapper.SINGLETON, "sqlCounter");
		JdbcWrapperHelper.setFieldValue(JdbcWrapper.SINGLETON, "sqlCounter", value);
	}

	private static void setProperty(Parameter parameter, String value) {
		Utils.setProperty(parameter, value);
	}
}
