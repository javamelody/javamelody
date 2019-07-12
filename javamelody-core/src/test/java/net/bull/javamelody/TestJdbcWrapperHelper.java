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
