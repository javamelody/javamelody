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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * Test unitaire de la classe Parameter.
 * @author Emeric Vernat
 */
public class TestParameter {
	/** Check. */
	@BeforeEach
	public void setUp() {
		Utils.initialize();
	}

	/** Test. */
	@Test
	public void testGetParameterValue() {
		assertNull(Parameter.DATASOURCES.getValue(), "getValue");
	}

	/** Test. */
	@Test
	public void testSetParameterValue() {
		Parameter.DATASOURCES.setValue("jdbc/DataSource");
		assertEquals(Parameter.DATASOURCES.getValue(), "jdbc/DataSource", "getValue");
	}

	/** Test. */
	@Test
	public void testParameterValueOfIgnoreCase() {
		assertNotNull(Parameter.valueOfIgnoreCase(Parameter.DISABLED.toString()), "Parameter.valueOfIgnoreCase");
	}
}
