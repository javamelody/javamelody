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

import org.junit.Before;
import org.junit.Test;

import net.bull.javamelody.Utils;

/**
 * Test unitaire de la classe PID.
 * @author Emeric Vernat
 */
public class TestPID {
	/** Check. */
	@Before
	public void setUp() {
		Utils.initialize();
	}

	/** Test. */
	@Test
	public void testGetPID() {
		assertNotNull("getPID", PID.getPID());
	}

	/** Test. */
	@Test
	public void testGetPIDFromOS() {
		assertNotNull("getPIDFromOS", PID.getPIDFromOS());
	}
}
