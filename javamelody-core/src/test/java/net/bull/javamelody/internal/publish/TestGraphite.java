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
package net.bull.javamelody.internal.publish;

import java.io.IOException;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import net.bull.javamelody.Parameter;
import net.bull.javamelody.Utils;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Test unitaire de la classe Graphite.
 * @author Emeric Vernat
 */
class TestGraphite {
	/**
	 * Initialisation.
	 */
	@BeforeEach
	void setUp() {
		Utils.initialize();
	}

	/** Test.
	 * @throws IOException e */
	@Test
	void test() throws IOException {
		final Graphite graphite0 = Graphite.getInstance("/test", "hostname");
		assertNull(graphite0, "getInstance");
		setProperty(Parameter.GRAPHITE_ADDRESS, "localhost:2003");
		final Graphite graphite = Graphite.getInstance("/test", "hostname");
		assertNotNull(graphite, "getInstance");
		graphite.addValue("metric", 1);
		graphite.addValue("metric", 2);
		graphite.addValue("metric", 3);
		assertThrows(IOException.class, () -> graphite.send());
		setProperty(Parameter.GRAPHITE_ADDRESS, "localhost");
		assertThrows(IOException.class, () ->
			Graphite.getInstance("/test", "hostname").send()
		);
		setProperty(Parameter.GRAPHITE_ADDRESS, null);
		graphite.stop();
	}

	private static void setProperty(Parameter parameter, String value) {
		Utils.setProperty(parameter, value);
	}
}
