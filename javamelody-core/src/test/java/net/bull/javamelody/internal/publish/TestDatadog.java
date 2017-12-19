/*
 * Copyright 2008-2017 by Emeric Vernat
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

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import java.io.IOException;

import org.junit.Before;
import org.junit.Test;

import net.bull.javamelody.Parameter;
import net.bull.javamelody.Utils;

/**
 * Test unitaire de la classe StatsD.
 * @author Emeric Vernat
 */
public class TestDatadog {
	/**
	 * Initialisation.
	 */
	@Before
	public void setUp() {
		Utils.initialize();
	}

	/** Test. 
	 * @throws IOException e */
	@Test
	public void test() throws IOException {
		Datadog datadog = Datadog.getInstance("/test", "hostname");
		assertNull("getInstance", datadog);
		setProperty(Parameter.DATADOG_API_KEY, "9775a026f1ca7d1c6c5af9d94d9595a4");
		datadog = Datadog.getInstance("/test", "hostname");
		assertNotNull("getInstance", datadog);
		datadog.addValue("metric", 1);
		datadog.addValue("metric", 2);
		datadog.addValue("metric", 3);
		// n'appelons pas le serveur datadog
		// datadog.send();
		datadog.stop();
	}

	private static void setProperty(Parameter parameter, String value) {
		Utils.setProperty(parameter, value);
	}
}
