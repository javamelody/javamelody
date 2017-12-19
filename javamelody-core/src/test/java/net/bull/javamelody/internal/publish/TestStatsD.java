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
public class TestStatsD {
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
		Statsd statsd = Statsd.getInstance("/test", "hostname");
		assertNull("getInstance", statsd);
		setProperty(Parameter.STATSD_ADDRESS, "localhost:8125");
		statsd = Statsd.getInstance("/test", "hostname");
		assertNotNull("getInstance", statsd);
		statsd.addValue("metric", 1);
		statsd.addValue("metric", 2);
		statsd.addValue("metric", 3);
		statsd.send();
		statsd.stop();
	}

	private static void setProperty(Parameter parameter, String value) {
		Utils.setProperty(parameter, value);
	}
}
