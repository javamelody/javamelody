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

import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import net.bull.javamelody.Parameter;
import net.bull.javamelody.Utils;
import net.bull.javamelody.internal.model.JavaInformations;

/**
 * Test unitaire de la classe StatsD.
 * @author Emeric Vernat
 */
public class TestMetricsPublisher {
	/**
	 * Initialisation.
	 */
	@Before
	public void setUp() {
		Utils.initialize();
	}

	/** Test. */
	@Test
	public void test() {
		final List<JavaInformations> javaInformationsList = new ArrayList<JavaInformations>();
		javaInformationsList.add(new JavaInformations(null, false));
		javaInformationsList.add(new JavaInformations(null, false));
		assertEquals("getMetricsPublishers", 0,
				MetricsPublisher.getMetricsPublishers(javaInformationsList).size());
		setProperty(Parameter.GRAPHITE_ADDRESS, "localhost:2003");
		setProperty(Parameter.STATSD_ADDRESS, "localhost:8125");
		setProperty(Parameter.CLOUDWATCH_NAMESPACE, "MyCompany/MyAppDomain");
		System.setProperty("aws.region", "us-west-1");
		setProperty(Parameter.INFLUXDB_URL, "http://localhost:8086/write?db=mydb");
		setProperty(Parameter.DATADOG_API_KEY, "9775a026f1ca7d1c6c5af9d94d9595a4");
		assertEquals("getMetricsPublishers", 5,
				MetricsPublisher.getMetricsPublishers(javaInformationsList).size());
	}

	private static void setProperty(Parameter parameter, String value) {
		Utils.setProperty(parameter, value);
	}
}
