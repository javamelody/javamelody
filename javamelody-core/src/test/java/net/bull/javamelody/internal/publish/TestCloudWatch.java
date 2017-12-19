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
import static org.junit.Assert.assertTrue;

import java.io.IOException;

import org.junit.Before;
import org.junit.Test;

import com.amazonaws.SdkClientException;

import net.bull.javamelody.Parameter;
import net.bull.javamelody.Utils;

/**
 * Test unitaire de la classe CloudWatch.
 * @author Emeric Vernat
 */
public class TestCloudWatch {
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
		CloudWatch cloudWatch = CloudWatch.getInstance("/test", "hostname");
		assertNull("getInstance", cloudWatch);
		setProperty(Parameter.CLOUDWATCH_NAMESPACE, "MyCompany/MyAppDomain");
		System.setProperty("aws.region", "us-west-1");
		try {
			cloudWatch = CloudWatch.getInstance("/test", "hostname");
		} catch (final NoClassDefFoundError e) {
			// for ant tests
			return;
		}
		assertNotNull("getInstance", cloudWatch);
		cloudWatch.addValue("metric", 1);
		cloudWatch.addValue("metric", 2);
		cloudWatch.addValue("metric", 3);
		boolean exception = false;
		try {
			cloudWatch.send();
		} catch (final SdkClientException e) {
			exception = true;
		} catch (final IOException e) {
			exception = true;
		}
		assertTrue("no credentials provided", exception);
		setProperty(Parameter.CLOUDWATCH_NAMESPACE, null);
		System.getProperties().remove("aws.region");
		cloudWatch.stop();
	}

	private static void setProperty(Parameter parameter, String value) {
		Utils.setProperty(parameter, value);
	}
}
