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

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.amazonaws.SdkClientException;

import net.bull.javamelody.Parameter;
import net.bull.javamelody.Utils;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Test unitaire de la classe CloudWatch.
 * @author Emeric Vernat
 */
class TestCloudWatch {
	/**
	 * Initialisation.
	 */
	@BeforeEach
	void setUp() {
		Utils.initialize();
	}

	/** Test. */
	@Test
	void test() {
		final CloudWatch cloudWatch = CloudWatch.getInstance("/test", "hostname");
		assertNull(cloudWatch, "getInstance");
		setProperty(Parameter.CLOUDWATCH_NAMESPACE, "MyCompany/MyAppDomain");
		System.setProperty("aws.region", "us-west-1");
		final CloudWatch cloudWatch2 = CloudWatch.getInstance("/test", "hostname");
		assertNotNull(cloudWatch2, "getInstance");
		cloudWatch2.addValue("metric", 1);
		cloudWatch2.addValue("metric", 2);
		cloudWatch2.addValue("metric", 3);
		assertThrows(SdkClientException.class, () ->
			// no credentials provided
			cloudWatch2.send()
		);
		setProperty(Parameter.CLOUDWATCH_NAMESPACE, null);
		System.getProperties().remove("aws.region");
		cloudWatch2.stop();
	}

	private static void setProperty(Parameter parameter, String value) {
		Utils.setProperty(parameter, value);
	}
}
