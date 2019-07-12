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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import org.junit.Before;
import org.junit.Test;

import net.bull.javamelody.CounterRequestMXBean.CounterRequestAggregationData;
import net.bull.javamelody.CounterRequestMXBean.CounterRequestData;
import net.bull.javamelody.CounterRequestMXBean.CounterRequestMXBeanImpl;
import net.bull.javamelody.internal.model.Counter;

/**
 * Test unitaire des classes internes de CounterRequestMXBean.
 * @author Emeric Vernat
 */
public class TestCounterRequestMxBean {

	/** Before. */
	@Before
	public void setUp() {
		Utils.initialize();
	}

	/** Test. */
	@Test
	public void test() {
		final Counter counter = new Counter("http", "db.png");
		counter.addRequest("test 1", 10, 10, 10, false, 100);
		final CounterRequestMXBeanImpl counterRequestMXBeanImpl = new CounterRequestMXBeanImpl(
				counter);
		final CounterRequestAggregationData counterRequestAggregation = counterRequestMXBeanImpl
				.getCounterRequestAggregation();
		assertNotNull("getGlobalRequest", counterRequestAggregation.getGlobalRequest());
		assertNotNull("getWarningRequest", counterRequestAggregation.getWarningRequest());
		assertNotNull("getSevereRequest", counterRequestAggregation.getSevereRequest());
		assertNotNull("getWarningThreshold", counterRequestAggregation.getWarningThreshold());
		assertNotNull("getSevereThreshold", counterRequestAggregation.getSevereThreshold());
		assertNotNull("getRequests", counterRequestAggregation.getRequests());
		for (final CounterRequestData counterRequestData : counterRequestAggregation.getRequests()
				.values()) {
			assertEquals("getName", "test 1", counterRequestData.getName());
			assertEquals("getHits", 1, counterRequestData.getHits());
			assertEquals("getMean", 10, counterRequestData.getMean());
			assertEquals("getMaximum", 10, counterRequestData.getMaximum());
			assertEquals("getStandardDeviation", 0, counterRequestData.getStandardDeviation());
			assertEquals("getCpuTimeMean", 10, counterRequestData.getCpuTimeMean());
			assertEquals("getAllocatedKBytesMean", 10, counterRequestData.getAllocatedKBytesMean());
			assertEquals("getSystemErrorPercentage", 0,
					counterRequestData.getSystemErrorPercentage(), 0);
			assertEquals("getResponseSizeMean", 100, counterRequestData.getResponseSizeMean());
		}
	}
}
