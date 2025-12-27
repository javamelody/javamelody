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

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import net.bull.javamelody.CounterRequestMXBean.CounterRequestAggregationData;
import net.bull.javamelody.CounterRequestMXBean.CounterRequestData;
import net.bull.javamelody.CounterRequestMXBean.CounterRequestMXBeanImpl;
import net.bull.javamelody.internal.model.Counter;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Test unitaire des classes internes de CounterRequestMXBean.
 * @author Emeric Vernat
 */
public class TestCounterRequestMxBean {

	/** Before. */
	@BeforeEach
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
		assertNotNull(counterRequestAggregation.getGlobalRequest(), "getGlobalRequest");
		assertNotNull(counterRequestAggregation.getWarningRequest(), "getWarningRequest");
		assertNotNull(counterRequestAggregation.getSevereRequest(), "getSevereRequest");
		assertTrue(counterRequestAggregation.getWarningThreshold() >= 0, "getWarningThreshold");
		assertTrue(counterRequestAggregation.getSevereThreshold() >= 0, "getSevereThreshold");
		assertNotNull(counterRequestAggregation.getRequests(), "getRequests");
		for (final CounterRequestData counterRequestData : counterRequestAggregation.getRequests()
				.values()) {
			assertEquals("test 1", counterRequestData.getName(), "getName");
			assertEquals(1, counterRequestData.getHits(), "getHits");
			assertEquals(10, counterRequestData.getMean(), "getMean");
			assertEquals(10, counterRequestData.getMaximum(), "getMaximum");
			assertEquals(0, counterRequestData.getStandardDeviation(), "getStandardDeviation");
			assertEquals(10, counterRequestData.getCpuTimeMean(), "getCpuTimeMean");
			assertEquals(10, counterRequestData.getAllocatedKBytesMean(), "getAllocatedKBytesMean");
			assertEquals(0, counterRequestData.getSystemErrorPercentage(), 0, "getSystemErrorPercentage");
			assertEquals(100, counterRequestData.getResponseSizeMean(), "getResponseSizeMean");
		}
	}
}
