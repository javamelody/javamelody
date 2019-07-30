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
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

/**
 * Test unitaire pour Stopwatch.
 * @author Emeric Vernat
 */
public class TestStopwatch {
	@Test
	public void test() throws Exception {
		final int requestsCount = MonitoringProxy.getServicesCounter().getRequestsCount();
		final Stopwatch stopwatch = new Stopwatch("stopwatch name");
		assertEquals("getName", "stopwatch name", stopwatch.getName());
		assertTrue("getDuration", stopwatch.getDuration() >= 0);
		assertFalse("isClosed", stopwatch.isClosed());
		stopwatch.close();
		assertTrue("isClosed", stopwatch.isClosed());
		final long duration = stopwatch.getDuration();
		Thread.sleep(10);
		assertEquals("getDuration", duration, stopwatch.getDuration());
		assertEquals("requestsCount", requestsCount + 1,
				MonitoringProxy.getServicesCounter().getRequestsCount());
		try {
			stopwatch.close();
		} catch (final IllegalStateException e) {
			assertNotNull("e", e);
		}
	}
}
