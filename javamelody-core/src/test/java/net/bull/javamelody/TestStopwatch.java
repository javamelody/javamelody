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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

/**
 * Test unitaire pour Stopwatch.
 * @author Emeric Vernat
 */
public class TestStopwatch {
	@Test
	public void test() throws Exception {
		final int requestsCount = MonitoringProxy.getServicesCounter().getRequestsCount();
		final Stopwatch stopwatch = new Stopwatch("stopwatch name");
		assertEquals("stopwatch name", stopwatch.getName(), "getName");
		assertTrue(stopwatch.getDuration() >= 0, "getDuration");
		assertFalse(stopwatch.isClosed(), "isClosed");
		stopwatch.close();
		assertTrue(stopwatch.isClosed(), "isClosed");
		final long duration = stopwatch.getDuration();
		Thread.sleep(10);
		assertEquals(duration, stopwatch.getDuration(), "getDuration");
		assertEquals(requestsCount + 1, MonitoringProxy.getServicesCounter().getRequestsCount(), "requestsCount");
		try {
			stopwatch.close();
		} catch (final IllegalStateException e) {
			assertNotNull(e, "e");
		}
	}
}
