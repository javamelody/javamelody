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
package net.bull.javamelody.internal.model;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import org.junit.Test;

/**
 * Test for CounterRequestRumData.
 * @author Emeric Vernat
 */
public class TestCounterRequestRumData {
	/**
	 * Test.
	 */
	@Test
	public void test() {
		final CounterRequestRumData rumData = new CounterRequestRumData();
		assertNotNull("toString", rumData.toString());
		assertEquals("getHits", 0, rumData.getHits());
		assertEquals("getNetworkTimeMean", -1, rumData.getNetworkTimeMean());
		assertEquals("getDomProcessingMean", -1, rumData.getDomProcessingMean());
		assertEquals("getPageRenderingMean", -1, rumData.getPageRenderingMean());

		rumData.addHits(rumData);
		assertEquals("getHits", 0, rumData.getHits());
		rumData.removeHits(rumData);
		assertEquals("getHits", 0, rumData.getHits());
		rumData.addHit(-1, 0, 0);
		rumData.addHit(0, -1, 0);
		rumData.addHit(0, 0, -1);
		rumData.addHit(300001, 0, 0);
		rumData.addHit(0, 300001, 0);
		rumData.addHit(0, 0, 300001);
		assertEquals("getHits", 0, rumData.getHits());

		rumData.addHit(10, 20, 30);
		assertEquals("getHits", 1, rumData.getHits());
		rumData.addHit(10, 20, 30);
		assertEquals("getHits", 2, rumData.getHits());
		assertEquals("getNetworkTimeMean", 10, rumData.getNetworkTimeMean());
		assertEquals("getDomProcessingMean", 20, rumData.getDomProcessingMean());
		assertEquals("getPageRenderingMean", 30, rumData.getPageRenderingMean());

		final CounterRequestRumData clone = rumData.clone();
		assertEquals("getHits", 2, clone.getHits());
		assertEquals("getNetworkTimeMean", 10, clone.getNetworkTimeMean());
		assertEquals("getDomProcessingMean", 20, clone.getDomProcessingMean());
		assertEquals("getPageRenderingMean", 30, clone.getPageRenderingMean());

		rumData.addHits(rumData);
		assertEquals("getHits", 4, rumData.getHits());
		rumData.removeHits(rumData);
		assertEquals("getHits", 0, rumData.getHits());
	}
}
