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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;

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
		assertNotNull(rumData.toString(), "toString");
		assertEquals(0, rumData.getHits(), "getHits");
		assertEquals(-1, rumData.getNetworkTimeMean(), "getNetworkTimeMean");
		assertEquals(-1, rumData.getDomProcessingMean(), "getDomProcessingMean");
		assertEquals(-1, rumData.getPageRenderingMean(), "getPageRenderingMean");

		rumData.addHits(rumData);
		assertEquals(0, rumData.getHits(), "getHits");
		rumData.removeHits(rumData);
		assertEquals(0, rumData.getHits(), "getHits");
		rumData.addHit(-1, 0, 0);
		rumData.addHit(0, -1, 0);
		rumData.addHit(0, 0, -1);
		rumData.addHit(300001, 0, 0);
		rumData.addHit(0, 300001, 0);
		rumData.addHit(0, 0, 300001);
		assertEquals(0, rumData.getHits(), "getHits");

		rumData.addHit(10, 20, 30);
		assertEquals(1, rumData.getHits(), "getHits");
		rumData.addHit(10, 20, 30);
		assertEquals(2, rumData.getHits(), "getHits");
		assertEquals(10, rumData.getNetworkTimeMean(), "getNetworkTimeMean");
		assertEquals(20, rumData.getDomProcessingMean(), "getDomProcessingMean");
		assertEquals(30, rumData.getPageRenderingMean(), "getPageRenderingMean");

		final CounterRequestRumData clone = rumData.clone();
		assertEquals(2, clone.getHits(), "getHits");
		assertEquals(10, clone.getNetworkTimeMean(), "getNetworkTimeMean");
		assertEquals(20, clone.getDomProcessingMean(), "getDomProcessingMean");
		assertEquals(30, clone.getPageRenderingMean(), "getPageRenderingMean");

		rumData.addHits(rumData);
		assertEquals(4, rumData.getHits(), "getHits");
		rumData.removeHits(rumData);
		assertEquals(0, rumData.getHits(), "getHits");
	}
}
