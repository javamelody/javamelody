/*
 * Copyright 2008-2014 by Emeric Vernat
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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.Test;

/**
 * Test unitaire de la classe SamplingProfiler.
 * @author Emeric Vernat
 */
public class TestSamplingProfiler {
	private static final int NB_ROWS = 100;

	/**
	 * Test.
	 */
	@Test
	public void test1() {
		final SamplingProfiler samplingProfiler = new SamplingProfiler();
		assertEmptyHotspots(samplingProfiler);
		samplingProfiler.update();
	}

	/**
	 * Test.
	 */
	@Test
	public void test2() {
		final SamplingProfiler samplingProfiler = new SamplingProfiler(new ArrayList<String>(),
				null);
		assertEmptyHotspots(samplingProfiler);
		samplingProfiler.update();
		samplingProfiler.clear();
		assertEmptyHotspots(samplingProfiler);
		// Start some threads, and wait until they are done
		doSomeWorkAndTakeSample(samplingProfiler);
		// We should now have some samples
		assertNotEmptyHotspots(samplingProfiler);
		samplingProfiler.clear();
		assertEmptyHotspots(samplingProfiler);
	}

	/**
	 * Test that classes from packages are included
	 */
	@Test
	public void testClassesInInclude() {
		final SamplingProfiler samplingProfiler = new SamplingProfiler(null,
				Arrays.asList("net.bull"));
		assertEmptyHotspots(samplingProfiler);
		samplingProfiler.update();
		samplingProfiler.clear();
		assertEmptyHotspots(samplingProfiler);
		doSomeWorkAndTakeSample(samplingProfiler);
		assertNotEmptyHotspots(samplingProfiler);
		samplingProfiler.clear();
		assertEmptyHotspots(samplingProfiler);
	}

	/**
	 * Test that classes from packages are included, where include pattern does not match any packages
	 */
	@Test
	public void testClassesInIncludeNoneMatching() {
		final SamplingProfiler samplingProfiler = new SamplingProfiler(null,
				Arrays.asList("not.matching.package,also.not.matching"));
		assertEmptyHotspots(samplingProfiler);
		samplingProfiler.update();
		assertEmptyHotspots(samplingProfiler);
		doSomeWorkAndTakeSample(samplingProfiler);
		assertEmptyHotspots(samplingProfiler);
		samplingProfiler.clear();
		assertEmptyHotspots(samplingProfiler);
	}

	/**
	 * Test.
	 */
	@Test
	public void testConstructor() {
		final SamplingProfiler samplingProfiler = new SamplingProfiler(Arrays.asList("java",
				"javax."), null);
		assertEmptyHotspots(samplingProfiler);
	}

	/**
	 * Test include packages
	 */
	@Test
	public void testConstructorInclude() {
		final SamplingProfiler samplingProfiler = new SamplingProfiler(null,
				Arrays.asList("net.bull"));
		assertEmptyHotspots(samplingProfiler);
	}

	/**
	 * Test.
	 */
	@SuppressWarnings("unused")
	@Test(expected = Exception.class)
	public void testConstructor2() {
		new SamplingProfiler(Arrays.asList(" "), null);
	}

	private static void assertEmptyHotspots(SamplingProfiler samplingProfiler) {
		assertTrue("empty hotspots", samplingProfiler.getHotspots(NB_ROWS).isEmpty());
	}

	private static void assertNotEmptyHotspots(final SamplingProfiler samplingProfiler) {
		assertFalse("not empty hotspots", samplingProfiler.getHotspots(NB_ROWS).isEmpty());
	}

	private static void doSomeWorkAndTakeSample(SamplingProfiler samplingProfiler) {
		final List<Thread> threads = new ArrayList<Thread>();
		for (int i = 0; i < 5; i++) {
			final Thread thread = new Thread(new DummyTask());
			threads.add(thread);
			thread.start();
		}
		samplingProfiler.update();
		for (final Thread thread : threads) {
			try {
				thread.join(1000);
			} catch (final InterruptedException e) {
				fail("Interrupted while waiting for threads to finish");
			}
		}
	}

	static class DummyTask implements Runnable {
		@Override
		public void run() {
			for (int i = 0; i < 1000000000; i++) {
				Math.sqrt(i);
			}
		}
	}
}
