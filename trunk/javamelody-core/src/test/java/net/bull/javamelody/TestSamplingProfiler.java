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

import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Arrays;

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
		assertTrue(samplingProfiler.getHotspots(NB_ROWS).isEmpty());
		samplingProfiler.update();
	}

	/**
	 * Test.
	 */
	@Test
	public void test2() {
		final SamplingProfiler samplingProfiler = new SamplingProfiler(new ArrayList<String>());
		assertTrue(samplingProfiler.getHotspots(NB_ROWS).isEmpty());
		samplingProfiler.update();
		samplingProfiler.clear();
		assertTrue(samplingProfiler.getHotspots(NB_ROWS).isEmpty());
	}

	/**
	 * Test.
	 */
	@Test
	public void testConstructor() {
		final SamplingProfiler samplingProfiler = new SamplingProfiler(Arrays.asList("java",
				"javax."));
		assertTrue(samplingProfiler.getHotspots(NB_ROWS).isEmpty());
	}

	/**
	 * Test.
	 */
	@SuppressWarnings("unused")
	@Test(expected = Exception.class)
	public void testConstructor2() {
		new SamplingProfiler(Arrays.asList(" "));
	}
}
