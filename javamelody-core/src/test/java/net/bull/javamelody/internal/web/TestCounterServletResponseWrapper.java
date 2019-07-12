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
package net.bull.javamelody.internal.web;

import static org.junit.Assert.assertEquals;

import java.io.IOException;

import org.junit.Before;
import org.junit.Test;

import net.bull.javamelody.Utils;
import net.bull.javamelody.internal.web.TestCompressionServletResponseWrapper.HttpResponse;

/**
 * Test unitaire des classes CounterServletResponseWrapper et CounterResponseStream.
 * @author Emeric Vernat
 */
public class TestCounterServletResponseWrapper {
	/** Check. */
	@Before
	public void setUp() {
		Utils.initialize();
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testCounterServletResponseWrapper() throws IOException {
		final CounterServletResponseWrapper wrapper = new CounterServletResponseWrapper(
				new HttpResponse());
		// stream est null pour l'instant
		wrapper.reset();
		wrapper.resetBuffer();

		wrapper.createOutputStream();
		wrapper.getOutputStream().write(new byte[8]);
		assertEquals("dataLength", 8, wrapper.getDataLength());
		wrapper.reset();
		assertEquals("dataLength with reset", 0, wrapper.getDataLength());
		wrapper.getOutputStream().write(new byte[8]);
		wrapper.resetBuffer();
		assertEquals("dataLength with resetBuffer", 0, wrapper.getDataLength());
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testCounterResponseStream() throws IOException {
		final CounterResponseStream wrapper = new CounterResponseStream(new HttpResponse());
		wrapper.write(1);
		wrapper.write(new byte[8]);
		wrapper.write(new byte[8], 1, 7);
		assertEquals("dataLength", 16, wrapper.getDataLength());
		wrapper.isReady();
		wrapper.setWriteListener(null);
		wrapper.flush();
		wrapper.close();
	}
}
