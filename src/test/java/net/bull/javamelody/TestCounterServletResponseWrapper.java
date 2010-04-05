/*
 * Copyright 2008-2010 by Emeric Vernat
 *
 *     This file is part of Java Melody.
 *
 * Java Melody is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Java Melody is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Java Melody.  If not, see <http://www.gnu.org/licenses/>.
 */
package net.bull.javamelody;

import static org.junit.Assert.assertEquals;

import java.io.IOException;

import net.bull.javamelody.TestCompressionServletResponseWrapper.HttpResponse;

import org.junit.Test;

/**
 * Test unitaire des classes CounterServletResponseWrapper et CounterResponseStream.
 * @author Emeric Vernat
 */
public class TestCounterServletResponseWrapper {
	/** Test.
	 * @throws IOException e */
	@Test
	public void testCounterServletResponseWrapper() throws IOException {
		final CounterServletResponseWrapper wrapper = new CounterServletResponseWrapper(
				new HttpResponse());
		wrapper.createOutputStream();
		wrapper.getOutputStream().write(new byte[8]);
		assertEquals("dataLength", 8, wrapper.getDataLength());
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
		wrapper.flush();
		wrapper.close();
	}
}
