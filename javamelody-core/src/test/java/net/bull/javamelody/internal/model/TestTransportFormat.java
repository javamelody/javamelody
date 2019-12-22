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

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.Serializable;
import java.util.Locale;

import org.ehcache.Status;
import org.junit.Before;
import org.junit.Test;

import net.bull.javamelody.Utils;
import net.bull.javamelody.internal.common.InputOutput;

/**
 * Test unitaire de la classe TransportFormat.
 * @author Emeric Vernat
 */
public class TestTransportFormat {
	/** Check. */
	@Before
	public void setUp() {
		Utils.initialize();
	}

	private Counter createCounter() {
		final Counter counter = new Counter("test transport format", null);
		counter.addRequest("test1", 0, 0, 0, false, 1000);
		counter.addRequest("test2", 1000, 500, 500, false, 1000);
		counter.addRequest("test3", 10000, 500, 500, true, 10000);
		return counter;
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testWriteSerialized() throws IOException {
		final Counter counter = createCounter();
		final ByteArrayOutputStream output = new ByteArrayOutputStream();
		TransportFormat.SERIALIZED.writeSerializableTo(counter, output);
		final String message = "flux vide";
		assertTrue(message, output.size() > 0);
		output.reset();
		TransportFormat.SERIALIZED.writeSerializableTo(null, output);
		assertTrue(message, output.size() > 0);
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testWriteXml() throws IOException {
		final Counter counter = createCounter();
		final ByteArrayOutputStream output = new ByteArrayOutputStream();
		TransportFormat.XML.writeSerializableTo(counter, output);
		assertTrue("flux vide", output.size() > 0);
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testWriteJson() throws IOException {
		final Counter counter = createCounter();
		final ByteArrayOutputStream output = new ByteArrayOutputStream();
		TransportFormat.JSON.writeSerializableTo(counter, output);
		assertTrue("flux vide", output.size() > 0);
	}

	/** Test.
	 * @throws IOException e
	 * @throws ClassNotFoundException e */
	@Test
	public void testReadSerializable() throws IOException, ClassNotFoundException {
		final Counter counter = createCounter();
		final Counter after = (Counter) serialize(counter);
		assertEquals("counter", counter.toString(), after.toString());

		assertNull("null", serialize(null));

		final String[][] array = {};
		assertArrayEquals("array", array, (String[][]) serialize(array));
		final boolean[] barray = {};
		assertArrayEquals("boolean", barray, (boolean[]) serialize(barray));
		final File file = new File("test");
		assertEquals("file", file, serialize(file));
		try {
			// objects from not white-listed packages should not be deserialized
			assertNull("should not return a result", serialize(Status.UNINITIALIZED));
		} catch (final ClassNotFoundException e) {
			assertNotNull("e", e);
		}
	}

	private static Serializable serialize(Serializable serializable)
			throws IOException, ClassNotFoundException {
		final ByteArrayOutputStream output = new ByteArrayOutputStream();
		TransportFormat.SERIALIZED.writeSerializableTo(serializable, output);
		final ByteArrayInputStream input = new ByteArrayInputStream(output.toByteArray());
		return TransportFormat.SERIALIZED.readSerializableFrom(input);
	}

	/** Test.
	 * @throws IOException e
	 * @throws ClassNotFoundException e */
	@Test
	public void testReadXml() throws IOException, ClassNotFoundException {
		final Counter counter = createCounter();
		final ByteArrayOutputStream output = new ByteArrayOutputStream();
		TransportFormat.XML.writeSerializableTo(counter, output);
		final ByteArrayInputStream input = new ByteArrayInputStream(output.toByteArray());
		final Counter after = (Counter) TransportFormat.XML.readSerializableFrom(input);
		assertEquals("counter", counter.toString(), after.toString());
	}

	/** Test. */
	@Test
	public void testReadJson() {
		Exception result = null;
		try {
			TransportFormat.JSON.readSerializableFrom(new ByteArrayInputStream(new byte[0]));
		} catch (final Exception e) {
			result = e;
		}
		// la désérialisation d'un flux json n'est pas possible
		assertNotNull("readJson", result);
	}

	/** Test. */
	@Test
	public void testGetCode() {
		for (final TransportFormat tf : TransportFormat.values()) {
			assertSame("same", tf, TransportFormat.valueOfIgnoreCase(tf.getCode()));
		}
	}

	/** Test. */
	@Test
	public void testGetMimeType() {
		for (final TransportFormat tf : TransportFormat.values()) {
			assertNotNull("mimeType", tf.getMimeType());
		}
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testPump() throws IOException {
		final Counter counter = createCounter();
		final ByteArrayOutputStream output = new ByteArrayOutputStream();
		TransportFormat.XML.writeSerializableTo(counter, output);
		final byte[] byteArray = output.toByteArray();
		final ByteArrayInputStream input = new ByteArrayInputStream(byteArray);
		output.reset();
		InputOutput.pump(input, output);
		assertArrayEquals("array equals", byteArray, output.toByteArray());
	}

	/** Test. */
	@Test
	public void testIsATransportFormat() {
		final String message = "isATransportFormat";
		assertFalse(message, TransportFormat.isATransportFormat(null));
		for (final TransportFormat transportFormat : TransportFormat.values()) {
			final String format = transportFormat.toString();
			assertTrue(message, TransportFormat.isATransportFormat(format));
			assertTrue(message,
					TransportFormat.isATransportFormat(format.toLowerCase(Locale.getDefault())));
			assertTrue(message, TransportFormat.isATransportFormat(format + ' '));
		}
		assertFalse(message, TransportFormat.isATransportFormat("n'importe quoi"));
	}
}
