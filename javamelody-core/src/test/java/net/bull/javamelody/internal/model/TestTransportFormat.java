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

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.Serializable;
import java.util.Locale;

import org.ehcache.Status;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import net.bull.javamelody.Utils;
import net.bull.javamelody.internal.common.InputOutput;

/**
 * Test unitaire de la classe TransportFormat.
 * @author Emeric Vernat
 */
class TestTransportFormat {
	/** Check. */
	@BeforeEach
	void setUp() {
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
	void testWriteSerialized() throws IOException {
		final Counter counter = createCounter();
		final ByteArrayOutputStream output = new ByteArrayOutputStream();
		TransportFormat.SERIALIZED.writeSerializableTo(counter, output);
		final String message = "flux vide";
		assertTrue(output.size() > 0, message);
		output.reset();
		TransportFormat.SERIALIZED.writeSerializableTo(null, output);
		assertTrue(output.size() > 0, message);
	}

	/** Test.
	 * @throws IOException e */
	@Test
	void testWriteXml() throws IOException {
		final Counter counter = createCounter();
		final ByteArrayOutputStream output = new ByteArrayOutputStream();
		TransportFormat.XML.writeSerializableTo(counter, output);
		assertTrue(output.size() > 0, "flux vide");
	}

	/** Test.
	 * @throws IOException e */
	@Test
	void testWriteJson() throws IOException {
		final Counter counter = createCounter();
		final ByteArrayOutputStream output = new ByteArrayOutputStream();
		TransportFormat.JSON.writeSerializableTo(counter, output);
		assertTrue(output.size() > 0, "flux vide");
	}

	/** Test.
	 * @throws IOException e
	 * @throws ClassNotFoundException e */
	@Test
	void testReadSerializable() throws IOException, ClassNotFoundException {
		final Counter counter = createCounter();
		final Counter after = (Counter) serialize(counter);
		assertEquals(counter.toString(), after.toString(), "counter");

		assertNull(serialize(null), "null");

		final String[][] array = {};
		assertArrayEquals(array, (String[][]) serialize(array), "array");
		final boolean[] barray = {};
		assertArrayEquals(barray, (boolean[]) serialize(barray), "boolean");
		final File file = new File("test");
		assertEquals(file, serialize(file), "file");
		try {
			// objects from not white-listed packages should not be deserialized
			assertNull(serialize(Status.UNINITIALIZED), "should not return a result");
		} catch (final ClassNotFoundException e) {
			assertNotNull(e, "e");
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
	void testReadXml() throws IOException, ClassNotFoundException {
		final Counter counter = createCounter();
		final ByteArrayOutputStream output = new ByteArrayOutputStream();
		TransportFormat.XML.writeSerializableTo(counter, output);
		final ByteArrayInputStream input = new ByteArrayInputStream(output.toByteArray());
		final Counter after = (Counter) TransportFormat.XML.readSerializableFrom(input);
		assertEquals(counter.toString(), after.toString(), "counter");
	}

	/** Test. */
	@Test
	void testReadJson() {
		Exception result = null;
		try {
			TransportFormat.JSON.readSerializableFrom(new ByteArrayInputStream(new byte[0]));
		} catch (final Exception e) {
			result = e;
		}
		// la désérialisation d'un flux json n'est pas possible
		assertNotNull(result, "readJson");
	}

	/** Test. */
	@Test
	void testGetCode() {
		for (final TransportFormat tf : TransportFormat.values()) {
			assertSame(tf, TransportFormat.valueOfIgnoreCase(tf.getCode()), "same");
		}
	}

	/** Test. */
	@Test
	void testGetMimeType() {
		for (final TransportFormat tf : TransportFormat.values()) {
			assertNotNull(tf.getMimeType(), "mimeType");
		}
	}

	/** Test.
	 * @throws IOException e */
	@Test
	void testPump() throws IOException {
		final Counter counter = createCounter();
		final ByteArrayOutputStream output = new ByteArrayOutputStream();
		TransportFormat.XML.writeSerializableTo(counter, output);
		final byte[] byteArray = output.toByteArray();
		final ByteArrayInputStream input = new ByteArrayInputStream(byteArray);
		output.reset();
		InputOutput.pump(input, output);
		assertArrayEquals(byteArray, output.toByteArray(), "array equals");
	}

	/** Test. */
	@Test
	void testIsATransportFormat() {
		final String message = "isATransportFormat";
		assertFalse(TransportFormat.isATransportFormat(null), message);
		for (final TransportFormat transportFormat : TransportFormat.values()) {
			final String format = transportFormat.toString();
			assertTrue(TransportFormat.isATransportFormat(format), message);
			assertTrue(TransportFormat.isATransportFormat(format.toLowerCase(Locale.getDefault())), message);
			assertTrue(TransportFormat.isATransportFormat(format + ' '), message);
		}
		assertFalse(TransportFormat.isATransportFormat("n'importe quoi"), message);
	}
}
