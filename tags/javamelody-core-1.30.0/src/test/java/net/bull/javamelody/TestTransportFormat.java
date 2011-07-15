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

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Locale;

import org.junit.Before;
import org.junit.Test;

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
		counter.addRequest("test1", 0, 0, false, 1000);
		counter.addRequest("test2", 1000, 500, false, 1000);
		counter.addRequest("test3", 10000, 500, true, 10000);
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
		final ByteArrayOutputStream output = new ByteArrayOutputStream();
		TransportFormat.SERIALIZED.writeSerializableTo(counter, output);
		final ByteArrayInputStream input = new ByteArrayInputStream(output.toByteArray());
		final Counter after = (Counter) TransportFormat.SERIALIZED.readSerializableFrom(input);
		assertEquals("counter", counter.toString(), after.toString());

		final ByteArrayOutputStream output2 = new ByteArrayOutputStream();
		TransportFormat.SERIALIZED.writeSerializableTo(null, output2);
		final ByteArrayInputStream input2 = new ByteArrayInputStream(output2.toByteArray());
		assertNull("null", TransportFormat.SERIALIZED.readSerializableFrom(input2));
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
		TransportFormat.pump(input, output);
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
