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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Test;

import net.bull.javamelody.internal.model.SamplingProfiler.SampledMethod;

import static org.junit.jupiter.api.Assertions.*;

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
		final SamplingProfiler samplingProfiler = new SamplingProfiler(new ArrayList<>(), null);
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
	 * Test that classes from packages are included.
	 */
	@Test
	public void testClassesInInclude() {
		final SamplingProfiler samplingProfiler = new SamplingProfiler(null,
				List.of("net.bull", "java"));
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
	 * Test that classes from packages are included, where include pattern does not match any packages.
	 */
	@Test
	public void testClassesInIncludeNoneMatching() {
		final SamplingProfiler samplingProfiler = new SamplingProfiler(null,
				List.of("not.matching.package,also.not.matching"));
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
		final SamplingProfiler samplingProfiler = new SamplingProfiler(List.of("java", "javax."),
				null);
		assertEmptyHotspots(samplingProfiler);
	}

	/**
	 * Test include packages.
	 */
	@Test
	public void testConstructorInclude() {
		final SamplingProfiler samplingProfiler = new SamplingProfiler(null, List.of("net.bull"));
		assertEmptyHotspots(samplingProfiler);
	}

	/**
	 * Test.
	 */
    @SuppressWarnings("unused")
	@Test
	public void testConstructor2() {
        assertThrows(Exception.class, () ->		new SamplingProfiler(List.of(" "), null));
	}

	/**
	 * Test.
	 */
	@Test
	public void testConstructor3() {
		final SamplingProfiler samplingProfiler = new SamplingProfiler("java,javax.", null);
		assertEmptyHotspots(samplingProfiler);
	}

	/**
	 * Test.
	 * @throws IOException e
	 * @throws ClassNotFoundException e
	 */
	@Test
	public void testSampledMethod() throws IOException, ClassNotFoundException {
		final SampledMethod sampledMethod = new SampledMethod("class1", "method1");
		final SampledMethod sampledMethod1 = new SampledMethod("class1", "method1");
		assertEquals("class1", sampledMethod.getClassName(), "getClassName");
		assertEquals("method1", sampledMethod.getMethodName(), "getMethodName");
		assertEquals(0, sampledMethod.getCount(), "getCount");
		assertEquals(sampledMethod, sampledMethod, "equals");
		assertEquals(sampledMethod, sampledMethod1, "equals");
		assertNotEquals(new SampledMethod("class1", "method2"), sampledMethod, "equals");
		assertNotEquals(new SampledMethod("class2", "method2"), sampledMethod, "equals");
		assertNotEquals(sampledMethod, new Object(), "equals");
		final Object object = null;
		assertNotEquals(object, sampledMethod, "equals");
		assertEquals(sampledMethod.hashCode(), sampledMethod1.hashCode(), "hashCode");
		assertEquals(sampledMethod.toString(), sampledMethod1.toString(), "toString");
		assertEquals(0, sampledMethod.compareTo(sampledMethod1), "compareTo");
		sampledMethod.incrementCount();
		assertEquals(1, sampledMethod.getCount(), "getCount");
		sampledMethod.setCount(2);
		assertEquals(2, sampledMethod.getCount(), "getCount");
		assertEquals(-1, sampledMethod.compareTo(sampledMethod1), "compareTo");
		sampledMethod1.setCount(3);
		assertEquals(+1, sampledMethod.compareTo(sampledMethod1), "compareTo");

		// test readResolve
		final ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
		final ObjectOutputStream objectOutputStream = new ObjectOutputStream(byteArrayOutputStream);
		objectOutputStream.writeObject(sampledMethod);
		objectOutputStream.close();
		final ObjectInputStream objectInputStream = new ObjectInputStream(
				new ByteArrayInputStream(byteArrayOutputStream.toByteArray()));
		final Object sampledMethodNew = objectInputStream.readObject();
		assertEquals(sampledMethod, sampledMethodNew, "readResolve");
		assertEquals(sampledMethod.hashCode(), sampledMethodNew.hashCode(), "readResolve");
	}

	private static void assertEmptyHotspots(SamplingProfiler samplingProfiler) {
		assertTrue(samplingProfiler.getHotspots(NB_ROWS).isEmpty(), "empty hotspots");
	}

	private static void assertNotEmptyHotspots(final SamplingProfiler samplingProfiler) {
		assertFalse(samplingProfiler.getHotspots(NB_ROWS).isEmpty(), "not empty hotspots");
	}

	private static void doSomeWorkAndTakeSample(SamplingProfiler samplingProfiler) {
		final Thread thread = new Thread(new DummyTask());
		thread.start();
		samplingProfiler.update();
	}

	static class DummyTask implements Runnable {
		@Override
		public void run() {
			new Pi().calcPiDigits(1000);
		}
	}

	/**
	 * Compute PI just to have something to do.
	 * from http://rosettacode.org/wiki/Pi#Java
	 */
	static class Pi {
		private static final BigInteger TWO = BigInteger.valueOf(2);
		private static final BigInteger THREE = BigInteger.valueOf(3);
		private static final BigInteger FOUR = BigInteger.valueOf(4);
		private static final BigInteger SEVEN = BigInteger.valueOf(7);

		private BigInteger q = BigInteger.ONE;
		private BigInteger r = BigInteger.ZERO;
		private BigInteger t = BigInteger.ONE;
		private BigInteger k = BigInteger.ONE;
		private BigInteger n = BigInteger.valueOf(3);
		private BigInteger l = BigInteger.valueOf(3);

		/**
		 * Start.
		 * @param limit Number of digits
		 */
		public void calcPiDigits(int limit) {
			BigInteger nn;
			BigInteger nr;
			boolean first = true;
			int count = 0;
			while (true) {
				if (FOUR.multiply(q).add(r).subtract(t).compareTo(n.multiply(t)) == -1) {
					System.out.print(n); // NOPMD
					count++;
					if (count >= limit) {
						return;
					}
					if (first) {
						System.out.print("."); // NOPMD
						first = false;
					}
					nr = BigInteger.TEN.multiply(r.subtract(n.multiply(t)));
					n = BigInteger.TEN.multiply(THREE.multiply(q).add(r)).divide(t)
							.subtract(BigInteger.TEN.multiply(n));
					q = q.multiply(BigInteger.TEN);
					r = nr;
					System.out.flush();
				} else {
					nr = TWO.multiply(q).add(r).multiply(l);
					nn = q.multiply(SEVEN.multiply(k)).add(TWO).add(r.multiply(l))
							.divide(t.multiply(l));
					q = q.multiply(k);
					t = t.multiply(l);
					l = l.add(TWO);
					k = k.add(BigInteger.ONE);
					n = nn;
					r = nr;
				}
			}
		}
	}
}
