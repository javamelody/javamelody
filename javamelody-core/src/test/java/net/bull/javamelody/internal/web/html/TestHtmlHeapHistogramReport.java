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
package net.bull.javamelody.internal.web.html;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;
import java.util.Collections;
import java.util.List;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import net.bull.javamelody.Utils;
import net.bull.javamelody.internal.common.HttpPart;
import net.bull.javamelody.internal.model.Collector;
import net.bull.javamelody.internal.model.Counter;
import net.bull.javamelody.internal.model.HeapHistogram;
import net.bull.javamelody.internal.model.HeapHistogram.ClassInfo;
import net.bull.javamelody.internal.model.JavaInformations;
import net.bull.javamelody.internal.model.Period;
import net.bull.javamelody.internal.model.VirtualMachine;

/**
 * Test unitaire de la classe HtmlHeapHistogramReport.
 * @author Emeric Vernat
 */
public class TestHtmlHeapHistogramReport {
	private static final String EXCEPTION = "exception";

	/** Initialisation. */
	@Before
	public void setUp() {
		Utils.initialize();
	}

	/** Finalisation.
	 * @throws Exception e */
	@After
	public void tearDown() throws Exception {
		VirtualMachine.detach();
	}

	private static List<JavaInformations> createJavaInformationsList() {
		return Collections.singletonList(new JavaInformations(null, true));
	}

	private static void assertNotEmptyAndClear(final StringWriter writer) {
		assertTrue("rapport vide", writer.getBuffer().length() > 0);
		writer.getBuffer().setLength(0);
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testHeapHistoSun() throws IOException {
		try (InputStream input = getClass().getResourceAsStream("/heaphisto.txt")) {
			final HeapHistogram heapHistogram = new HeapHistogram(input, false);
			report(heapHistogram);
		}
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testHeapHistoJdk9() throws IOException {
		try (InputStream input = getClass().getResourceAsStream("/heaphisto_jdk9.txt")) {
			final HeapHistogram heapHistogram = new HeapHistogram(input, false);
			report(heapHistogram);
		}
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testHeapHistoJdkEa() throws IOException {
		try (InputStream input = getClass().getResourceAsStream("/heaphisto_jdk-ea.txt")) {
			final HeapHistogram heapHistogram = new HeapHistogram(input, false);
			report(heapHistogram);
		}
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testHeapHistoBEA() throws IOException {
		try (InputStream input = getClass().getResourceAsStream("/heaphisto_jrockit.txt")) {
			final HeapHistogram heapHistogram = new HeapHistogram(input, true);
			report(heapHistogram);
		}
	}

	/** Test. */
	@Test
	public void testHeapHistoClassInfoParseLong() {
		assertEquals("parseLongWithK", 100 * 1024, HeapHistogram.ClassInfo.parseLongWithK("100k"));
		assertEquals("parseLongWithK", 100, HeapHistogram.ClassInfo.parseLongWithK("100"));
	}

	private void report(HeapHistogram heapHistogram) throws IOException {
		heapHistogram.add(heapHistogram);
		final StringWriter writer = new StringWriter();
		new HtmlHeapHistogramReport(heapHistogram, writer).toHtml();
		assertNotEmptyAndClear(writer);

		final Counter counter = new Counter("test html report", null);
		final Collector collector = new Collector("test", Collections.singletonList(counter));
		final Period period = Period.TOUT;
		final HtmlReport htmlReport = new HtmlReport(collector, null, createJavaInformationsList(),
				period, writer);
		htmlReport.writeHeapHistogram(heapHistogram, "message", HttpPart.HEAP_HISTO.getName());
		assertNotEmptyAndClear(writer);

		htmlReport.writeHeapHistogram(heapHistogram, null, HttpPart.HEAP_HISTO.getName());
		assertNotEmptyAndClear(writer);

		final ClassInfo classInfo = heapHistogram.getHeapHistogram().get(0);
		classInfo.hashCode();
		assertEquals("classInfo", classInfo, classInfo);
		if (classInfo.equals(new Object())) {
			fail("classInfo");
		}
	}

	/** Test. */
	@Test
	public void testVirtualMachine() {
		// rq : on ne peut pas tester complètement VirtualMachine
		// car eclipse et maven lance junit dans un jre (sans tools.jar) et non dans un jdk
		final boolean supported = VirtualMachine.isSupported();
		final boolean enabled = VirtualMachine.isEnabled();
		if (supported && !enabled) {
			fail("supported && !enabled");
		}
		VirtualMachine.isJRockit();
		// ces méthodes ne peuvent fonctionner dans maven et/ou junit
		final Runnable runnable = new Runnable() {
			@Override
			public void run() {
				virtualMachine();
			}
		};

		final Thread thread = new Thread(runnable, getClass().getSimpleName());
		thread.setDaemon(true);
		thread.start();
		try {
			// timeout de 10s pour certains envionnements de tests
			thread.join(10000);
		} catch (final InterruptedException e) {
			assertNotNull(EXCEPTION, e);
		}
	}

	void virtualMachine() {
		try {
			VirtualMachine.getJvmVirtualMachine();
		} catch (final Exception e) {
			assertNotNull(EXCEPTION, e);
		}
		try {
			VirtualMachine.heapHisto();
		} catch (final Exception e) {
			assertNotNull(EXCEPTION, e);
		}
		try {
			VirtualMachine.createHeapHistogram();
		} catch (final Exception e) {
			assertNotNull(EXCEPTION, e);
		}
		try {
			VirtualMachine.detach();
		} catch (final Exception e) {
			assertNotNull(EXCEPTION, e);
		}
	}
}
