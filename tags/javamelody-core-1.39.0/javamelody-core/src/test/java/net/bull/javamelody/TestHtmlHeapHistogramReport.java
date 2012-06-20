/*
 * Copyright 2008-2012 by Emeric Vernat
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

import static net.bull.javamelody.HttpParameters.HEAP_HISTO_PART;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;
import java.util.Collections;
import java.util.List;

import net.bull.javamelody.HeapHistogram.ClassInfo;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

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
		final InputStream input = getClass().getResourceAsStream("/heaphisto.txt");
		try {
			final HeapHistogram heapHistogram = new HeapHistogram(input, false);
			report(heapHistogram);
		} finally {
			input.close();
		}
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testHeapHistoBEA() throws IOException {
		final InputStream input = getClass().getResourceAsStream("/heaphisto_jrockit.txt");
		try {
			final HeapHistogram heapHistogram = new HeapHistogram(input, true);
			report(heapHistogram);
		} finally {
			input.close();
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
		htmlReport.writeHeapHistogram(heapHistogram, "message", HEAP_HISTO_PART);
		assertNotEmptyAndClear(writer);

		htmlReport.writeHeapHistogram(heapHistogram, null, HEAP_HISTO_PART);
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
