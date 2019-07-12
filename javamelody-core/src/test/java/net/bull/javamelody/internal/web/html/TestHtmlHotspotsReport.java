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

import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import net.bull.javamelody.Utils;
import net.bull.javamelody.internal.model.Collector;
import net.bull.javamelody.internal.model.Counter;
import net.bull.javamelody.internal.model.JavaInformations;
import net.bull.javamelody.internal.model.Period;
import net.bull.javamelody.internal.model.SamplingProfiler;
import net.bull.javamelody.internal.model.SamplingProfiler.SampledMethod;

/**
 * Test unitaire de la classe HtmlHotspotsReport.
 * @author Emeric Vernat
 */
public class TestHtmlHotspotsReport {
	private static final int NB_ROWS = 100;

	/** Initialisation. */
	@Before
	public void setUp() {
		Utils.initialize();
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
	public void test() throws IOException {
		final SamplingProfiler samplingProfiler = new SamplingProfiler(new ArrayList<String>(),
				null);
		final List<SampledMethod> emptyHotspots = samplingProfiler.getHotspots(NB_ROWS);
		samplingProfiler.update();
		final List<SampledMethod> hotspots = samplingProfiler.getHotspots(NB_ROWS);
		final StringWriter writer = new StringWriter();
		new HtmlHotspotsReport(emptyHotspots, writer).toHtml();
		assertNotEmptyAndClear(writer);
		new HtmlHotspotsReport(hotspots, writer).toHtml();
		assertNotEmptyAndClear(writer);

		final Counter counter = new Counter("test html report", null);
		final Collector collector = new Collector("test", Collections.singletonList(counter));
		final Period period = Period.TOUT;
		final HtmlReport htmlReport = new HtmlReport(collector, null, createJavaInformationsList(),
				period, writer);
		htmlReport.writeHotspots(hotspots);
		assertNotEmptyAndClear(writer);
	}
}
