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

import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import net.bull.javamelody.SamplingProfiler.SampledMethod;

import org.junit.Before;
import org.junit.Test;

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
		final SamplingProfiler samplingProfiler = new SamplingProfiler(new ArrayList<String>());
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
