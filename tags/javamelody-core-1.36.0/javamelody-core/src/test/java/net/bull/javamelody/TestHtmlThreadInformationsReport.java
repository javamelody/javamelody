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
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

/**
 * Test unitaire de la classe HtmlSessionInformationsReport.
 * @author Emeric Vernat
 */
public class TestHtmlThreadInformationsReport {
	/** Check. */
	@Before
	public void setUp() {
		Utils.initialize();
	}

	private static void assertNotEmptyAndClear(StringWriter writer) {
		assertTrue("rapport vide", writer.getBuffer().length() > 0);
		writer.getBuffer().setLength(0);
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testThreadInformations() throws IOException {
		final StringWriter writer = new StringWriter();
		new HtmlThreadInformationsReport(Collections.<ThreadInformations> emptyList(), true, writer)
				.toHtml();
		assertNotEmptyAndClear(writer);
		new HtmlThreadInformationsReport(JavaInformations.buildThreadInformationsList(), true,
				writer).toHtml();
		assertNotEmptyAndClear(writer);
		new HtmlThreadInformationsReport(JavaInformations.buildThreadInformationsList(), false,
				writer).toHtml();
		assertNotEmptyAndClear(writer);

		final List<ThreadInformations> threads = new ArrayList<ThreadInformations>();
		final Thread thread = Thread.currentThread();
		final List<StackTraceElement> stackTrace = Arrays.asList(thread.getStackTrace());
		final String hostAddress = Parameters.getHostAddress();
		threads.add(new ThreadInformations(thread, null, 10, 10, false, hostAddress));
		threads.add(new ThreadInformations(thread, Collections.<StackTraceElement> emptyList(), 10,
				10, false, hostAddress));
		threads.add(new ThreadInformations(thread, stackTrace, 10, 10, true, hostAddress));
		threads.add(new ThreadInformations(thread, stackTrace, 10, 10, false, hostAddress));
		new HtmlThreadInformationsReport(threads, true, writer).toHtml();
		assertNotEmptyAndClear(writer);

		new HtmlThreadInformationsReport(threads, true, writer).writeDeadlocks();
		assertNotEmptyAndClear(writer);
	}
}
