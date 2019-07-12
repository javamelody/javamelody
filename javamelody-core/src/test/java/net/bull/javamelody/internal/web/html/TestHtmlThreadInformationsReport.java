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
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import net.bull.javamelody.Utils;
import net.bull.javamelody.internal.common.Parameters;
import net.bull.javamelody.internal.model.JavaInformations;
import net.bull.javamelody.internal.model.ThreadInformations;

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
