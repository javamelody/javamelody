/*
 * Copyright 2008-2014 by Emeric Vernat
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
package net.bull.javamelody;

import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

/**
 * Test unitaire de la classe HtmlSessionInformationsReport.
 * @author Emeric Vernat
 */
public class TestHtmlSessionInformationsReport {
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
	public void testSessionsInformations() throws IOException {
		final List<SessionInformations> sessions = new ArrayList<SessionInformations>();
		sessions.add(new SessionInformations(new SessionTestImpl(true), false));
		sessions.add(new SessionInformations(new SessionTestImpl(false), false));
		final SessionTestImpl serializableButNotSession = new SessionTestImpl(true);
		serializableButNotSession.setAttribute("serializable but not",
				Collections.singleton(new Object()));
		sessions.add(new SessionInformations(serializableButNotSession, false));
		final StringWriter writer = new StringWriter();
		new HtmlSessionInformationsReport(Collections.<SessionInformations> emptyList(), null,
				writer).toHtml();
		assertNotEmptyAndClear(writer);

		new HtmlSessionInformationsReport(sessions, null, writer).toHtml();
		assertNotEmptyAndClear(writer);

		// aucune session sérialisable
		new HtmlSessionInformationsReport(Collections.singletonList(new SessionInformations(
				new SessionTestImpl(false), false)), null, writer).toHtml();
		assertNotEmptyAndClear(writer);

		// pays non existant
		final SessionTestImpl sessionPays = new SessionTestImpl(true);
		sessionPays.setCountry("nimporte.quoi");
		new HtmlSessionInformationsReport(Collections.singletonList(new SessionInformations(
				sessionPays, false)), null, writer).toHtml();
		assertNotEmptyAndClear(writer);

		// pays null
		sessionPays.setCountry(null);
		assertNull("countryDisplay null",
				new SessionInformations(sessionPays, false).getCountryDisplay());
		new HtmlSessionInformationsReport(Collections.singletonList(new SessionInformations(
				sessionPays, false)), null, writer).toHtml();
		assertNotEmptyAndClear(writer);

		new HtmlSessionInformationsReport(null, null, writer).writeSessionDetails("id session",
				new SessionInformations(new SessionTestImpl(true), true));
		new HtmlSessionInformationsReport(null, null, writer).writeSessionDetails("id session",
				new SessionInformations(new SessionTestImpl(false), true));
		assertNotEmptyAndClear(writer);
	}
}
