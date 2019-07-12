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
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import net.bull.javamelody.SessionListener;
import net.bull.javamelody.SessionTestImpl;
import net.bull.javamelody.Utils;
import net.bull.javamelody.internal.model.SessionInformations;

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
		final SessionTestImpl mySession = new SessionTestImpl("myId", true,
				System.currentTimeMillis());
		sessions.add(new SessionInformations(mySession, false));
		final StringWriter writer = new StringWriter();
		new HtmlSessionInformationsReport(Collections.<SessionInformations> emptyList(), writer)
				.toHtml();
		assertNotEmptyAndClear(writer);

		try {
			SessionListener.bindSession(mySession);
			new HtmlSessionInformationsReport(sessions, writer).toHtml();
			assertNotEmptyAndClear(writer);
		} finally {
			SessionListener.unbindSession();
		}

		// aucune session sérialisable
		new HtmlSessionInformationsReport(Collections
				.singletonList(new SessionInformations(new SessionTestImpl(false), false)), writer)
						.toHtml();
		assertNotEmptyAndClear(writer);

		// pays non existant
		final SessionTestImpl sessionPays = new SessionTestImpl(true);
		sessionPays.setCountry("nimporte.quoi");
		new HtmlSessionInformationsReport(
				Collections.singletonList(new SessionInformations(sessionPays, false)), writer)
						.toHtml();
		assertNotEmptyAndClear(writer);

		// pays null
		sessionPays.setCountry(null);
		assertNull("countryDisplay null",
				new SessionInformations(sessionPays, false).getCountryDisplay());
		new HtmlSessionInformationsReport(
				Collections.singletonList(new SessionInformations(sessionPays, false)), writer)
						.toHtml();
		assertNotEmptyAndClear(writer);

		// userAgent windows
		final SessionTestImpl sessionUserAgent = new SessionTestImpl(true);
		sessionUserAgent.setUserAgent(
				"Mozilla/5.0 (Windows NT 6.1; WOW64; Trident/7.0; AS; rv:11.0) like Gecko");
		assertEquals("userAgent windows MSIE", "MSIE 11.0",
				new SessionInformations(sessionUserAgent, false).getBrowser());
		assertEquals("userAgent windows MSIE", "Windows 7",
				new SessionInformations(sessionUserAgent, false).getOs());

		// userAgent Android
		sessionUserAgent.setUserAgent(
				"Mozilla/5.0 (Linux; Android 4.0.4; Galaxy Nexus Build/IMM76B) AppleWebKit/535.19 (KHTML, like Gecko) Chrome/18.0.1025.133 Mobile Safari/535.19");
		assertEquals("userAgent android chrome", "Chrome/18.0.1025.133",
				new SessionInformations(sessionUserAgent, false).getBrowser());
		assertEquals("userAgent android chrome", "Android 4.0.4",
				new SessionInformations(sessionUserAgent, false).getOs());

		// userAgent Mac OS
		sessionUserAgent.setUserAgent(
				"Mozilla/5.0 (iPad; CPU OS 6_0 like Mac OS X) AppleWebKit/536.26 (KHTML, like Gecko) Version/6.0 Mobile/10A5355d Safari/8536.25");
		assertEquals("userAgent mac OS safari", "Safari/8536.25",
				new SessionInformations(sessionUserAgent, false).getBrowser());
		assertEquals("userAgent mac OS safari", "CPU OS 6_0 like Mac OS X",
				new SessionInformations(sessionUserAgent, false).getOs());

		// userAgent inconnu
		sessionUserAgent.setUserAgent("n'importe quoi");
		assertNull("userAgent n'importe quoi",
				new SessionInformations(sessionUserAgent, false).getBrowser());
		assertNull("userAgent n'importe quoi",
				new SessionInformations(sessionUserAgent, false).getOs());
		new HtmlSessionInformationsReport(
				Collections.singletonList(new SessionInformations(sessionUserAgent, false)), writer)
						.toHtml();
		assertNotEmptyAndClear(writer);

		// userAgent null
		sessionUserAgent.setUserAgent(null);
		assertNull("userAgent null", new SessionInformations(sessionUserAgent, false).getBrowser());
		assertNull("userAgent null", new SessionInformations(sessionUserAgent, false).getOs());
		new HtmlSessionInformationsReport(
				Collections.singletonList(new SessionInformations(sessionUserAgent, false)), writer)
						.toHtml();
		assertNotEmptyAndClear(writer);

		new HtmlSessionInformationsReport(null, writer).writeSessionDetails("id session",
				new SessionInformations(new SessionTestImpl(true), true));
		new HtmlSessionInformationsReport(null, writer).writeSessionDetails("id session",
				new SessionInformations(new SessionTestImpl(false), true));
		assertNotEmptyAndClear(writer);
	}
}
