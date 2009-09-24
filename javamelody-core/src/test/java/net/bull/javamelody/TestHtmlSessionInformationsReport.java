/*
 * Copyright 2008-2009 by Emeric Vernat, Bull
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

import net.bull.javamelody.HtmlSessionInformationsReport;
import net.bull.javamelody.SessionInformations;

import org.junit.Test;

/**
 * Test unitaire de la classe HtmlSessionInformationsReport.
 * @author Emeric Vernat
 */
public class TestHtmlSessionInformationsReport {
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
		final StringWriter writer = new StringWriter();
		final HtmlSessionInformationsReport htmlSessionInformationsReport = new HtmlSessionInformationsReport(
				writer);
		htmlSessionInformationsReport.toHtml(Collections.<SessionInformations> emptyList());
		assertNotEmptyAndClear(writer);

		htmlSessionInformationsReport.toHtml(sessions);
		assertNotEmptyAndClear(writer);

		// aucune session sérialisable
		htmlSessionInformationsReport.toHtml(Collections.singletonList(new SessionInformations(
				new SessionTestImpl(false), false)));
		assertNotEmptyAndClear(writer);

		// pays non existant
		final SessionTestImpl sessionPays = new SessionTestImpl(true);
		sessionPays.setCountry("nimporte.quoi");
		htmlSessionInformationsReport.toHtml(Collections.singletonList(new SessionInformations(
				sessionPays, false)));
		assertNotEmptyAndClear(writer);

		htmlSessionInformationsReport.writeSessionDetails("id session", new SessionInformations(
				new SessionTestImpl(true), true));
		htmlSessionInformationsReport.writeSessionDetails("id session", new SessionInformations(
				new SessionTestImpl(false), true));
		assertNotEmptyAndClear(writer);
	}
}
