/*
 * Copyright 2008-2010 by Emeric Vernat
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

import static org.easymock.EasyMock.createNiceMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.io.StringWriter;

import javax.naming.Binding;
import javax.naming.Context;
import javax.naming.NamingEnumeration;
import javax.naming.NamingException;

import org.junit.Before;
import org.junit.Test;

/**
 * Test unitaire de la classe HtmlSessionInformationsReport.
 * @author Emeric Vernat
 */
public class TestHtmlJndiTreeReport {
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
	 * @throws IOException e
	 * @throws NamingException e */
	@Test
	public void testToHtml() throws IOException, NamingException {
		final StringWriter writer = new StringWriter();
		final Context context = createNiceMock(Context.class);
		@SuppressWarnings("unchecked")
		final NamingEnumeration<Binding> enumeration = createNiceMock(NamingEnumeration.class);
		expect(context.listBindings("java:")).andReturn(enumeration).once();
		expect(enumeration.hasMore()).andReturn(true).times(4);
		expect(enumeration.next()).andReturn(new Binding("test value", "test")).once();
		expect(enumeration.next()).andReturn(
				new Binding("test context", createNiceMock(Context.class))).once();
		expect(enumeration.next()).andReturn(new Binding("test null classname", null, null)).once();
		expect(enumeration.next()).andThrow(new NamingException("test")).once();
		final HtmlJndiTreeReport htmlJndiTreeReport = new HtmlJndiTreeReport(context, null, writer);

		replay(context);
		replay(enumeration);
		htmlJndiTreeReport.toHtml();
		verify(context);
		verify(enumeration);
		assertNotEmptyAndClear(writer);

		assertNotNull("path", new HtmlJndiTreeReport(context, "mycontext", writer));
	}
}
