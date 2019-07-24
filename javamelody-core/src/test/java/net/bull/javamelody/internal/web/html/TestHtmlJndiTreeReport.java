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

import static org.easymock.EasyMock.createNiceMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.io.StringWriter;
import java.util.Arrays;
import java.util.List;

import javax.naming.Binding;
import javax.naming.Context;
import javax.naming.NamingEnumeration;
import javax.naming.NamingException;
import javax.servlet.ServletContext;

import org.junit.Before;
import org.junit.Test;

import net.bull.javamelody.Utils;
import net.bull.javamelody.internal.common.Parameters;
import net.bull.javamelody.internal.model.JndiBinding;

/**
 * Test unitaire de la classe HtmlSessionInformationsReport.
 * @author Emeric Vernat
 */
public class TestHtmlJndiTreeReport {
	private static final String JNDI_PREFIX = "java:";

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
		doToHtmlWithServerName("Mock");
		doToHtmlWithServerName("GlassFish");
		doToHtmlWithServerName("WebLogic");
	}

	private void doToHtmlWithServerName(String serverName) throws NamingException, IOException {
		final ServletContext servletContext = createNiceMock(ServletContext.class);
		expect(servletContext.getServerInfo()).andReturn(serverName).anyTimes();
		replay(servletContext);
		Parameters.initialize(servletContext);
		doToHtml(null);
		verify(servletContext);
	}

	/** Test.
	 * @throws IOException e
	 * @throws NamingException e */
	@Test
	public void testToHtmlWithContextPath() throws IOException, NamingException {
		final ServletContext servletContext = createNiceMock(ServletContext.class);
		expect(servletContext.getServerInfo()).andReturn("Mock").anyTimes();
		replay(servletContext);
		Parameters.initialize(servletContext);
		doToHtml("comp");
		verify(servletContext);
	}

	private void doToHtml(String contextPath) throws NamingException, IOException {
		final StringWriter writer = new StringWriter();
		final Context context = createNiceMock(Context.class);
		final NamingEnumeration<Binding> enumeration = createNiceMock(NamingEnumeration.class);
		if (contextPath == null) {
			expect(context.listBindings(JNDI_PREFIX)).andReturn(enumeration).anyTimes();
			expect(context.listBindings(JNDI_PREFIX + '/')).andReturn(enumeration).anyTimes();
			expect(context.listBindings(JNDI_PREFIX + "comp")).andReturn(enumeration).anyTimes();
			expect(context.listBindings(JNDI_PREFIX + "comp/")).andReturn(enumeration).anyTimes();
		} else {
			expect(context.listBindings(JNDI_PREFIX + contextPath)).andReturn(enumeration)
					.anyTimes();
			expect(context.listBindings(JNDI_PREFIX + contextPath + '/')).andReturn(enumeration)
					.anyTimes();
		}
		expect(enumeration.hasMore()).andReturn(true).times(7);
		expect(enumeration.next()).andReturn(new Binding("test value", "test")).once();
		expect(enumeration.next()).andReturn(new Binding("test value collection",
				Arrays.asList("test collection", "test collection"))).once();
		expect(enumeration.next())
				.andReturn(new Binding("test context", createNiceMock(Context.class))).once();
		expect(enumeration.next()).andReturn(new Binding("", "test")).once();
		expect(enumeration.next())
				.andReturn(new Binding("java:/test context", createNiceMock(Context.class))).once();
		expect(enumeration.next()).andReturn(new Binding("test null classname", null, null)).once();
		expect(enumeration.next()).andThrow(new NamingException("test")).once();

		replay(context);
		replay(enumeration);
		final List<JndiBinding> bindings = JndiBinding.listBindings(context, contextPath);
		for (final JndiBinding binding : bindings) {
			binding.toString();
		}
		final HtmlJndiTreeReport htmlJndiTreeReport = new HtmlJndiTreeReport(bindings, contextPath,
				writer);
		htmlJndiTreeReport.toHtml();
		verify(context);
		verify(enumeration);
		assertNotEmptyAndClear(writer);
	}
}
