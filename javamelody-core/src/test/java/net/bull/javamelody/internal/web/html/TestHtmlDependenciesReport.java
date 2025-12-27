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
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.easymock.IAnswer;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jakarta.servlet.ServletContext;
import net.bull.javamelody.Utils;
import net.bull.javamelody.internal.common.Parameters;
import net.bull.javamelody.internal.model.MavenArtifact;

/**
 * Test unitaire de la classe HtmlDependenciesReport.
 * @author Emeric Vernat
 */
class TestHtmlDependenciesReport {
	/** Check. */
	@BeforeEach
	void setUp() {
		Utils.initialize();
	}

	private static void assertNotEmptyAndClear(StringWriter writer) {
		assertTrue(writer.getBuffer().length() > 0, "rapport vide");
		writer.getBuffer().setLength(0);
	}

	@Test
	void testEmptyDependencies() throws IOException {
		final Map<String, MavenArtifact> webappDependencies = Collections.emptyMap();
		final StringWriter writer = new StringWriter();
		final HtmlDependenciesReport htmlDependenciesReport = new HtmlDependenciesReport(
				webappDependencies, writer);
		htmlDependenciesReport.toHtml();
		assertNotEmptyAndClear(writer);
	}

	/** Test.
	 * @throws IOException e */
	@Test
	void testDependencies() throws IOException {
		final ServletContext context = createNiceMock(ServletContext.class);
		final String javamelodyDir = "/META-INF/maven/net.bull.javamelody/";
		final String webapp = javamelodyDir + "javamelody-test-webapp/";
		expect(context.getResourcePaths("/META-INF/maven/"))
				.andReturn(Collections.singleton(javamelodyDir)).anyTimes();
		expect(context.getResourcePaths(javamelodyDir)).andReturn(Collections.singleton(webapp))
				.anyTimes();
		final IAnswer<InputStream> answer = () -> getClass().getResourceAsStream("/pom.xml");
		expect(context.getResourceAsStream(webapp + "pom.xml")).andAnswer(answer).anyTimes();
		final Set<String> dependencies = new LinkedHashSet<>(
				List.of("/WEB-INF/lib/jrobin-1.5.9.jar", "/WEB-INF/lib/javamelody-core-1.65.0.jar",
						"/WEB-INF/lib/nothing.jar"));
		expect(context.getResourcePaths("/WEB-INF/lib/")).andReturn(dependencies).anyTimes();
		expect(context.getMajorVersion()).andReturn(5).anyTimes();
		expect(context.getMinorVersion()).andReturn(0).anyTimes();
		replay(context);
		Parameters.initialize(context);
		final Map<String, MavenArtifact> webappDependencies = MavenArtifact.getWebappDependencies();
		assertFalse(webappDependencies.isEmpty(), "getWebappDependencies");
		verify(context);

		final StringWriter writer = new StringWriter();
		final HtmlDependenciesReport htmlDependenciesReport = new HtmlDependenciesReport(
				webappDependencies, writer);
		htmlDependenciesReport.toHtml();
		assertNotEmptyAndClear(writer);
	}
}
