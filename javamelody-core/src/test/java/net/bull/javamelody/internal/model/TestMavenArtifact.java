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
package net.bull.javamelody.internal.model;

import static org.easymock.EasyMock.createNiceMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

import javax.servlet.ServletContext;

import org.easymock.IAnswer;
import org.jrobin.graph.RrdGraph;
import org.junit.Before;
import org.junit.Test;

import net.bull.javamelody.Parameter;
import net.bull.javamelody.Utils;
import net.bull.javamelody.internal.common.Parameters;

/**
 * Test for MavenArtifact.
 * @author Emeric Vernat
 */
public class TestMavenArtifact {
	private static final String MAVEN_CENTRAL = "http://repo1.maven.org/maven2";

	private static final File LOCAL_REPO = new File(
			System.getProperty("user.home") + "/.m2/repository");

	private static void rmdir(final File file) {
		final File[] files = file.listFiles();
		if (files != null) {
			for (final File f : files) {
				f.delete();
			}
		}
	}

	/** Check. */
	@Before
	public void setUp() {
		Utils.initialize();
	}

	/**
	 * Test.
	 * @throws ClassNotFoundException e
	 * @throws IOException e
	 */
	@Test
	public void testGetSourceJarFile() throws ClassNotFoundException, IOException {
		final File storageDirectory = Parameters
				.getStorageDirectory(Parameters.getCurrentApplication());
		rmdir(new File(storageDirectory, "poms"));
		rmdir(new File(storageDirectory, "sources"));

		final Class<?> clazz = Class.forName("org.apache.commons.dbcp2.BasicDataSource");
		final URL location = clazz.getProtectionDomain().getCodeSource().getLocation();
		assertNotNull("getSourceJarFile", MavenArtifact.getSourceJarFile(location));
		Utils.setProperty(Parameter.MAVEN_REPOSITORIES, LOCAL_REPO.getPath() + ',' + MAVEN_CENTRAL);
		assertNotNull("getSourceJarFile", MavenArtifact.getSourceJarFile(location));
	}

	/**
	 * Test.
	 * @throws IOException e
	 */
	@Test
	public void testGetWebappDependencies() throws IOException {
		final ServletContext context = createNiceMock(ServletContext.class);
		final String javamelodyDir = "/META-INF/maven/net.bull.javamelody/";
		final String webapp = javamelodyDir + "javamelody-test-webapp/";
		expect(context.getResourcePaths("/META-INF/maven/"))
				.andReturn(Collections.singleton(javamelodyDir)).anyTimes();
		expect(context.getResourcePaths(javamelodyDir)).andReturn(Collections.singleton(webapp))
				.anyTimes();
		final IAnswer<InputStream> answer = new IAnswer<InputStream>() {
			@Override
			public InputStream answer() throws Throwable {
				return getClass().getResourceAsStream("/pom.xml");
			}
		};
		expect(context.getResourceAsStream(webapp + "pom.xml")).andAnswer(answer).anyTimes();
		final Set<String> dependencies = new LinkedHashSet<String>(Arrays.asList(
				"/WEB-INF/lib/jrobin-1.5.9.jar", "/WEB-INF/lib/javamelody-core-1.65.0.jar"));
		expect(context.getResourcePaths("/WEB-INF/lib/")).andReturn(dependencies).anyTimes();
		final URL jrobinJar = RrdGraph.class.getProtectionDomain().getCodeSource().getLocation();
		expect(context.getResource("/WEB-INF/lib/jrobin-1.5.9.jar")).andReturn(jrobinJar)
				.anyTimes();
		expect(context.getMajorVersion()).andReturn(2).anyTimes();
		expect(context.getMinorVersion()).andReturn(5).anyTimes();
		replay(context);
		Parameters.initialize(context);
		final Map<String, MavenArtifact> webappDependencies = MavenArtifact.getWebappDependencies();
		assertFalse("getWebappDependencies", webappDependencies.isEmpty());
		verify(context);
		for (final MavenArtifact dependency : webappDependencies.values()) {
			if (dependency != null) {
				assertNotNull("groupId", dependency.getGroupId());
				assertNotNull("artifactId", dependency.getArtifactId());
				assertNotNull("version", dependency.getVersion());
				assertNotNull("name", dependency.getName());
				assertNotNull("url", dependency.getUrl());
				assertNotNull("licenseUrlsByName", dependency.getLicenseUrlsByName());
				assertNotNull("allDependencies", dependency.getAllDependencies());
				assertNotNull("toString", dependency.toString());
			}
		}
	}
}
