/*
 * Copyright 2008-2026 by Emeric Vernat
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
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.jar.JarOutputStream;
import java.util.jar.Manifest;
import java.util.zip.ZipEntry;

import org.easymock.IAnswer;
import org.jrobin.graph.RrdGraph;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import jakarta.servlet.ServletContext;
import net.bull.javamelody.Parameter;
import net.bull.javamelody.Utils;
import net.bull.javamelody.internal.common.Parameters;

/**
 * Test for MavenArtifact.
 * @author Emeric Vernat
 */
class TestMavenArtifact {

	private static final String MAVEN_CENTRAL = "https://repo1.maven.org/maven2";

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
	@BeforeEach
	void setUp() {
		Utils.initialize();
	}

	/**
	 * Test.
	 * @throws ClassNotFoundException e
	 * @throws IOException e
	 */
	@Test
	void testGetSourceJarFile() throws ClassNotFoundException, IOException {
		final File storageDirectory = Parameters
				.getStorageDirectory(Parameters.getCurrentApplication());
		rmdir(new File(storageDirectory, "poms"));
		rmdir(new File(storageDirectory, "sources"));

		final Class<?> clazz = Class.forName("org.apache.commons.dbcp2.BasicDataSource");
		final URL location = clazz.getProtectionDomain().getCodeSource().getLocation();
		assertNotNull(MavenArtifact.getSourceJarFile(location), "getSourceJarFile");
		Utils.setProperty(Parameter.MAVEN_REPOSITORIES, LOCAL_REPO.getPath() + ',' + MAVEN_CENTRAL);
		assertNotNull(MavenArtifact.getSourceJarFile(location), "getSourceJarFile");
	}

	/**
	 * Test.
	 * @throws IOException e
	 */
	@Test
	void testGetWebappDependencies() throws IOException {
		final ServletContext context = createNiceMock(ServletContext.class);
		final String javamelodyDir = "/META-INF/maven/net.bull.javamelody/";
		final String webapp = javamelodyDir + "javamelody-test-webapp/";
		expect(context.getResourcePaths("/META-INF/maven/"))
				.andReturn(Collections.singleton(javamelodyDir)).anyTimes();
		expect(context.getResourcePaths(javamelodyDir)).andReturn(Collections.singleton(webapp))
				.anyTimes();
		final IAnswer<InputStream> answer = () -> getClass().getResourceAsStream("/pom.xml");
		expect(context.getResourceAsStream(webapp + "pom.xml")).andAnswer(answer).anyTimes();
		final Set<String> dependencies = new LinkedHashSet<>(List
				.of("/WEB-INF/lib/jrobin-1.5.9.jar", "/WEB-INF/lib/javamelody-core-1.65.0.jar"));
		expect(context.getResourcePaths("/WEB-INF/lib/")).andReturn(dependencies).anyTimes();
		final URL jrobinJar = RrdGraph.class.getProtectionDomain().getCodeSource().getLocation();
		expect(context.getResource("/WEB-INF/lib/jrobin-1.5.9.jar")).andReturn(jrobinJar)
				.anyTimes();
		expect(context.getMajorVersion()).andReturn(5).anyTimes();
		expect(context.getMinorVersion()).andReturn(0).anyTimes();
		replay(context);
		Parameters.initialize(context);
		final Map<String, MavenArtifact> webappDependencies = MavenArtifact.getWebappDependencies();
		assertFalse(webappDependencies.isEmpty(), "getWebappDependencies");
		verify(context);
		for (final MavenArtifact dependency : webappDependencies.values()) {
			if (dependency != null) {
				assertNotNull(dependency.getGroupId(), "groupId");
				assertNotNull(dependency.getArtifactId(), "artifactId");
				assertNotNull(dependency.getVersion(), "version");
				if ("jrobin".equals(dependency.getArtifactId())) {
					assertNotNull(dependency.getName(), "name");
					assertNotNull(dependency.getUrl(), "url");
					assertNotNull(dependency.getLicenseUrlsByName(), "licenseUrlsByName");
					assertNotNull(dependency.getAllDependencies(), "allDependencies");
					assertNotNull(dependency.toString(), "toString");
				}
			}
		}
	}

	/**
	 * Test for artifacts without embedded META-INF/maven/.../pom.xml (for example Spring
	 * Framework/Boot since their move to Gradle, or the PostgreSQL JDBC driver), whose version
	 * must be read from META-INF/MANIFEST.MF instead.
	 * @param tempDir dossier temporaire fourni et nettoyé par JUnit
	 * @throws IOException e
	 */
	@Test
	void testGetWebappDependenciesFromManifest(@TempDir File tempDir) throws IOException {
		final String jarFileName = "no-pom-example-1.2.3.jar";
		final File jarFile = createJarWithManifestOnly(tempDir, jarFileName, "1.2.3", "org.example",
				"No Pom Example");

		final ServletContext context = createNiceMock(ServletContext.class);
		final Set<String> dependencies = Collections.singleton("/WEB-INF/lib/" + jarFileName);
		expect(context.getResourcePaths("/WEB-INF/lib/")).andReturn(dependencies).anyTimes();
		expect(context.getResource("/WEB-INF/lib/" + jarFileName)).andReturn(jarFile.toURI().toURL())
		                                                           .anyTimes();
		expect(context.getMajorVersion()).andReturn(5).anyTimes();
		expect(context.getMinorVersion()).andReturn(0).anyTimes();
		replay(context);
		Parameters.initialize(context);
		final Map<String, MavenArtifact> webappDependencies = MavenArtifact.getWebappDependencies();
		verify(context);

		MavenArtifact dependency = webappDependencies.get(jarFileName);
		assertNotNull(dependency, "dependency resolved from manifest");
		assertEquals("1.2.3", dependency.getVersion(), "version");
		assertEquals("no-pom-example", dependency.getArtifactId(), "artifactId");
		assertEquals("org.example", dependency.getGroupId(), "groupId");
	}

	private static File createJarWithManifestOnly(File directory, String fileName, String version,
			String vendorId, String title) throws IOException {
		final File jarFile = new File(directory, fileName);
		final Manifest manifest = new Manifest();
		manifest.getMainAttributes().putValue("Manifest-Version", "1.0");
		manifest.getMainAttributes().putValue("Implementation-Version", version);
		manifest.getMainAttributes().putValue("Implementation-Vendor-Id", vendorId);
		manifest.getMainAttributes().putValue("Implementation-Title", title);
		try (JarOutputStream jarOutputStream = new JarOutputStream(new FileOutputStream(jarFile),
				manifest)) {
			jarOutputStream.putNextEntry(new ZipEntry("README.txt"));
			jarOutputStream.write(("This jar is only a test fixture: it intentionally has no files, in order to test "
								   + "the fallback of MavenArtifact to META-INF/MANIFEST.MF.").getBytes(StandardCharsets.UTF_8));
			jarOutputStream.closeEntry();
		}
		return jarFile;
	}

}
