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
package net.bull.javamelody;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.IOException;
import java.net.MalformedURLException;
import java.util.Locale;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jakarta.servlet.FilterConfig;
import jakarta.servlet.ServletContext;
import net.bull.javamelody.internal.common.Parameters;
import net.bull.javamelody.internal.model.Period;
import net.bull.javamelody.internal.model.TransportFormat;

/**
 * Test unitaire de la classe Parameters.
 * @author Emeric Vernat
 */
class TestParameters {
	private static void setProperty(Parameter parameter, String value) {
		Utils.setProperty(parameter, value);
	}

	/** Check. */
	@BeforeEach
	void setUp() {
		Utils.initialize();
	}

	/** Test. */
	@Test
	void testInitializeFilterConfig() {
		// test pas d'erreur
		Parameters.initialize((FilterConfig) null);
	}

	/** Test. */
	@Test
	void testInitializeServletContext() {
		// test pas d'erreur
		Parameters.initialize((ServletContext) null);
	}

	/** Test. */
	@Test
	void testGetHostName() {
		assertNotNull(Parameters.getHostName(), "getHostName");
	}

	/** Test. */
	@Test
	void testGetHostAddress() {
		assertNotNull(Parameters.getHostAddress(), "getHostAddress");
	}

	/** Test. */
	@Test
	void testGetResourcePath() {
		assertNotNull(Parameters.getResourcePath("resource"), "getResourcePath");
	}

	/** Test. */
	@Test
	void testGetResolutionSeconds() {
		if (Parameters.getResolutionSeconds() <= 0) {
			fail("getResolutionSeconds");
		}
		setProperty(Parameter.RESOLUTION_SECONDS, "60");
		if (Parameters.getResolutionSeconds() <= 0) {
			fail("getResolutionSeconds");
		}
		Exception ex = null;
		setProperty(Parameter.RESOLUTION_SECONDS, "-1");
		try {
			Parameters.getResolutionSeconds();
		} catch (final Exception e) {
			ex = e;
		}
		if (ex == null) {
			fail("getResolutionSeconds");
		}
		setProperty(Parameter.RESOLUTION_SECONDS, "60");
	}

	/** Test. */
	@Test
	void testGetStorageDirectory() {
		final String message = "getStorageDirectory";
		final String application = "test";
		assertNotNull(Parameters.getStorageDirectory(application), message);
		setProperty(Parameter.STORAGE_DIRECTORY, "");
		assertNotNull(Parameters.getStorageDirectory(application), message);
		setProperty(Parameter.STORAGE_DIRECTORY, "/");
		assertNotNull(Parameters.getStorageDirectory(application), message);
		if (System.getProperty("os.name").toLowerCase(Locale.getDefault()).contains("windows")) {
			setProperty(Parameter.STORAGE_DIRECTORY, "c:/");
			assertNotNull(Parameters.getStorageDirectory(application), message);
		}
		setProperty(Parameter.STORAGE_DIRECTORY, "javamelody");
	}

	/** Test. */
	@Test
	void testGetCurrentApplication() {
		Parameters.initialize((ServletContext) null);
		// null car pas de servletContext
		assertNull(Parameters.getCurrentApplication(), "getCurrentApplication");
		setProperty(Parameter.APPLICATION_NAME, "test application");
		assertEquals("test application", Parameters.getCurrentApplication(), "getCurrentApplication");
	}

	/** Test. */
	@Test
	void testPeriodValueOfIgnoreCase() {
		assertNotNull(Period.valueOfIgnoreCase(Period.TOUT.toString()), "Period.valueOfIgnoreCase");
	}

	/** Test.
	 * @throws IOException e */
	@Test
	void testGetCollectorUrlsByApplication() throws IOException {
		assertNotNull(Parameters.getCollectorUrlsByApplications(), "getCollectorUrlsByApplications");
	}

	/** Test.
	 * @throws IOException e */
	@Test
	void testAddRemoveCollectorApplication() throws IOException {
		final String application = "testapp";
		Parameters.removeCollectorApplication(application);
		final int size = Parameters.getCollectorUrlsByApplications().size();
		try {
			Parameters.addCollectorApplication(application,
					Parameters.parseUrls("http://localhost:8090/test"));
			assertEquals(size + 1,
					Parameters.getCollectorUrlsByApplications().size(),
					"addCollectorApplication");
			Parameters.removeCollectorApplication(application);
			assertEquals(size,
					Parameters.getCollectorUrlsByApplications().size(),
					"removeCollectorApplication");
			// pour que le test ait une application à lire la prochaine fois
			Parameters.addCollectorApplication(application,
					Parameters.parseUrls("http://localhost:8090/test"));
		} finally {
			Parameters.removeCollectorApplication(application);
		}
	}

	/** Test.
	 * @throws MalformedURLException e */
	@Test
	void testParseUrl() throws MalformedURLException {
		setProperty(Parameter.TRANSPORT_FORMAT, TransportFormat.XML.getCode());
		assertNotNull(Parameters.parseUrls("http://localhost,http://localhost"), "parseUrl");
		assertNotNull(Parameters.parseUrls("http://localhost/"), "parseUrl");
		setProperty(Parameter.TRANSPORT_FORMAT, TransportFormat.SERIALIZED.getCode());
		assertNotNull(Parameters.parseUrls("http://localhost,http://localhost"), "parseUrl");
	}

	/** Test. */
	@Test
	void testIsCounterHidden() {
		setProperty(Parameter.DISPLAYED_COUNTERS, null);
		assertFalse(Parameters.isCounterHidden("http"), "isCounterHidden");
		setProperty(Parameter.DISPLAYED_COUNTERS, "http,sql");
		assertFalse(Parameters.isCounterHidden("http"), "isCounterHidden");
		setProperty(Parameter.DISPLAYED_COUNTERS, "sql");
		assertTrue(Parameters.isCounterHidden("http"), "isCounterHidden");
	}
}
