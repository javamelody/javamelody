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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.net.MalformedURLException;

import javax.servlet.FilterConfig;
import javax.servlet.ServletContext;

import org.junit.Test;

/**
 * Test unitaire de la classe Parameters.
 * @author Emeric Vernat
 */
public class TestParameters {
	private static void setProperty(Parameter parameter, String value) {
		System.setProperty(Parameters.PARAMETER_SYSTEM_PREFIX + parameter.getCode(), value);
	}

	/** Test. */
	@Test
	public void testInitializeFilterConfig() {
		// test pas d'erreur
		Parameters.initialize((FilterConfig) null);
		assertNotNull("initialize", Parameters.class);
	}

	/** Test. */
	@Test
	public void testInitializeServletContext() {
		// test pas d'erreur
		Parameters.initialize((ServletContext) null);
		assertNotNull("initialize", Parameters.class);
	}

	/** Test. */
	@Test
	public void testGetHostName() {
		assertNotNull("getHostName", Parameters.getHostName());
	}

	/** Test. */
	@Test
	public void testGetHostAddress() {
		assertNotNull("getHostAddress", Parameters.getHostAddress());
	}

	/** Test. */
	@Test
	public void testGetResourcePath() {
		assertNotNull("getResourcePath", Parameters.getResourcePath("resource"));
	}

	/** Test. */
	@Test
	public void testGetResolutionSeconds() {
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
	public void testGetStorageDirectory() {
		assertNotNull("getStorageDirectory", Parameters.getStorageDirectory("test"));
		setProperty(Parameter.STORAGE_DIRECTORY, "");
		assertNotNull("getStorageDirectory", Parameters.getStorageDirectory("test"));
		setProperty(Parameter.STORAGE_DIRECTORY, "/");
		assertNotNull("getStorageDirectory", Parameters.getStorageDirectory("test"));
		setProperty(Parameter.STORAGE_DIRECTORY, "javamelody");
	}

	/** Test. */
	@Test
	public void testGetCurrentApplication() {
		// null car pas de servletContext
		assertNull("getCurrentApplication", Parameters.getCurrentApplication());
	}

	/** Test. */
	@Test
	public void testGetParameter() {
		assertNull("getParameter", Parameters.getParameter(Parameter.DATASOURCES));
	}

	/** Test. */
	@Test
	public void testParameterValueOfIgnoreCase() {
		assertNotNull("Parameter.valueOfIgnoreCase", Parameter.valueOfIgnoreCase(Parameter.DISABLED
				.toString()));
	}

	/** Test. */
	@Test
	public void testPeriodValueOfIgnoreCase() {
		assertNotNull("Period.valueOfIgnoreCase", Period.valueOfIgnoreCase(Period.TOUT.toString()));
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testGetCollectorUrlsByApplication() throws IOException {
		assertNotNull("getCollectorUrlsByApplications", Parameters.getCollectorUrlsByApplications());
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testAddRemoveCollectorApplication() throws IOException {
		final String application = "testapp";
		Parameters.removeCollectorApplication(application);
		final int size = Parameters.getCollectorUrlsByApplications().size();
		Parameters.addCollectorApplication(application, Parameters
				.parseUrl("http://localhost:8090/test"));
		assertEquals("addCollectorApplication", size + 1, Parameters
				.getCollectorUrlsByApplications().size());
		Parameters.removeCollectorApplication(application);
		assertEquals("removeCollectorApplication", size, Parameters
				.getCollectorUrlsByApplications().size());
		// pour que le test ait une application à lire la prochaine fois
		Parameters.addCollectorApplication(application, Parameters
				.parseUrl("http://localhost:8090/test"));
	}

	/** Test.
	 * @throws MalformedURLException e */
	@Test
	public void testParseUrl() throws MalformedURLException {
		setProperty(Parameter.TRANSPORT_FORMAT, TransportFormat.XML.getCode());
		assertNotNull("parseUrl", Parameters.parseUrl("http://localhost,http://localhost"));
		setProperty(Parameter.TRANSPORT_FORMAT, TransportFormat.SERIALIZED.getCode());
		assertNotNull("parseUrl", Parameters.parseUrl("http://localhost,http://localhost"));
	}
}
