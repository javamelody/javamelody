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
package net.bull.javamelody; // NOPMD

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.management.JMException;
import javax.management.MBeanServer;
import javax.management.ObjectName;

import net.bull.javamelody.TestTomcatInformations.GlobalRequestProcessor;
import net.bull.javamelody.TestTomcatInformations.ThreadPool;

import org.junit.Before;
import org.junit.Test;

/**
 * Test unitaire de la classe HtmlJavaInformationsReport.
 * @author Emeric Vernat
 */
public class TestHtmlJavaInformationsReport {
	private StringWriter writer;

	/** Initialisation. */
	@Before
	public void setUp() {
		Utils.initialize();
		// pour testTomcatInformations (si la classe TomcatInformations est déjà chargée,
		// c'est trop tard, et c'est pourquoi cela doit être défini au lancement de junit)
		System.setProperty("catalina.home", "unknown");
		writer = new StringWriter();
	}

	/** Test. */
	@Test
	public void testGetApplicationServerIconName() {
		assertNotNull("getApplicationServerIconName",
				HtmlJavaInformationsReport.getApplicationServerIconName("Tomcat"));
		assertNull("getApplicationServerIconName",
				HtmlJavaInformationsReport.getApplicationServerIconName("unknown"));
	}

	/** Test. */
	@Test
	public void testGetOSIconName() {
		assertNotNull("getOSIconName", HtmlJavaInformationsReport.getOSIconName("Linux"));
		assertNull("getOSIconName", HtmlJavaInformationsReport.getOSIconName("unknown"));
	}

	/** Test.
	 * @throws IOException e
	 * @throws JMException e */
	@Test
	public void testTomcatInformations() throws IOException, JMException {
		final MBeanServer mBeanServer = MBeans.getPlatformMBeanServer();
		final List<ObjectName> mBeans = new ArrayList<ObjectName>();
		try {
			mBeans.add(mBeanServer.registerMBean(new ThreadPool(),
					new ObjectName("Catalina:type=ThreadPool,name=jk-8009")).getObjectName());
			mBeans.add(mBeanServer.registerMBean(new GlobalRequestProcessor(),
					new ObjectName("Catalina:type=GlobalRequestProcessor,name=jk-8009"))
					.getObjectName());
			TomcatInformations.initMBeans();
			final List<JavaInformations> myJavaInformationsList = Arrays
					.asList(new JavaInformations(null, true));
			final HtmlJavaInformationsReport htmlReport = new HtmlJavaInformationsReport(
					myJavaInformationsList, writer);
			htmlReport.toHtml();
			assertNotEmptyAndClear(writer);

			mBeans.add(mBeanServer.registerMBean(new ThreadPool(),
					new ObjectName("Catalina:type=ThreadPool,name=jk-8010")).getObjectName());
			final GlobalRequestProcessor jk8010 = new GlobalRequestProcessor();
			jk8010.setrequestCount(0);
			mBeans.add(mBeanServer.registerMBean(jk8010,
					new ObjectName("Catalina:type=GlobalRequestProcessor,name=jk-8010"))
					.getObjectName());
			TomcatInformations.initMBeans();
			final List<JavaInformations> myJavaInformationsList2 = Arrays
					.asList(new JavaInformations(null, true));
			final HtmlJavaInformationsReport htmlReport2 = new HtmlJavaInformationsReport(
					myJavaInformationsList2, writer);
			htmlReport2.toHtml();
			assertNotEmptyAndClear(writer);

			jk8010.setrequestCount(1000);
			final List<JavaInformations> myJavaInformationsList3 = Arrays
					.asList(new JavaInformations(null, true));
			final HtmlJavaInformationsReport htmlReport3 = new HtmlJavaInformationsReport(
					myJavaInformationsList3, writer);
			htmlReport3.toHtml();
			assertNotEmptyAndClear(writer);
		} finally {
			for (final ObjectName registeredMBean : mBeans) {
				mBeanServer.unregisterMBean(registeredMBean);
			}
			TomcatInformations.initMBeans();
		}
	}

	private static void assertNotEmptyAndClear(final StringWriter writer) {
		assertTrue("rapport vide", writer.getBuffer().length() > 0);
		writer.getBuffer().setLength(0);
	}
}
