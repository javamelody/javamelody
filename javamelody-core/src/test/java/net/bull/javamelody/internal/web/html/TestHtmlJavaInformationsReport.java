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

import org.junit.Before;
import org.junit.Test;

import net.bull.javamelody.Utils;
import net.bull.javamelody.internal.model.JavaInformations;
import net.bull.javamelody.internal.model.MBeans;
import net.bull.javamelody.internal.model.TestTomcatInformations.GlobalRequestProcessor;
import net.bull.javamelody.internal.model.TestTomcatInformations.ThreadPool;
import net.bull.javamelody.internal.model.TomcatInformations;

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
			mBeans.add(mBeanServer
					.registerMBean(new ThreadPool(),
							new ObjectName("Catalina:type=ThreadPool,name=jk-8009"))
					.getObjectName());
			mBeans.add(
					mBeanServer
							.registerMBean(new GlobalRequestProcessor(),
									new ObjectName(
											"Catalina:type=GlobalRequestProcessor,name=jk-8009"))
							.getObjectName());
			TomcatInformations.initMBeans();
			final List<JavaInformations> myJavaInformationsList = Arrays
					.asList(new JavaInformations(null, true));
			final HtmlJavaInformationsReport htmlReport = new HtmlJavaInformationsReport(
					myJavaInformationsList, writer);
			htmlReport.toHtml();
			assertNotEmptyAndClear(writer);

			mBeans.add(mBeanServer
					.registerMBean(new ThreadPool(),
							new ObjectName("Catalina:type=ThreadPool,name=jk-8010"))
					.getObjectName());
			final GlobalRequestProcessor jk8010 = new GlobalRequestProcessor();
			jk8010.setrequestCount(0);
			mBeans.add(
					mBeanServer
							.registerMBean(jk8010,
									new ObjectName(
											"Catalina:type=GlobalRequestProcessor,name=jk-8010"))
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

	/** Test. */
	@Test
	public void testToBar() {
		assertNotNull("toBar", HtmlJavaInformationsReport.toBar(0));
		assertNotNull("toBar", HtmlJavaInformationsReport.toBar(1));
		assertNotNull("toBar", HtmlJavaInformationsReport.toBar(10));
		assertNotNull("toBar", HtmlJavaInformationsReport.toBar(15));
		assertNotNull("toBarWithAlert", HtmlJavaInformationsReport.toBarWithAlert(10, "detail"));
		assertNotNull("toBarWithAlert", HtmlJavaInformationsReport.toBarWithAlert(100, "detail"));
		assertNotNull("toBarWithAlert", HtmlJavaInformationsReport.toBarWithAlert(100, null));
	}

	private static void assertNotEmptyAndClear(final StringWriter writer) {
		assertTrue("rapport vide", writer.getBuffer().length() > 0);
		writer.getBuffer().setLength(0);
	}
}
