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

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;

import javax.management.JMException;
import javax.management.MBeanServer;
import javax.management.ObjectName;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

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
class TestHtmlJavaInformationsReport {
	private StringWriter writer;

	/** Initialisation. */
	@BeforeEach
	void setUp() {
		Utils.initialize();
		// pour testTomcatInformations (si la classe TomcatInformations est déjà chargée,
		// c'est trop tard, et c'est pourquoi cela doit être défini au lancement de junit)
		System.setProperty("catalina.home", "unknown");
		writer = new StringWriter();
	}

	/** Test. */
	@Test
	void testGetApplicationServerIconName() {
		assertNotNull(HtmlJavaInformationsReport.getApplicationServerIconName("Tomcat"),
				"getApplicationServerIconName");
		assertNull(HtmlJavaInformationsReport.getApplicationServerIconName("unknown"),
				"getApplicationServerIconName");
	}

	/** Test. */
	@Test
	void testGetOSIconName() {
		assertNotNull(HtmlJavaInformationsReport.getOSIconName("Linux"), "getOSIconName");
		assertNull(HtmlJavaInformationsReport.getOSIconName("unknown"), "getOSIconName");
	}

	/** Test.
	 * @throws IOException e
	 * @throws JMException e */
	@Test
	void testTomcatInformations() throws IOException, JMException {
		final MBeanServer mBeanServer = MBeans.getPlatformMBeanServer();
		final List<ObjectName> mBeans = new ArrayList<>();
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
			final List<JavaInformations> myJavaInformationsList = List
					.of(new JavaInformations(null, true));
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
			final List<JavaInformations> myJavaInformationsList2 = List
					.of(new JavaInformations(null, true));
			final HtmlJavaInformationsReport htmlReport2 = new HtmlJavaInformationsReport(
					myJavaInformationsList2, writer);
			htmlReport2.toHtml();
			assertNotEmptyAndClear(writer);

			jk8010.setrequestCount(1000);
			final List<JavaInformations> myJavaInformationsList3 = List
					.of(new JavaInformations(null, true));
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
	void testToBar() {
		assertNotNull(HtmlJavaInformationsReport.toBar(0), "toBar");
		assertNotNull(HtmlJavaInformationsReport.toBar(1), "toBar");
		assertNotNull(HtmlJavaInformationsReport.toBar(10), "toBar");
		assertNotNull(HtmlJavaInformationsReport.toBar(15), "toBar");
		assertNotNull(HtmlJavaInformationsReport.toBarWithAlert(10, "detail"), "toBarWithAlert");
		assertNotNull(HtmlJavaInformationsReport.toBarWithAlert(100, "detail"), "toBarWithAlert");
		assertNotNull(HtmlJavaInformationsReport.toBarWithAlert(100, null), "toBarWithAlert");
	}

	private static void assertNotEmptyAndClear(final StringWriter writer) {
		assertTrue(writer.getBuffer().length() > 0, "rapport vide");
		writer.getBuffer().setLength(0);
	}
}
