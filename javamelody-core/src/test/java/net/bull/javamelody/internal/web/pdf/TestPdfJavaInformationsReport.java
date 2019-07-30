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
package net.bull.javamelody.internal.web.pdf;

import static org.junit.Assert.assertTrue;

import java.io.ByteArrayOutputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.management.MBeanServer;
import javax.management.ObjectName;

import org.junit.Before;
import org.junit.Test;

import com.lowagie.text.Document;

import net.bull.javamelody.Utils;
import net.bull.javamelody.internal.model.JavaInformations;
import net.bull.javamelody.internal.model.MBeans;
import net.bull.javamelody.internal.model.TestTomcatInformations.GlobalRequestProcessor;
import net.bull.javamelody.internal.model.TestTomcatInformations.ThreadPool;
import net.bull.javamelody.internal.model.TomcatInformations;

/**
 * Test unitaire de la classe PdfJavaInformationsReport.
 * @author Emeric Vernat
 */
public class TestPdfJavaInformationsReport {
	private static final String TEST_APP = "test app";

	/** Check. */
	@Before
	public void setUp() {
		Utils.initialize();
	}

	/** Test.
	 * @throws Exception e */
	@Test
	public void testTomcatInformations() throws Exception {
		final ByteArrayOutputStream output = new ByteArrayOutputStream();
		final PdfDocumentFactory pdfDocumentFactory = new PdfDocumentFactory(TEST_APP, null,
				output);
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
			final Document document = pdfDocumentFactory.createDocument();
			document.open();
			final PdfJavaInformationsReport pdfReport = new PdfJavaInformationsReport(
					myJavaInformationsList, document);
			pdfReport.writeInformationsDetails();
			document.close();
			assertNotEmptyAndClear(output);

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
			final Document document2 = pdfDocumentFactory.createDocument();
			document2.open();
			final PdfJavaInformationsReport pdfReport2 = new PdfJavaInformationsReport(
					myJavaInformationsList2, document2);
			pdfReport2.writeInformationsDetails();
			document2.close();
			assertNotEmptyAndClear(output);

			jk8010.setrequestCount(1000);
			final List<JavaInformations> myJavaInformationsList3 = Arrays
					.asList(new JavaInformations(null, true));
			final Document document3 = pdfDocumentFactory.createDocument();
			document3.open();
			final PdfJavaInformationsReport pdfReport3 = new PdfJavaInformationsReport(
					myJavaInformationsList3, document3);
			pdfReport3.writeInformationsDetails();
			document3.close();
			assertNotEmptyAndClear(output);
		} finally {
			for (final ObjectName registeredMBean : mBeans) {
				mBeanServer.unregisterMBean(registeredMBean);
			}
			TomcatInformations.initMBeans();
		}
	}

	private void assertNotEmptyAndClear(ByteArrayOutputStream output) {
		assertTrue("rapport vide", output.size() > 0);
		output.reset();
	}
}
