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

import static org.junit.Assert.assertTrue;

import java.io.ByteArrayOutputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.management.MBeanServer;
import javax.management.MBeanServerFactory;
import javax.management.ObjectName;

import net.bull.javamelody.TestTomcatInformations.GlobalRequestProcessor;
import net.bull.javamelody.TestTomcatInformations.ThreadPool;

import org.junit.Before;
import org.junit.Test;

import com.lowagie.text.Document;

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
	public void testTomcatInformations() throws Exception { // NOPMD
		final ByteArrayOutputStream output = new ByteArrayOutputStream();
		final PdfDocumentFactory pdfDocumentFactory = new PdfDocumentFactory(TEST_APP, null, output);
		final List<MBeanServer> mBeanServerList = MBeanServerFactory.findMBeanServer(null);
		final MBeanServer mBeanServer;
		if (mBeanServerList.isEmpty()) {
			mBeanServer = MBeanServerFactory.createMBeanServer();
		} else {
			mBeanServer = mBeanServerList.get(0);
		}
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
			final Document document = pdfDocumentFactory.createDocument();
			document.open();
			final PdfJavaInformationsReport pdfReport = new PdfJavaInformationsReport(
					myJavaInformationsList, document);
			pdfReport.writeInformationsDetails();
			document.close();
			assertNotEmptyAndClear(output);

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
