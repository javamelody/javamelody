/*
 * Copyright 2008-2014 by Emeric Vernat
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

import static org.easymock.EasyMock.createNiceMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import javax.management.JMException;
import javax.naming.Binding;
import javax.naming.Context;
import javax.naming.NamingEnumeration;
import javax.naming.NamingException;
import javax.servlet.ServletContext;

import net.bull.javamelody.SamplingProfiler.SampledMethod;

import org.junit.Before;
import org.junit.Test;

/**
 * Test unitaire de la classe PdfOtherReport.
 * @author Emeric Vernat
 */
//CHECKSTYLE:OFF
public class TestPdfOtherReport {
	private static final String TEST_APP = "test app";

	/** Check. */
	@Before
	public void setUp() {
		Utils.initialize();
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testWriteHeapHistogram() throws IOException {
		final ByteArrayOutputStream output = new ByteArrayOutputStream();
		final InputStream input = getClass().getResourceAsStream("/heaphisto.txt");
		try {
			final PdfOtherReport pdfOtherReport = new PdfOtherReport(TEST_APP, output);
			final HeapHistogram heapHistogram = new HeapHistogram(input, false);
			pdfOtherReport.writeHeapHistogram(heapHistogram);
			pdfOtherReport.close();
		} finally {
			input.close();
		}
		assertNotEmptyAndClear(output);

		final InputStream input2 = getClass().getResourceAsStream("/heaphisto_jrockit.txt");
		try {
			final PdfOtherReport pdfOtherReport = new PdfOtherReport(TEST_APP, output);
			final HeapHistogram heapHistogram = new HeapHistogram(input2, true);
			pdfOtherReport.writeHeapHistogram(heapHistogram);
			pdfOtherReport.close();
		} finally {
			input2.close();
		}
		assertNotEmptyAndClear(output);
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testWriteSessionInformations() throws IOException {
		final ByteArrayOutputStream output = new ByteArrayOutputStream();

		final List<SessionInformations> sessions = new ArrayList<SessionInformations>();
		sessions.add(new SessionInformations(new SessionTestImpl(true), false));
		sessions.add(new SessionInformations(new SessionTestImpl(false), false));
		final SessionTestImpl serializableButNotSession = new SessionTestImpl(true);
		serializableButNotSession.setAttribute("serializable but not",
				Collections.singleton(new Object()));
		sessions.add(new SessionInformations(serializableButNotSession, false));
		PdfOtherReport pdfOtherReport = new PdfOtherReport(TEST_APP, output);
		pdfOtherReport.writeSessionInformations(Collections.<SessionInformations> emptyList());
		assertNotEmptyAndClear(output);

		pdfOtherReport = new PdfOtherReport(TEST_APP, output);
		pdfOtherReport.writeSessionInformations(sessions);
		assertNotEmptyAndClear(output);

		// aucune session sérialisable
		pdfOtherReport = new PdfOtherReport(TEST_APP, output);
		pdfOtherReport.writeSessionInformations(Collections.singletonList(new SessionInformations(
				new SessionTestImpl(false), false)));
		assertNotEmptyAndClear(output);

		// pays non existant
		final SessionTestImpl sessionPays = new SessionTestImpl(true);
		sessionPays.setCountry("nimporte.quoi");
		pdfOtherReport = new PdfOtherReport(TEST_APP, output);
		pdfOtherReport.writeSessionInformations(Collections.singletonList(new SessionInformations(
				sessionPays, false)));
		assertNotEmptyAndClear(output);

		// pays null
		sessionPays.setCountry(null);
		assertNull("countryDisplay null",
				new SessionInformations(sessionPays, false).getCountryDisplay());
		pdfOtherReport = new PdfOtherReport(TEST_APP, output);
		pdfOtherReport.writeSessionInformations(Collections.singletonList(new SessionInformations(
				sessionPays, false)));
		assertNotEmptyAndClear(output);
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testWriteProcessInformations() throws IOException {
		final ByteArrayOutputStream output = new ByteArrayOutputStream();

		PdfOtherReport pdfOtherReport = new PdfOtherReport(TEST_APP, output);
		pdfOtherReport.writeProcessInformations(ProcessInformations.buildProcessInformations(
				getClass().getResourceAsStream("/tasklist.txt"), true, false));
		assertNotEmptyAndClear(output);
		pdfOtherReport = new PdfOtherReport(TEST_APP, output);
		pdfOtherReport.writeProcessInformations(ProcessInformations.buildProcessInformations(
				getClass().getResourceAsStream("/ps.txt"), false, false));
		assertNotEmptyAndClear(output);
		pdfOtherReport = new PdfOtherReport(TEST_APP, output);
		pdfOtherReport.writeProcessInformations(Collections.singletonMap(
				"localhost",
				ProcessInformations.buildProcessInformations(
						getClass().getResourceAsStream("/ps.txt"), false, false)));
		assertNotEmptyAndClear(output);
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testWriteHotspots() throws IOException {
		final ByteArrayOutputStream output = new ByteArrayOutputStream();
		final SamplingProfiler samplingProfiler = new SamplingProfiler(new ArrayList<String>(),
				null);
		final List<SampledMethod> emptyHotspots = samplingProfiler.getHotspots(100);
		samplingProfiler.update();
		final List<SampledMethod> hotspots = samplingProfiler.getHotspots(100);

		PdfOtherReport pdfOtherReport = new PdfOtherReport(TEST_APP, output);
		pdfOtherReport.writeHotspots(emptyHotspots);
		assertNotEmptyAndClear(output);
		pdfOtherReport = new PdfOtherReport(TEST_APP, output);
		pdfOtherReport.writeHotspots(hotspots);
		assertNotEmptyAndClear(output);
	}

	/** Test.
	 * @throws IOException e
	 * @throws NamingException e
	 * @throws SQLException e */
	@Test
	public void testWriteDatabaseInformations() throws IOException, SQLException, NamingException {
		final ByteArrayOutputStream output = new ByteArrayOutputStream();

		TestDatabaseInformations.initJdbcDriverParameters();
		PdfOtherReport pdfOtherReport = new PdfOtherReport(TEST_APP, output);
		pdfOtherReport.writeDatabaseInformations(new DatabaseInformations(0)); // h2.memory
		assertNotEmptyAndClear(output);
		pdfOtherReport = new PdfOtherReport(TEST_APP, output);
		pdfOtherReport.writeDatabaseInformations(new DatabaseInformations(3)); // h2.settings
		assertNotEmptyAndClear(output);
	}

	/** Test.
	 * @throws IOException e
	 * @throws JMException e */
	@Test
	public void testWriteMBeans() throws IOException, JMException {
		final ByteArrayOutputStream output = new ByteArrayOutputStream();
		final PdfOtherReport pdfOtherReport = new PdfOtherReport(TEST_APP, output);
		final List<MBeanNode> allMBeanNodes = MBeans.getAllMBeanNodes();
		pdfOtherReport.writeMBeans(allMBeanNodes);
		assertNotEmptyAndClear(output);

		final PdfOtherReport pdfOtherReport2 = new PdfOtherReport(TEST_APP, output);
		pdfOtherReport2.writeMBeans(Collections.singletonMap("TEST_APP", allMBeanNodes));
		assertNotEmptyAndClear(output);
	}

	/** Test.
	 * @throws IOException e
	 * @throws NamingException e */
	@Test
	public void testWriteJndi() throws NamingException, IOException {
		final String contextPath = "comp/env/";
		final Context context = createNiceMock(Context.class);
		@SuppressWarnings("unchecked")
		final NamingEnumeration<Binding> enumeration = createNiceMock(NamingEnumeration.class);
		expect(context.listBindings("java:" + contextPath)).andReturn(enumeration).anyTimes();
		expect(enumeration.hasMore()).andReturn(true).times(6);
		expect(enumeration.next()).andReturn(new Binding("test value", "test value")).once();
		expect(enumeration.next()).andReturn(
				new Binding("test context", createNiceMock(Context.class))).once();
		expect(enumeration.next()).andReturn(new Binding("", "test")).once();
		expect(enumeration.next()).andReturn(
				new Binding("java:/test context", createNiceMock(Context.class))).once();
		expect(enumeration.next()).andReturn(new Binding("test null classname", null, null)).once();
		expect(enumeration.next()).andThrow(new NamingException("test")).once();

		final ServletContext servletContext = createNiceMock(ServletContext.class);
		expect(servletContext.getServerInfo()).andReturn("Mock").anyTimes();
		replay(servletContext);
		Parameters.initialize(servletContext);

		replay(context);
		replay(enumeration);

		final List<JndiBinding> bindings = JndiBinding.listBindings(context, contextPath);
		final ByteArrayOutputStream output = new ByteArrayOutputStream();
		final PdfOtherReport pdfOtherReport = new PdfOtherReport(TEST_APP, output);
		pdfOtherReport.writeJndi(bindings, contextPath);
		assertNotEmptyAndClear(output);
		verify(context);
		verify(enumeration);
		verify(servletContext);

		final PdfOtherReport pdfOtherReport2 = new PdfOtherReport(TEST_APP, output);
		final List<JndiBinding> bindings2 = Collections.emptyList();
		pdfOtherReport2.writeJndi(bindings2, "");
		assertNotEmptyAndClear(output);
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testWriteRuntimeDependencies() throws IOException {
		final ByteArrayOutputStream output = new ByteArrayOutputStream();
		final Counter sqlCounter = new Counter("sql", null);
		final Counter counter = new Counter("services", null, sqlCounter);
		counter.bindContextIncludingCpu("BeanA.test");
		counter.bindContextIncludingCpu("BeanA.test2");
		counter.bindContextIncludingCpu("BeanB.test");
		counter.addRequestForCurrentContext(false);
		counter.bindContextIncludingCpu("BeanB.test2");
		counter.addRequestForCurrentContext(false);
		counter.addRequestForCurrentContext(false);
		counter.addRequestForCurrentContext(false);
		counter.bindContextIncludingCpu("test");
		counter.bindContextIncludingCpu("BeanA.test");
		counter.addRequestForCurrentContext(false);
		counter.addRequestForCurrentContext(false);
		counter.bindContextIncludingCpu("test2");
		sqlCounter.bindContextIncludingCpu("sql");
		sqlCounter.addRequestForCurrentContext(false);
		counter.addRequestForCurrentContext(false);
		final PdfOtherReport pdfOtherReport = new PdfOtherReport(TEST_APP, output);
		pdfOtherReport.writeRuntimeDependencies(counter, Period.TOUT.getRange());
		assertNotEmptyAndClear(output);
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testWriteCounterSummaryPerClass() throws IOException {
		final ByteArrayOutputStream output = new ByteArrayOutputStream();
		final PdfOtherReport pdfOtherReport = new PdfOtherReport(TEST_APP, output);
		final Counter counter = new Counter("services", null);
		final Collector collector = new Collector(TEST_APP, Arrays.asList(counter));
		pdfOtherReport
				.writeCounterSummaryPerClass(collector, counter, null, Period.TOUT.getRange());
		assertNotEmptyAndClear(output);
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testWriteAllCurrentRequestsAsPart() throws IOException {
		final ByteArrayOutputStream output = new ByteArrayOutputStream();
		final PdfOtherReport pdfOtherReport = new PdfOtherReport(TEST_APP, output);
		final Counter counter = new Counter("services", null);
		final Collector collector = new Collector(TEST_APP, Arrays.asList(counter));
		final long timeOfSnapshot = System.currentTimeMillis();
		final List<CounterRequestContext> requests = Collections.emptyList();
		final JavaInformations javaInformations = new JavaInformations(null, true);
		final Map<JavaInformations, List<CounterRequestContext>> currentRequests = Collections
				.singletonMap(javaInformations, requests);
		pdfOtherReport.writeAllCurrentRequestsAsPart(currentRequests, collector,
				collector.getCounters(), timeOfSnapshot);
		assertNotEmptyAndClear(output);
	}

	private void assertNotEmptyAndClear(ByteArrayOutputStream output) {
		assertTrue("rapport vide", output.size() > 0);
		output.reset();
	}
}
