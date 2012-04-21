/*
 * Copyright 2008-2012 by Emeric Vernat
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
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import javax.management.JMException;
import javax.management.MBeanServer;
import javax.management.ObjectName;

import net.bull.javamelody.TestTomcatInformations.GlobalRequestProcessor;
import net.bull.javamelody.TestTomcatInformations.ThreadPool;
import net.sf.ehcache.CacheManager;

import org.junit.Before;
import org.junit.Test;
import org.quartz.JobDetail;
import org.quartz.Scheduler;
import org.quartz.SchedulerException;
import org.quartz.impl.StdSchedulerFactory;

/**
 * Test unitaire de la classe Collector.
 * @author Emeric Vernat
 */
// CHECKSTYLE:OFF
public class TestCollector {
	// CHECKSTYLE:ON
	private static final String TEST = "test";

	/** Test. */
	@Before
	public void setUp() {
		Utils.initialize();
		Parameters.getStorageDirectory(TEST).mkdirs();
		final File[] files = Parameters.getStorageDirectory(TEST).listFiles();
		if (files != null) {
			for (final File file : files) {
				if ((file.getName().endsWith(".rrd") || file.getName().endsWith(".ser.gz"))
						&& !file.delete()) {
					file.deleteOnExit();
				}
			}
		}
	}

	private Collector createCollectorWithOneCounter() {
		final Counter counter = createCounter();
		return new Collector("test collector", Collections.singletonList(counter));
	}

	/** Test. */
	@Test
	public void testNewCollector() {
		assertNotNull("collector", createCollectorWithOneCounter());
	}

	/** Test.
	 * @throws SchedulerException e */
	@Test
	public void testToString() throws SchedulerException { // NOPMD
		final Collector collector = createCollectorWithOneCounter();
		assertToStringNotEmpty("collector", collector);
		assertToStringNotEmpty("java", new JavaInformations(null, false));
		assertToStringNotEmpty(
				"thread",
				new ThreadInformations(Thread.currentThread(), Arrays.asList(Thread.currentThread()
						.getStackTrace()), 100, 1000, false, Parameters.getHostAddress()));
		assertToStringNotEmpty("session", new SessionInformations(new SessionTestImpl(true), true));
		assertToStringNotEmpty("memory", new MemoryInformations());
		CacheManager.getInstance().addCache("testToString");
		try {
			assertToStringNotEmpty("cache", new CacheInformations(CacheManager.getInstance()
					.getEhcache("testToString")));
		} finally {
			CacheManager.getInstance().shutdown();
		}
		final Scheduler scheduler = StdSchedulerFactory.getDefaultScheduler();
		final JobDetail job = new JobDetail("job", null, JobTestImpl.class);
		assertToStringNotEmpty("job", new JobInformations(job, null, scheduler));
		assertToStringNotEmpty("connectionInfos", new ConnectionInformations());
	}

	private static void assertToStringNotEmpty(String type, Object value) {
		if (value.toString().isEmpty()) {
			fail("toString " + type + " vide");
		}
	}

	/** Test. */
	@Test
	public void testClearCounter() {
		final Counter counter = createCounter();
		final Collector collector = new Collector("test collector",
				Collections.singletonList(counter));
		counter.addRequest("test clear", 0, 0, false, 1000);
		collector.clearCounter(counter.getName());
		if (counter.getRequestsCount() != 0) {
			fail("counter vide");
		}
		collector.clearCounter("nothing");
	}

	private Counter createCounter() {
		return new Counter("http", null);
	}

	/** Test. */
	@Test
	public void testGetApplication() {
		assertEquals("getApplication", "test collector", createCollectorWithOneCounter()
				.getApplication());
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testCollectWithoutErrors() throws IOException {
		final Counter counter = createCounter();
		final Counter jspCounter = new Counter(Counter.JSP_COUNTER_NAME, null);
		final Counter strutsCounter = new Counter(Counter.STRUTS_COUNTER_NAME, null);
		final Counter jobCounter = new Counter(Counter.JOB_COUNTER_NAME, null);
		final Collector collector = new Collector(TEST, Arrays.asList(counter, jspCounter,
				strutsCounter, jobCounter));
		if (collector.getCounters().size() == 0) {
			fail("getCounters");
		}
		counter.addRequest("test1", 0, 0, false, 1000);
		jspCounter.addRequest("test2", 0, 0, false, 0);
		strutsCounter.addRequest("test3", 0, 0, false, 0);
		jobCounter.addRequest("test4", 0, 0, false, 0);
		collector.collectWithoutErrors(Collections.singletonList(new JavaInformations(null, true)));
		counter.addRequest("test2", 0, 0, false, 1000);
		counter.addRequest("test3", 1000, 500, false, 1000);
		counter.addRequest("test4", 10000, 200, true, 10000);
		collector
				.collectWithoutErrors(Collections.singletonList(new JavaInformations(null, false)));
		final Counter buildsCounter = new Counter(Counter.BUILDS_COUNTER_NAME, null);
		new Collector(TEST, Collections.singletonList(buildsCounter))
				.collectWithoutErrors(Collections.singletonList(new JavaInformations(null, false)));
		setProperty(Parameter.NO_DATABASE, "true");
		try {
			new Collector(TEST, Collections.singletonList(counter))
					.collectWithoutErrors(Collections.singletonList(new JavaInformations(null,
							false)));
		} finally {
			setProperty(Parameter.NO_DATABASE, null);
		}

		if (collector.getLastCollectDuration() == 0) {
			fail("getLastCollectDuration");
		}

		if (collector.getCounterJRobins().size() == 0) {
			fail("getCounterJRobins");
		}
		final Range range = Period.JOUR.getRange();
		for (final JRobin jrobin : collector.getCounterJRobins()) {
			final JRobin robin = collector.getJRobin(jrobin.getName());
			assertNotNull("getJRobin non null", robin);
			jrobin.graph(range, 80, 80);
			robin.deleteFile();
		}
		for (final JRobin jrobin : collector.getOtherJRobins()) {
			final JRobin robin = collector.getJRobin(jrobin.getName());
			assertNotNull("getJRobin non null", robin);
			robin.deleteFile();
		}
		for (final CounterRequest request : counter.getRequests()) {
			final JRobin robin = collector.getJRobin(request.getId());
			assertNotNull("getJRobin non null", robin);
			robin.deleteFile();
		}
		assertNull("getJRobin null", collector.getJRobin("n'importe quoi"));
	}

	/** Test.
	 * @throws JMException e */
	@Test
	public void testCollectTomcatInformations() throws JMException {
		final MBeanServer mBeanServer = MBeans.getPlatformMBeanServer();
		final List<ObjectName> mBeans = new ArrayList<ObjectName>();
		try {
			mBeans.add(mBeanServer.registerMBean(new ThreadPool(),
					new ObjectName("Catalina:type=ThreadPool,name=jk-8009")).getObjectName());
			mBeans.add(mBeanServer.registerMBean(new GlobalRequestProcessor(),
					new ObjectName("Catalina:type=GlobalRequestProcessor,name=jk-8009"))
					.getObjectName());
			TomcatInformations.initMBeans();
			final Collector collector = new Collector(TEST,
					Arrays.asList(new Counter("http", null)));
			collector.collectWithoutErrors(Collections.singletonList(new JavaInformations(null,
					true)));
		} finally {
			for (final ObjectName registeredMBean : mBeans) {
				mBeanServer.unregisterMBean(registeredMBean);
			}
			TomcatInformations.initMBeans();
		}
	}

	/** Test. */
	@Test
	public void testRemoveRequest() {
		final Counter counter = new Counter("error", null);
		counter.setMaxRequestsCount(1);
		final Collector collector = new Collector(TEST, Collections.singletonList(counter));

		// test removeRequest dans collectCounterData
		counter.addRequest("test 1", 0, 0, false, 1000);
		counter.addRequest("test 2", 0, 0, false, 1000);
		for (int i = 0; i < 50; i++) {
			counter.addRequest("test 3", 0, 0, false, 1000);
		}
		collector.collectWithoutErrors(Collections.<JavaInformations> emptyList());
		if (counter.getRequestsCount() > 1) {
			fail("removeRequest");
		}
		counter.addRequest("test 1", 0, 0, false, 1000);
		counter.addRequest("test 2", 0, 0, false, 1000);
		collector.collectWithoutErrors(Collections.<JavaInformations> emptyList());
		if (counter.getRequestsCount() > 1) {
			fail("removeRequest");
		}
		for (int i = 0; i < counter.getMaxRequestsCount() * 2; i++) {
			counter.addRequest("test 1", 0, 0, false, 1000);
		}
		for (int i = 0; i <= 10; i++) {
			counter.addRequest("test 2", 0, 0, false, 1000);
		}
		counter.addRequest("test 3", 0, 0, false, 1000);
		counter.addRequest("test 4", 0, 0, false, 1000);
		collector.collectWithoutErrors(Collections.<JavaInformations> emptyList());
		if (counter.getRequestsCount() > 1) {
			fail("removeRequest");
		}
	}

	/** Test. */
	@Test
	public void testGetCounterByName() {
		final Counter counter = createCounter();
		final Collector collector = new Collector("test collector2",
				Collections.singletonList(counter));
		assertNotNull("getCounterByName", collector.getCounterByName(counter.getName()));

		assertNull("getCounterByName", collector.getCounterByName("unknown"));
	}

	/** Test. */
	@Test
	public void testGetCounterByRequestId() {
		final Counter counter = createCounter();
		final Collector collector = new Collector("test collector3",
				Collections.singletonList(counter));
		counter.addRequest("test request", 0, 0, false, 1000);
		final CounterRequest request = counter.getRequests().get(0);
		assertEquals("getCounterByRequestId", counter, collector.getCounterByRequestId(request));
		assertNull("getCounterByRequestId",
				collector.getCounterByRequestId(new CounterRequest("test", "unknown")));
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testGetRangeCountersToBeDisplayed() throws IOException {
		final Counter counter = createCounter();
		final Collector collector = new Collector(TEST, Collections.singletonList(counter));
		if (collector.getCounters().size() == 0) {
			fail("getCounters");
		}
		final JavaInformations javaInformations = new JavaInformations(null, true);
		final List<JavaInformations> javaInformationsList = Collections
				.singletonList(javaInformations);
		counter.addRequest("test1", 0, 0, false, 1000);
		collector.collectWithoutErrors(javaInformationsList);
		counter.addRequest("test1", 0, 0, false, 1000);
		collector.collectWithoutErrors(javaInformationsList);
		collector.collectWithoutErrors(javaInformationsList);

		assertEquals("jour", 1, getSizeOfCountersToBeDisplayed(collector, Period.JOUR));
		assertEquals("semaine", 1, getSizeOfCountersToBeDisplayed(collector, Period.SEMAINE));
		assertEquals("mois", 1, getSizeOfCountersToBeDisplayed(collector, Period.MOIS));
		assertEquals("année", 1, getSizeOfCountersToBeDisplayed(collector, Period.ANNEE));
		assertEquals("tout", 1, getSizeOfCountersToBeDisplayed(collector, Period.TOUT));
		assertEquals(
				"custom",
				1,
				collector.getRangeCountersToBeDisplayed(
						Range.createCustomRange(new Date(), new Date())).size());
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testGetRangeCounter() throws IOException {
		final Counter counter = createCounter();
		final Counter counter2 = new Counter("sql", null);
		final Collector collector = new Collector(TEST, Arrays.asList(counter, counter2));
		collector.getRangeCounter(Period.JOUR.getRange(), counter2.getName());
		collector.getRangeCounter(Period.TOUT.getRange(), counter2.getName());
		try {
			collector.getRangeCounter(Period.TOUT.getRange(), "unknown");
		} catch (final IllegalArgumentException e) {
			assertNotNull("getRangeCounter", e);
		}
	}

	private int getSizeOfCountersToBeDisplayed(Collector collector, Period period)
			throws IOException {
		return collector.getRangeCountersToBeDisplayed(period.getRange()).size();
	}

	/** Test. */
	@Test
	public void testStop() {
		final Collector collector = createCollectorWithOneCounter();
		collector.stop();
		if (collector.getCounters().size() == 0) {
			fail("collector.getCounters() ne doit pas être vide après stop");
		}
		// on ne risque pas grand chose à tenter de détacher quelque chose que l'on a pas attacher
		Collector.detachVirtualMachine();

		// on provoque une erreur, mais elle ne doit pas remonter (seulement trace dans console)
		setProperty(Parameter.STORAGE_DIRECTORY, "/???");
		final Counter counter = createCounter();
		final Collector collector2 = new Collector("test stop", Collections.singletonList(counter));
		counter.addRequest("test stop", 0, 0, false, 1000);
		collector2
				.collectWithoutErrors(Collections.singletonList(new JavaInformations(null, true)));
		collector2.stop();
		setProperty(Parameter.STORAGE_DIRECTORY, "javamelody");
	}

	/** Test. */
	@Test
	public void testJavaInformations() {
		final JavaInformations javaInformations = new JavaInformations(null, true);
		javaInformations.getUnixMaxFileDescriptorCount();
		javaInformations.getContextPath();
		javaInformations.isDependenciesEnabled();
		javaInformations.getDependenciesList();
		javaInformations.getDependencies();
		javaInformations.isStackTraceEnabled();
		javaInformations.isCacheEnabled();
		javaInformations.doesWebXmlExists();
		javaInformations.doesPomXmlExists();
		assertNotNull("JavaInformations", javaInformations);
	}

	/** Test. */
	@Test
	public void testThreadInformations() {
		assertTrue("getCurrentThreadCpuTime", ThreadInformations.getCurrentThreadCpuTime() > 0);
	}

	/** Test.
	 * @throws IOException e
	 * @throws SQLException e */
	@Test
	public void testCollectorServer() throws IOException, SQLException {
		Utils.setProperty(Parameters.PARAMETER_SYSTEM_PREFIX + "mockLabradorRetriever", "true");

		// ce test ne fait que vérifier s'il n'y a pas d'erreur inattendue
		// car sans serveur d'application monitoré il ne peut rien faire d'autre
		final String application = "testapp";
		Parameters.removeCollectorApplication(application);

		final CollectorServer collectorServer = new CollectorServer();
		try {
			collectorServer.collectWithoutErrors();

			// pour être sûr qu'il y a une application
			final List<URL> urls = Parameters
					.parseUrl("http://localhost/test1,http://localhost:8090/test1");
			Parameters.addCollectorApplication(application, urls);

			collectorServer.collectWithoutErrors();
			Parameters.removeCollectorApplication(application);
			collectorServer.addCollectorApplication(application, urls);
			collectorServer.collectSessionInformations(application, null);
			collectorServer.collectSessionInformations(application, "sessionId");
			final Connection connection = TestDatabaseInformations.initH2();
			try {
				collectorServer.collectDatabaseInformations(application, 0);
			} finally {
				connection.close();
			}
			collectorServer.collectHeapHistogram(application);
			collectorServer.getCollectorByApplication(application);
			collectorServer.getJavaInformationsByApplication(application);
			collectorServer.isApplicationDataAvailable(application);
			collectorServer.getFirstApplication();
			collectorServer.scheduleReportMailForCollectorServer(application);
			collectorServer.removeCollectorApplication(application);
			Parameters.addCollectorApplication(application, urls);
		} finally {
			collectorServer.stop();
			Parameters.removeCollectorApplication(application);
		}

		try {
			// test d'une erreur dans l'instanciation du CollectorServer : timer.cancel() doit être appelé
			setProperty(Parameter.RESOLUTION_SECONDS, "-1");
			new CollectorServer().stop();
		} catch (final IllegalStateException e) {
			assertNotNull("ok", e);
		} finally {
			setProperty(Parameter.RESOLUTION_SECONDS, null);
		}
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testCollectorMail() throws IOException {
		Utils.setProperty(Parameters.PARAMETER_SYSTEM_PREFIX + "mockLabradorRetriever", "true");

		// test mail_session
		setProperty(Parameter.MAIL_SESSION, null);
		CollectorServer tmp = new CollectorServer();
		tmp.collectWithoutErrors();
		tmp.stop();
		setProperty(Parameter.MAIL_SESSION, "test");
		setProperty(Parameter.ADMIN_EMAILS, null);
		tmp = new CollectorServer();
		tmp.collectWithoutErrors();
		tmp.stop();
		setProperty(Parameter.ADMIN_EMAILS, "evernat@free.fr");
		tmp = new CollectorServer();
		tmp.collectWithoutErrors();
		tmp.stop();
		setProperty(Parameter.MAIL_SESSION, null);
		setProperty(Parameter.ADMIN_EMAILS, null);
	}

	private static void setProperty(Parameter parameter, String value) {
		Utils.setProperty(parameter, value);
	}
}
