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
package net.bull.javamelody.internal.model;

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
import java.util.Timer;

import javax.cache.Caching;
import javax.cache.configuration.MutableConfiguration;
import javax.management.JMException;
import javax.management.MBeanServer;
import javax.management.ObjectName;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.quartz.JobDetail;
import org.quartz.Scheduler;
import org.quartz.SchedulerException;
import org.quartz.impl.StdSchedulerFactory;

import net.bull.javamelody.JobTestImpl;
import net.bull.javamelody.Parameter;
import net.bull.javamelody.SessionTestImpl;
import net.bull.javamelody.Utils;
import net.bull.javamelody.internal.common.Parameters;
import net.bull.javamelody.internal.model.TestTomcatInformations.GlobalRequestProcessor;
import net.bull.javamelody.internal.model.TestTomcatInformations.ThreadPool;
import net.sf.ehcache.CacheManager;

/**
 * Test unitaire de la classe Collector.
 * @author Emeric Vernat
 */
// CHECKSTYLE:OFF
public class TestCollector {
	// CHECKSTYLE:ON
	private static final String TEST = "test";

	/** Before.
	 * @throws IOException e */
	@Before
	public void setUp() throws IOException {
		Utils.initialize();
		JRobin.initBackendFactory(new Timer(getClass().getSimpleName(), true));
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

	/** After. */
	@After
	public void tearDown() {
		JRobin.stop();
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
	public void testToString() throws SchedulerException {
		final Collector collector = createCollectorWithOneCounter();
		assertToStringNotEmpty("collector", collector);
		assertToStringNotEmpty("java", new JavaInformations(null, false));
		assertToStringNotEmpty("thread",
				new ThreadInformations(Thread.currentThread(),
						Arrays.asList(Thread.currentThread().getStackTrace()), 100, 1000, false,
						Parameters.getHostAddress()));
		assertToStringNotEmpty("session", new SessionInformations(new SessionTestImpl(true), true));
		assertToStringNotEmpty("memory", new MemoryInformations());
		CacheManager.getInstance().addCache("testToString");
		try {
			assertToStringNotEmpty("cache", new CacheInformations(
					CacheManager.getInstance().getEhcache("testToString"), false));
		} finally {
			CacheManager.getInstance().shutdown();
		}
		final MutableConfiguration<Object, Object> conf = new MutableConfiguration<Object, Object>();
		conf.setManagementEnabled(true);
		conf.setStatisticsEnabled(true);
		Caching.getCachingProvider().getCacheManager().createCache("cache", conf);
		try {
			assertToStringNotEmpty("cache", new JCacheInformations("cache"));
		} finally {
			Caching.getCachingProvider().getCacheManager().close();
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
		counter.addRequest("test clear", 0, 0, 0, false, 1000);
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
		assertEquals("getApplication", "test collector",
				createCollectorWithOneCounter().getApplication());
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testCollectWithoutErrors() throws IOException {
		final Counter counter = createCounter();
		final Counter jspCounter = new Counter(Counter.JSP_COUNTER_NAME, null);
		final Counter strutsCounter = new Counter(Counter.STRUTS_COUNTER_NAME, null);
		final Counter jobCounter = new Counter(Counter.JOB_COUNTER_NAME, null);
		final Collector collector = new Collector(TEST,
				Arrays.asList(counter, jspCounter, strutsCounter, jobCounter));
		if (collector.getCounters().size() == 0) {
			fail("getCounters");
		}
		counter.addRequest("test1", 0, 0, 0, false, 1000);
		counter.addRequest("test5", 10000, 200, 200, true, 10000);
		jspCounter.addRequest("test2", 0, 0, 0, false, 0);
		strutsCounter.addRequest("test3", 0, 0, 0, false, 0);
		jobCounter.addRequest("test4", 0, 0, 0, false, 0);
		collector.collectWithoutErrors(Collections.singletonList(new JavaInformations(null, true)));
		counter.addRequest("test2", 0, 0, 0, false, 1000);
		counter.addRequest("test3", 1000, 500, 500, false, 1000);
		counter.addRequest("test4", 10000, 200, 200, true, 10000);
		counter.addRequest("test5", 10000, 200, 200, true, 10000);
		counter.addRequest("test5", 10000, 200, 200, true, 10000);
		collector
				.collectWithoutErrors(Collections.singletonList(new JavaInformations(null, false)));
		final Counter buildsCounter = new Counter(Counter.BUILDS_COUNTER_NAME, null);
		new Collector(TEST, Collections.singletonList(buildsCounter))
				.collectWithoutErrors(Collections.singletonList(new JavaInformations(null, false)));
		setProperty(Parameter.NO_DATABASE, "true");
		try {
			new Collector(TEST, Collections.singletonList(counter)).collectWithoutErrors(
					Collections.singletonList(new JavaInformations(null, false)));
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
			if ("test5".equals(request.getName())) {
				assertNotNull("getJRobin non null", robin);
				robin.deleteFile();
			} else {
				assertNull("getJRobin null", robin);
			}
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
			final Collector collector = new Collector(TEST,
					Arrays.asList(new Counter("http", null)));
			// first time to initialize against NOT_A_NUMBER
			collector.collectWithoutErrors(
					Collections.singletonList(new JavaInformations(null, true)));
			// second time to add value
			collector.collectWithoutErrors(
					Collections.singletonList(new JavaInformations(null, true)));
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
		counter.addRequest("test 1", 0, 0, 0, false, 1000);
		counter.addRequest("test 2", 0, 0, 0, false, 1000);
		for (int i = 0; i < 50; i++) {
			counter.addRequest("test 3", 0, 0, 0, false, 1000);
		}
		collector.collectWithoutErrors(Collections.<JavaInformations> emptyList());
		if (counter.getRequestsCount() > 1) {
			fail("removeRequest");
		}
		counter.addRequest("test 1", 0, 0, 0, false, 1000);
		counter.addRequest("test 2", 0, 0, 0, false, 1000);
		collector.collectWithoutErrors(Collections.<JavaInformations> emptyList());
		if (counter.getRequestsCount() > 1) {
			fail("removeRequest");
		}
		for (int i = 0; i < counter.getMaxRequestsCount() * 2; i++) {
			counter.addRequest("test 1", 0, 0, 0, false, 1000);
		}
		for (int i = 0; i <= 10; i++) {
			counter.addRequest("test 2", 0, 0, 0, false, 1000);
		}
		counter.addRequest("test 3", 0, 0, 0, false, 1000);
		counter.addRequest("test 4", 0, 0, 0, false, 1000);
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
		counter.addRequest("test request", 0, 0, 0, false, 1000);
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
		counter.addRequest("test1", 0, 0, 0, false, 1000);
		collector.collectWithoutErrors(javaInformationsList);
		counter.addRequest("test1", 0, 0, 0, false, 1000);
		collector.collectWithoutErrors(javaInformationsList);
		collector.collectWithoutErrors(javaInformationsList);

		assertEquals("jour", 1, getSizeOfCountersToBeDisplayed(collector, Period.JOUR));
		assertEquals("semaine", 1, getSizeOfCountersToBeDisplayed(collector, Period.SEMAINE));
		assertEquals("mois", 1, getSizeOfCountersToBeDisplayed(collector, Period.MOIS));
		assertEquals("année", 1, getSizeOfCountersToBeDisplayed(collector, Period.ANNEE));
		assertEquals("tout", 1, getSizeOfCountersToBeDisplayed(collector, Period.TOUT));
		assertEquals("custom", 1, collector
				.getRangeCountersToBeDisplayed(Range.createCustomRange(new Date(), new Date()))
				.size());
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

	/** Test.
	 * @throws IOException e */
	@Test
	public void testDeleteObsoleteFiles() throws IOException {
		final Collector collector = createCollectorWithOneCounter();
		collector.deleteObsoleteFiles();
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
		counter.addRequest("test stop", 0, 0, 0, false, 1000);
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

	/** Test. */
	@Test
	public void testCollectorSamplingProfiler() {
		final SamplingProfiler samplingProfiler = new SamplingProfiler();
		final List<Counter> counters = Collections.emptyList();
		final Collector collector = new Collector("test", counters, samplingProfiler);
		assertNotNull("getSamplingProfiler", collector.getSamplingProfiler());
		assertNotNull("getHotspots", collector.getHotspots());
		final Collector collector2 = new Collector("test", counters);
		assertNull("getSamplingProfiler", collector2.getSamplingProfiler());
		try {
			assertNull("getHotspots", collector2.getHotspots());
		} catch (final IllegalStateException e) {
			assertNotNull("e", e);
		}
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
			Parameters.addCollectorApplication(application, urls, null);

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
			collectorServer.collectHotspots(application);
			collectorServer.getCollectorByApplication(application);
			assertNull("getCollectorByApplication",
					collectorServer.getCollectorByApplication("dummy"));
			collectorServer.getJavaInformationsByApplication(application);
			collectorServer.isApplicationDataAvailable(application);
			collectorServer.getFirstApplication();
			collectorServer.scheduleReportMailForCollectorServer(application);
			collectorServer.removeCollectorApplication(application);
			Parameters.addCollectorApplication(application, urls, null);
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
