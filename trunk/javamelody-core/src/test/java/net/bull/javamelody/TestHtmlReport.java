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

import static net.bull.javamelody.HttpParameters.SESSIONS_PART;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.io.StringWriter;
import java.sql.Connection;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Random;
import java.util.Timer;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import javax.management.JMException;
import javax.management.MBeanServer;
import javax.management.MBeanServerFactory;
import javax.management.ObjectName;

import net.bull.javamelody.TestTomcatInformations.GlobalRequestProcessor;
import net.bull.javamelody.TestTomcatInformations.ThreadPool;
import net.sf.ehcache.Cache;
import net.sf.ehcache.CacheManager;
import net.sf.ehcache.Element;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.quartz.CronTrigger;
import org.quartz.JobDetail;
import org.quartz.Scheduler;
import org.quartz.SchedulerException;
import org.quartz.SimpleTrigger;
import org.quartz.Trigger;
import org.quartz.impl.StdSchedulerFactory;

/**
 * Test unitaire de la classe HtmlReport.
 * @author Emeric Vernat
 */
// CHECKSTYLE:OFF
public class TestHtmlReport {
	// CHECKSTYLE:ON
	private Timer timer;
	private List<JavaInformations> javaInformationsList;
	private Counter sqlCounter;
	private Counter servicesCounter;
	private Counter jspCounter;
	private Counter counter;
	private Counter errorCounter;
	private Collector collector;
	private StringWriter writer;

	/** Initialisation. */
	@Before
	public void setUp() {
		Utils.initialize();
		// pour testTomcatInformations (si la classe TomcatInformations est déjà chargée,
		// c'est trop tard, et c'est pourquoi cela doit être défini au lancement de junit)
		System.setProperty("catalina.home", "unknown");
		timer = new Timer("test timer", true);
		javaInformationsList = Collections.singletonList(new JavaInformations(null, true));
		sqlCounter = new Counter("sql", "db.png");
		sqlCounter.setDisplayed(false);
		servicesCounter = new Counter("services", "beans.png", sqlCounter);
		jspCounter = new Counter(Counter.JSP_COUNTER_NAME, null);
		// counterName doit être http, sql ou ejb pour que les libellés de graph soient trouvés dans les traductions
		counter = new Counter("http", "dbweb.png", sqlCounter);
		errorCounter = new Counter(Counter.ERROR_COUNTER_NAME, null);
		final Counter jobCounter = JobGlobalListener.getJobCounter();
		collector = new Collector("test", Arrays.asList(counter, sqlCounter, servicesCounter,
				jspCounter, errorCounter, jobCounter), timer);
		writer = new StringWriter();
	}

	/** Finalisation. */
	@After
	public void tearDown() {
		timer.cancel();
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testEmptyCounter() throws IOException {
		final HtmlReport htmlReport = new HtmlReport(collector, null, javaInformationsList,
				Period.TOUT, writer);
		// rapport avec counter sans requête
		counter.clear();
		errorCounter.clear();
		htmlReport.toHtml(null, null);
		assertNotEmptyAndClear(writer);
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testDoubleJavaInformations() throws IOException {
		final List<JavaInformations> myJavaInformationsList = Arrays.asList(new JavaInformations(
				null, true), new JavaInformations(null, true));
		final HtmlReport htmlReport = new HtmlReport(collector, null, myJavaInformationsList,
				Period.TOUT, writer);
		htmlReport.toHtml(null, null);
		assertNotEmptyAndClear(writer);
	}

	/** Test.
	 * @throws IOException e
	 * @throws JMException e */
	@Test
	public void testTomcatInformations() throws IOException, JMException {
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
			final HtmlReport htmlReport = new HtmlReport(collector, null, myJavaInformationsList,
					Period.TOUT, writer);
			htmlReport.toHtml(null, null);
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
			final HtmlReport htmlReport2 = new HtmlReport(collector, null, myJavaInformationsList2,
					Period.TOUT, writer);
			htmlReport2.toHtml(null, null);
			assertNotEmptyAndClear(writer);

			jk8010.setrequestCount(1000);
			final List<JavaInformations> myJavaInformationsList3 = Arrays
					.asList(new JavaInformations(null, true));
			final HtmlReport htmlReport3 = new HtmlReport(collector, null, myJavaInformationsList3,
					Period.TOUT, writer);
			htmlReport3.toHtml(null, null);
			assertNotEmptyAndClear(writer);
		} finally {
			for (final ObjectName registeredMBean : mBeans) {
				mBeanServer.unregisterMBean(registeredMBean);
			}
			TomcatInformations.initMBeans();
		}
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testCounter() throws IOException {
		// counter avec 3 requêtes
		setProperty(Parameter.WARNING_THRESHOLD_MILLIS, "500");
		setProperty(Parameter.SEVERE_THRESHOLD_MILLIS, "1500");
		setProperty(Parameter.ANALYTICS_ID, "123456789");
		counter.addRequest("test1", 0, 0, false, 1000);
		counter.addRequest("test2", 1000, 500, false, 1000);
		counter.addRequest("test3", 100000, 50000, true, 10000);
		collector.collectWithoutErrors(javaInformationsList);

		final HtmlReport htmlReport = new HtmlReport(collector, null, javaInformationsList,
				Period.TOUT, writer);
		htmlReport.toHtml("message 2", null);
		assertNotEmptyAndClear(writer);

		setProperty(Parameter.NO_DATABASE, Boolean.TRUE.toString());
		collector.collectWithoutErrors(javaInformationsList);
		htmlReport.toHtml("message 2", null);
		assertNotEmptyAndClear(writer);
		setProperty(Parameter.NO_DATABASE, Boolean.FALSE.toString());

		setProperty(Parameter.WARNING_THRESHOLD_MILLIS, "-1");
		try {
			htmlReport.toHtml("message 2", null);
		} catch (final IllegalStateException e) {
			assertNotNull("ok", e);
		}
		setProperty(Parameter.WARNING_THRESHOLD_MILLIS, null);
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testErrorCounter() throws IOException {
		// errorCounter
		errorCounter.addRequestForSystemError("error", -1, -1, null);
		errorCounter.addRequestForSystemError("error2", -1, -1, "ma stack-trace");
		collector.collectWithoutErrors(javaInformationsList);
		final HtmlReport htmlReport = new HtmlReport(collector, null, javaInformationsList,
				Period.TOUT, writer);
		htmlReport.toHtml("message 3", null);
		assertNotEmptyAndClear(writer);
		for (final CounterRequest request : errorCounter.getRequests()) {
			htmlReport.writeRequestAndGraphDetail(request.getId());
		}
		htmlReport.writeRequestAndGraphDetail("n'importe quoi");
		assertNotEmptyAndClear(writer);
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testPeriodeNonTout() throws IOException {
		// counter avec période non TOUT et des requêtes
		collector.collectWithoutErrors(javaInformationsList);
		final String requestName = "test 1";
		counter.bindContext(requestName, "complete test 1");
		sqlCounter.addRequest("sql1", 10, 10, false, -1);
		counter.addRequest(requestName, 0, 0, false, 1000);
		counter.addRequest("test2", 1000, 500, false, 1000);
		counter.addRequest("test3", 10000, 500, true, 10000);
		collector.collectWithoutErrors(javaInformationsList);
		final HtmlReport htmlReport = new HtmlReport(collector, null, javaInformationsList,
				Period.SEMAINE, writer);
		htmlReport.toHtml("message 6", null);
		assertNotEmptyAndClear(writer);

		// période personnalisée
		final HtmlReport htmlReportRange = new HtmlReport(collector, null, javaInformationsList,
				Range.createCustomRange(new Date(), new Date()), writer);
		htmlReportRange.toHtml("message 6", null);
		assertNotEmptyAndClear(writer);
	}

	/** Test.
	 * @throws Exception e */
	@Test
	public void testWriteRequests() throws Exception { // NOPMD
		final HtmlReport htmlReport = new HtmlReport(collector, null, javaInformationsList,
				Period.SEMAINE, writer);
		htmlReport.writeRequestAndGraphDetail("httpHitsRate");
		assertNotEmptyAndClear(writer);

		// writeRequestAndGraphDetail avec drill-down
		collector.collectWithoutErrors(javaInformationsList);
		// si sqlCounter reste à displayed=false,
		// il ne sera pas utilisé dans writeRequestAndGraphDetail
		sqlCounter.setDisplayed(true);
		final String requestName = "test 1";
		counter.bindContext(requestName, "complete test 1");
		servicesCounter.clear();
		servicesCounter.bindContext("myservices.service1", "service1");
		sqlCounter.bindContext("sql1", "complete sql1");
		sqlCounter.addRequest("sql1", 5, -1, false, -1);
		servicesCounter.addRequest("myservices.service1", 10, 10, false, -1);
		servicesCounter.bindContext("myservices.service2", "service2");
		servicesCounter.addRequest("myservices.service2", 10, 10, false, -1);
		servicesCounter.addRequest("otherservices.service3", 10, 10, false, -1);
		jspCounter.addRequest("jsp1", 10, 10, false, -1);
		counter.addRequest(requestName, 0, 0, false, 1000);
		collector.collectWithoutErrors(javaInformationsList);
		final HtmlReport toutHtmlReport = new HtmlReport(collector, null, javaInformationsList,
				Period.TOUT, writer);
		for (final Counter collectorCounter : collector.getCounters()) {
			for (final CounterRequest request : collectorCounter.getRequests()) {
				toutHtmlReport.writeRequestAndGraphDetail(request.getId());
				assertNotEmptyAndClear(writer);
				toutHtmlReport.writeRequestUsages(request.getId());
				assertNotEmptyAndClear(writer);
			}
		}
		sqlCounter.setDisplayed(false);

		// writeCounterSummaryPerClass
		toutHtmlReport.writeCounterSummaryPerClass(servicesCounter.getName(), null);
		String requestId = new CounterRequest("myservices", servicesCounter.getName()).getId();
		toutHtmlReport.writeCounterSummaryPerClass(servicesCounter.getName(), requestId);
		requestId = new CounterRequest("otherservices", servicesCounter.getName()).getId();
		toutHtmlReport.writeCounterSummaryPerClass(servicesCounter.getName(), requestId);
		toutHtmlReport.writeCounterSummaryPerClass(servicesCounter.getName(), "unknown");
	}

	/** Test.
	 * @throws Exception e */
	@Test
	public void testOtherWrites() throws Exception { // NOPMD
		final HtmlReport htmlReport = new HtmlReport(collector, null, javaInformationsList,
				Period.SEMAINE, writer);

		htmlReport.writeAllCurrentRequestsAsPart(true);
		assertNotEmptyAndClear(writer);
		htmlReport.writeAllCurrentRequestsAsPart(false);
		assertNotEmptyAndClear(writer);
		htmlReport.writeAllThreadsAsPart();
		assertNotEmptyAndClear(writer);

		htmlReport.writeSessionDetail("", null);
		assertNotEmptyAndClear(writer);
		htmlReport.writeSessions(Collections.<SessionInformations> emptyList(), "message",
				SESSIONS_PART);
		assertNotEmptyAndClear(writer);
		htmlReport
				.writeSessions(Collections.<SessionInformations> emptyList(), null, SESSIONS_PART);
		assertNotEmptyAndClear(writer);
		final String fileName = ProcessInformations.WINDOWS ? "/tasklist.txt" : "/ps.txt";
		htmlReport.writeProcesses(ProcessInformations.buildProcessInformations(getClass()
				.getResourceAsStream(fileName), ProcessInformations.WINDOWS));
		assertNotEmptyAndClear(writer);
		HtmlReport.writeAddAndRemoveApplicationLinks(null, writer);
		assertNotEmptyAndClear(writer);
		HtmlReport.writeAddAndRemoveApplicationLinks("test", writer);
		assertNotEmptyAndClear(writer);
		final Connection connection = TestDatabaseInformations.initH2();
		try {
			htmlReport.writeDatabase(new DatabaseInformations(0)); // h2.memory
			assertNotEmptyAndClear(writer);
			htmlReport.writeDatabase(new DatabaseInformations(3)); // h2.settings avec nbColumns==2
			assertNotEmptyAndClear(writer);
			JavaInformations.setWebXmlExistsAndPomXmlExists(true, true);
			htmlReport.toHtml(null, null); // pom.xml dans HtmlJavaInformationsReport.writeDependencies
			assertNotEmptyAndClear(writer);
			setProperty(Parameter.SYSTEM_ACTIONS_ENABLED, Boolean.TRUE.toString());
			htmlReport.toHtml(null, null); // writeSystemActionsLinks
			assertNotEmptyAndClear(writer);
			setProperty(Parameter.NO_DATABASE, Boolean.TRUE.toString());
			htmlReport.toHtml(null, null); // writeSystemActionsLinks
			assertNotEmptyAndClear(writer);
		} finally {
			connection.close();
		}
	}

	/** Test.
	 * @throws Exception e */
	@Test
	public void testWriteConnections() throws Exception { // NOPMD
		final HtmlReport htmlReport = new HtmlReport(collector, null, javaInformationsList,
				Period.SEMAINE, writer);

		// avant initH2 pour avoir une liste de connexions vide
		htmlReport.writeConnections(JdbcWrapper.getConnectionInformationsList(), false);
		assertNotEmptyAndClear(writer);
		// une connexion créée sur le thread courant
		final Connection connection = TestDatabaseInformations.initH2();
		// une deuxième connexion créée sur un thread qui n'existera plus quand le rapport sera généré
		final ExecutorService executorService = Executors.newFixedThreadPool(1);
		final Callable<Connection> task = new Callable<Connection>() {
			public Connection call() {
				return TestDatabaseInformations.initH2();
			}
		};
		final Future<Connection> future = executorService.submit(task);
		final Connection connection2 = future.get();
		executorService.shutdown();
		try {
			htmlReport.writeConnections(JdbcWrapper.getConnectionInformationsList(), false);
			assertNotEmptyAndClear(writer);
			htmlReport.writeConnections(JdbcWrapper.getConnectionInformationsList(), true);
			assertNotEmptyAndClear(writer);
		} finally {
			connection.close();
			connection2.close();
		}
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testRootContexts() throws IOException {
		HtmlReport htmlReport;
		// addRequest pour que CounterRequestContext.getCpuTime() soit appelée
		counter.addRequest("first request", 100, 100, false, 1000);
		TestCounter.bindRootContexts("first request", counter, 3);
		sqlCounter.bindContext("sql", "sql");
		htmlReport = new HtmlReport(collector, null, javaInformationsList, Period.TOUT, writer);
		htmlReport.toHtml("message a", null);
		assertNotEmptyAndClear(writer);

		final Counter myCounter = new Counter("http", null);
		final Collector collector2 = new Collector("test 2", Arrays.asList(myCounter), timer);
		myCounter.bindContext("my context", "my context");
		htmlReport = new HtmlReport(collector2, null, javaInformationsList, Period.SEMAINE, writer);
		htmlReport.toHtml("message b", null);
		assertNotEmptyAndClear(writer);

		final HtmlCounterRequestContextReport htmlCounterRequestContextReport = new HtmlCounterRequestContextReport(
				collector2.getRootCurrentContexts(), null, new ArrayList<ThreadInformations>(),
				false, 500, writer);
		htmlCounterRequestContextReport.toHtml();
		assertNotEmptyAndClear(writer);
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testCache() throws IOException {
		final String cacheName = "test 1";
		final CacheManager cacheManager = CacheManager.getInstance();
		cacheManager.addCache(cacheName);
		final String cacheName2 = "test 2";
		try {
			final Cache cache = cacheManager.getCache(cacheName);
			cache.put(new Element(1, Math.random()));
			cache.get(1);
			cache.get(0);
			cacheManager.addCache(cacheName2);
			final Cache cache2 = cacheManager.getCache(cacheName2);
			cache2.getCacheConfiguration().setOverflowToDisk(false);
			cache2.getCacheConfiguration().setEternal(true);

			// JavaInformations doit être réinstancié pour récupérer les caches
			final List<JavaInformations> javaInformationsList2 = Collections
					.singletonList(new JavaInformations(null, true));
			final HtmlReport htmlReport = new HtmlReport(collector, null, javaInformationsList2,
					Period.TOUT, writer);
			htmlReport.toHtml(null, null);
			assertNotEmptyAndClear(writer);
			setProperty(Parameter.SYSTEM_ACTIONS_ENABLED, "true");
			htmlReport.toHtml(null, null);
			assertNotEmptyAndClear(writer);
		} finally {
			setProperty(Parameter.SYSTEM_ACTIONS_ENABLED, null);
			cacheManager.removeCache(cacheName);
			cacheManager.removeCache(cacheName2);
		}
	}

	/** Test.
	 * @throws IOException e
	 * @throws SchedulerException e */
	@Test
	public void testJob() throws IOException, SchedulerException {
		// job quartz
		JobGlobalListener.initJobGlobalListener();
		JobGlobalListener.getJobCounter().clear();

		//Grab the Scheduler instance from the Factory
		final Scheduler scheduler = StdSchedulerFactory.getDefaultScheduler();

		try {
			// and start it off
			scheduler.start();

			final Random random = new Random();

			//Define a Trigger that will fire "later"
			final JobDetail job2 = new JobDetail("job" + random.nextInt(), null, JobTestImpl.class);
			final SimpleTrigger trigger2 = new SimpleTrigger("trigger" + random.nextInt(), null,
					new Date(System.currentTimeMillis() + 60000));
			trigger2.setRepeatInterval(2 * 24L * 60 * 60 * 1000);
			scheduler.scheduleJob(job2, trigger2);
			scheduler.pauseJob(job2.getName(), job2.getGroup());
			try {
				final JobDetail job3 = new JobDetail("job" + random.nextInt(), null,
						JobTestImpl.class);
				final Trigger trigger3 = new CronTrigger("trigger" + random.nextInt(), null,
						"0 0 0 * * ? 2030");
				scheduler.scheduleJob(job3, trigger3);
			} catch (final ParseException e) {
				throw new IllegalStateException(e);
			}

			// JavaInformations doit être réinstancié pour récupérer les jobs
			// (mais "Aucun job" dans le counter)
			final List<JavaInformations> javaInformationsList2 = Collections
					.singletonList(new JavaInformations(null, true));
			final HtmlReport htmlReport = new HtmlReport(collector, null, javaInformationsList2,
					Period.TOUT, writer);
			htmlReport.toHtml(null, null);
			assertNotEmptyAndClear(writer);

			// on lance 10 jobs pour être à peu près sûr qu'il y en a un qui fait une erreur
			// (aléatoirement il y en a 2/10 qui font une erreur)
			final Map<JobDetail, SimpleTrigger> triggersByJob = new LinkedHashMap<JobDetail, SimpleTrigger>();
			for (int i = 0; i < 10; i++) {
				//Define a Trigger that will fire "now"
				final JobDetail job = new JobDetail("job" + random.nextInt(), null,
						JobTestImpl.class);
				job.setDescription("description");

				final SimpleTrigger trigger = new SimpleTrigger("trigger" + random.nextInt(), null,
						new Date());
				//Schedule the job with the trigger
				scheduler.scheduleJob(job, trigger);
				triggersByJob.put(job, trigger);
			}
			// JobTestImpl fait un sleep de 2s au plus, donc on attend les jobs pour les compter
			try {
				Thread.sleep(3000);
			} catch (final InterruptedException e) {
				throw new IllegalStateException(e);
			}

			for (final Map.Entry<JobDetail, SimpleTrigger> entry : triggersByJob.entrySet()) {
				// et on les relance pour qu'ils soient en cours
				entry.getValue().setRepeatInterval(60000);
				scheduler.scheduleJob(entry.getKey(), entry.getValue());
			}

			// JavaInformations doit être réinstancié pour récupérer les jobs
			setProperty(Parameter.SYSTEM_ACTIONS_ENABLED, Boolean.TRUE.toString());
			final List<JavaInformations> javaInformationsList3 = Collections
					.singletonList(new JavaInformations(null, true));
			final HtmlReport htmlReport3 = new HtmlReport(collector, null, javaInformationsList3,
					Period.TOUT, writer);
			htmlReport3.toHtml(null, null);
			assertNotEmptyAndClear(writer);
		} finally {
			setProperty(Parameter.SYSTEM_ACTIONS_ENABLED, Boolean.FALSE.toString());
			scheduler.shutdown();
			JobGlobalListener.getJobCounter().clear();
			JobGlobalListener.destroyJobGlobalListener();
		}
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testWithCollectorServer() throws IOException {
		final CollectorServer collectorServer = new CollectorServer();
		try {
			final HtmlReport htmlReport = new HtmlReport(collector, collectorServer,
					javaInformationsList, Period.TOUT, writer);
			htmlReport.toHtml(null, null);
			assertNotEmptyAndClear(writer);

			Utils.setProperty(Parameters.PARAMETER_SYSTEM_PREFIX + "mockLabradorRetriever", "true");
			collectorServer.collectWithoutErrors();
			htmlReport.toHtml(null, null);
			assertNotEmptyAndClear(writer);
		} finally {
			collectorServer.stop();
		}
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testWithNoDatabase() throws IOException {
		final HtmlReport htmlReport = new HtmlReport(collector, null, javaInformationsList,
				Period.TOUT, writer);
		htmlReport.toHtml(null, null);
		assertNotEmptyAndClear(writer);
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testHtmlCounterRequestContext() throws IOException {
		// cas où counterReportsByCounterName est null
		assertNotNull(
				"HtmlCounterRequestContextReport",
				new HtmlCounterRequestContextReport(
						Collections.<CounterRequestContext> emptyList(), null, Collections
								.<ThreadInformations> emptyList(), true, 500, writer));

		// aucune requête en cours
		final HtmlCounterRequestContextReport report = new HtmlCounterRequestContextReport(
				Collections.<CounterRequestContext> emptyList(),
				Collections.<String, HtmlCounterReport> emptyMap(),
				Collections.<ThreadInformations> emptyList(), true, 500, writer);
		report.toHtml();
		assertNotEmptyAndClear(writer);

		// cas où nb requêtes en cours > maxContextDisplayed
		final List<CounterRequestContext> counterRequestContexts = Collections
				.singletonList(new CounterRequestContext(sqlCounter, null, "Test", "Test", null, -1));
		final HtmlCounterRequestContextReport report2 = new HtmlCounterRequestContextReport(
				counterRequestContexts, null, Collections.<ThreadInformations> emptyList(), true,
				0, writer);
		report2.toHtml();
		assertNotEmptyAndClear(writer);

		// writeTitleAndDetails
		report2.writeTitleAndDetails();
	}

	private static void setProperty(Parameter parameter, String value) {
		Utils.setProperty(parameter, value);
	}

	private static void assertNotEmptyAndClear(final StringWriter writer) {
		assertTrue("rapport vide", writer.getBuffer().length() > 0);
		writer.getBuffer().setLength(0);
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testToHtmlEn() throws IOException {
		I18N.bindLocale(Locale.US);
		Locale.setDefault(Locale.US);
		try {
			assertEquals("locale en", Locale.US, I18N.getCurrentLocale());

			// counter avec 3 requêtes
			counter.addRequest("test1", 0, 0, false, 1000);
			counter.addRequest("test2", 1000, 500, false, 1000);
			counter.addRequest("test3", 10000, 5000, true, 10000);
			final HtmlReport htmlReport = new HtmlReport(collector, null, javaInformationsList,
					Period.TOUT, writer);
			htmlReport.toHtml("message", null);
			assertNotEmptyAndClear(writer);
		} finally {
			I18N.unbindLocale();
			Locale.setDefault(Locale.FRENCH);
		}
	}
}
