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

import static net.bull.javamelody.HttpParameters.SESSIONS_PART;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.io.StringWriter;
import java.sql.Connection;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.Random;
import java.util.Timer;

import net.sf.ehcache.Cache;
import net.sf.ehcache.CacheManager;
import net.sf.ehcache.Element;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
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
public class TestHtmlReport {
	private Timer timer;
	private List<JavaInformations> javaInformationsList;
	private Counter sqlCounter;
	private Counter servicesCounter;
	private Counter counter;
	private Counter errorCounter;
	private Collector collector;
	private StringWriter writer;

	/** Initialisation. */
	@Before
	public void setUp() {
		timer = new Timer("test timer", true);
		javaInformationsList = Collections.singletonList(new JavaInformations(null, true));
		sqlCounter = new Counter("sql", "db.png");
		sqlCounter.setDisplayed(false);
		servicesCounter = new Counter("services", "beans.png", sqlCounter);
		// counterName doit être http, sql ou ejb pour que les libellés de graph soient trouvés dans les traductions
		counter = new Counter("http", "dbweb.png", sqlCounter);
		errorCounter = new Counter(Counter.ERROR_COUNTER_NAME, null);
		final Counter jobCounter = new Counter(Counter.JOB_COUNTER_NAME, "jobs.png");
		collector = new Collector("test", Arrays.asList(counter, sqlCounter, servicesCounter,
				errorCounter, jobCounter), timer);
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
		htmlReport.toHtml(null);
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
		htmlReport.toHtml(null);
		assertNotEmptyAndClear(writer);
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
		htmlReport.toHtml("message 2");
		assertNotEmptyAndClear(writer);

		setProperty(Parameter.NO_DATABASE, "true");
		collector.collectWithoutErrors(javaInformationsList);
		htmlReport.toHtml("message 2");
		assertNotEmptyAndClear(writer);
		setProperty(Parameter.NO_DATABASE, "false");
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
		htmlReport.toHtml("message 3");
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
		htmlReport.toHtml("message 6");
		assertNotEmptyAndClear(writer);
	}

	/** Test.
	 * @throws Exception e */
	@Test
	public void testAllWrite() throws Exception { // NOPMD
		final HtmlReport htmlReport = new HtmlReport(collector, null, javaInformationsList,
				Period.SEMAINE, writer);
		htmlReport.writeRequestAndGraphDetail("httpHitsRate");

		// writeRequestAndGraphDetail avec drill-down
		collector.collectWithoutErrors(javaInformationsList);
		// si sqlCounter reste à displayed=false,
		// il ne sera pas utilisé dans writeRequestAndGraphDetail
		sqlCounter.setDisplayed(true);
		final String requestName = "test 1";
		counter.bindContext(requestName, "complete test 1");
		servicesCounter.bindContext("service1", "service1");
		sqlCounter.bindContext("sql1", "complete sql1");
		sqlCounter.addRequest("sql1", 5, -1, false, -1);
		servicesCounter.addRequest("service1", 10, 10, false, -1);
		servicesCounter.bindContext("service2", "service2");
		servicesCounter.addRequest("service2", 10, 10, false, -1);
		counter.addRequest(requestName, 0, 0, false, 1000);
		collector.collectWithoutErrors(javaInformationsList);
		final HtmlReport toutHtmlReport = new HtmlReport(collector, null, javaInformationsList,
				Period.TOUT, writer);
		for (final Counter collectorCounter : collector.getCounters()) {
			for (final CounterRequest request : collectorCounter.getRequests()) {
				toutHtmlReport.writeRequestAndGraphDetail(request.getId());
				toutHtmlReport.writeRequestUsages(request.getId());
			}
		}
		sqlCounter.setDisplayed(false);

		htmlReport.writeSessionDetail("", null);
		htmlReport.writeSessions(Collections.<SessionInformations> emptyList(), "message",
				SESSIONS_PART);
		htmlReport
				.writeSessions(Collections.<SessionInformations> emptyList(), null, SESSIONS_PART);
		final String fileName = ProcessInformations.WINDOWS ? "/tasklist.txt" : "/ps.txt";
		htmlReport.writeProcesses(ProcessInformations.buildProcessInformations(getClass()
				.getResourceAsStream(fileName), ProcessInformations.WINDOWS));
		// avant initH2 pour avoir une liste de connexions vide
		htmlReport.writeConnections(JdbcWrapper.getConnectionInformationsList(), false);
		final Connection connection = TestDatabaseInformations.initH2();
		try {
			htmlReport.writeConnections(JdbcWrapper.getConnectionInformationsList(), false);
			htmlReport.writeConnections(JdbcWrapper.getConnectionInformationsList(), true);
			htmlReport.writeDatabase(new DatabaseInformations(0));
			HtmlReport.writeAddAndRemoveApplicationLinks(null, writer);
			HtmlReport.writeAddAndRemoveApplicationLinks("test", writer);
			setProperty(Parameter.SYSTEM_ACTIONS_ENABLED, "true");
			htmlReport.toHtml(null); // writeSystemActionsLinks
			assertNotEmptyAndClear(writer);
			setProperty(Parameter.NO_DATABASE, "true");
			htmlReport.toHtml(null); // writeSystemActionsLinks
			assertNotEmptyAndClear(writer);
			setProperty(Parameter.SYSTEM_ACTIONS_ENABLED, "false");
			setProperty(Parameter.NO_DATABASE, "false");
		} finally {
			connection.close();
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
		htmlReport.toHtml("message a");
		assertNotEmptyAndClear(writer);

		final Counter myCounter = new Counter("http", null);
		final Collector collector2 = new Collector("test 2", Arrays.asList(myCounter), timer);
		myCounter.bindContext("my context", "my context");
		htmlReport = new HtmlReport(collector2, null, javaInformationsList, Period.SEMAINE, writer);
		htmlReport.toHtml("message b");
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
			htmlReport.toHtml(null);
			assertNotEmptyAndClear(writer);
		} finally {
			cacheManager.removeCache(cacheName);
			cacheManager.removeCache(cacheName2);
		}
	}

	/** Test.
	 * @throws IOException e
	 * @throws SchedulerException e */
	@Test
	public void testJob() throws IOException, SchedulerException {
		//Grab the Scheduler instance from the Factory
		final Scheduler scheduler = StdSchedulerFactory.getDefaultScheduler();

		try {
			// and start it off
			scheduler.start();

			//Define job instance
			final Random random = new Random();
			final JobDetail job = new JobDetail("job" + random.nextInt(), null, JobTestImpl.class);

			//Define a Trigger that will fire "now"
			final Trigger trigger = new SimpleTrigger("trigger" + random.nextInt(), null,
					new Date());
			//Schedule the job with the trigger
			scheduler.scheduleJob(job, trigger);

			//Define a Trigger that will fire "later"
			final JobDetail job2 = new JobDetail("job" + random.nextInt(), null, JobTestImpl.class);
			final Trigger trigger2 = new SimpleTrigger("trigger" + random.nextInt(), null,
					new Date(System.currentTimeMillis() + random.nextInt(60000)));
			scheduler.scheduleJob(job2, trigger2);

			// JavaInformations doit être réinstancié pour récupérer les jobs
			final List<JavaInformations> javaInformationsList2 = Collections
					.singletonList(new JavaInformations(null, true));
			final HtmlReport htmlReport = new HtmlReport(collector, null, javaInformationsList2,
					Period.TOUT, writer);
			htmlReport.toHtml(null);
			assertNotEmptyAndClear(writer);
		} finally {
			scheduler.shutdown();
		}
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testWithCollectorServer() throws IOException {
		final CollectorServer collectorServer = new CollectorServer();
		final HtmlReport htmlReport = new HtmlReport(collector, collectorServer,
				javaInformationsList, Period.TOUT, writer);
		htmlReport.toHtml(null);
		assertNotEmptyAndClear(writer);

		collectorServer.collectWithoutErrors();
		htmlReport.toHtml(null);
		assertNotEmptyAndClear(writer);
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testWithNoDatabase() throws IOException {
		final HtmlReport htmlReport = new HtmlReport(collector, null, javaInformationsList,
				Period.TOUT, writer);
		htmlReport.toHtml(null);
		assertNotEmptyAndClear(writer);
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testEmptyHtmlCounterRequestContext() throws IOException {
		final HtmlCounterRequestContextReport report = new HtmlCounterRequestContextReport(
				Collections.<CounterRequestContext> emptyList(), Collections
						.<String, HtmlCounterReport> emptyMap(), Collections
						.<ThreadInformations> emptyList(), true, writer);
		report.toHtml();
		if (writer.getBuffer().length() != 0) {
			fail("HtmlCounterRequestContextReport");
		}
	}

	private static void setProperty(Parameter parameter, String value) {
		System.setProperty(Parameters.PARAMETER_SYSTEM_PREFIX + parameter.getCode(), value);
	}

	private static void assertNotEmptyAndClear(final StringWriter writer) {
		assertTrue("rapport vide", writer.getBuffer().length() > 0);
		writer.getBuffer().setLength(0);
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testToHtmlEn() throws IOException {
		I18N.bindLocale(Locale.UK);
		try {
			assertEquals("locale en", Locale.UK, I18N.getCurrentLocale());

			// counter avec 3 requêtes
			counter.addRequest("test1", 0, 0, false, 1000);
			counter.addRequest("test2", 1000, 500, false, 1000);
			counter.addRequest("test3", 10000, 5000, true, 10000);
			final HtmlReport htmlReport = new HtmlReport(collector, null, javaInformationsList,
					Period.TOUT, writer);
			htmlReport.toHtml("message");
			assertNotEmptyAndClear(writer);
		} finally {
			I18N.unbindLocale();
		}
	}
}
