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
package net.bull.javamelody.internal.web.pdf; // NOPMD

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Random;
import java.util.Timer;

import javax.cache.Caching;
import javax.cache.configuration.MutableConfiguration;

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

import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import com.lowagie.text.PageSize;

import net.bull.javamelody.JobTestImpl;
import net.bull.javamelody.Parameter;
import net.bull.javamelody.Utils;
import net.bull.javamelody.internal.common.I18N;
import net.bull.javamelody.internal.common.Parameters;
import net.bull.javamelody.internal.model.Collector;
import net.bull.javamelody.internal.model.Counter;
import net.bull.javamelody.internal.model.CounterRequestContext;
import net.bull.javamelody.internal.model.JRobin;
import net.bull.javamelody.internal.model.JavaInformations;
import net.bull.javamelody.internal.model.Period;
import net.bull.javamelody.internal.model.TestCounter;
import net.bull.javamelody.internal.model.ThreadInformations;
import net.bull.javamelody.internal.web.html.TestHtmlReport;
import net.sf.ehcache.CacheManager;
import net.sf.ehcache.Element;

/**
 * Test unitaire de la classe PdfReport.
 * @author Emeric Vernat
 */
//CHECKSTYLE:OFF
public class TestPdfReport {
	//CHECKSTYLE:ON
	private static final String TEST_APP = "test app";

	/** Before.
	 * @throws IOException e */
	@Before
	public void setUp() throws IOException {
		Utils.initialize();
		JRobin.initBackendFactory(new Timer(getClass().getSimpleName(), true));
	}

	/** After. */
	@After
	public void tearDown() {
		JRobin.stop();
	}

	private void toPdf(Collector collector, boolean collectorServer,
			List<JavaInformations> javaInformationsList, Map<String, byte[]> graphs)
			throws IOException {
		final ByteArrayOutputStream output = new ByteArrayOutputStream();
		final PdfReport pdfReport = new PdfReport(collector, collectorServer, javaInformationsList,
				Period.TOUT, output);
		if (graphs != null) {
			pdfReport.preInitGraphs(graphs, graphs, graphs);
		}
		pdfReport.toPdf();
		pdfReport.close();
		assertNotEmptyAndClear(output);
	}

	/** Test.
	 * @throws Exception e */
	@Test
	public void testToPdf() throws Exception {
		final Counter sqlCounter = new Counter("sql", "db.png");
		// counterName doit être http, sql ou ejb pour que les libellés de graph soient trouvés dans les traductions
		final Counter counter = new Counter("http", "db.png", sqlCounter);
		final Counter errorCounter = new Counter(Counter.ERROR_COUNTER_NAME, null);
		final Counter jobCounter = TestHtmlReport.getJobCounter();
		final List<Counter> counters = Arrays.asList(counter, sqlCounter, errorCounter, jobCounter);
		counter.addRequest("test1", 0, 0, 0, false, 1000);
		counter.addRequest("test2", 1000, 500, 500, false, 1000);
		counter.addRequest("test3", 10000, 500, 500, true, 10000);
		final Collector collector = new Collector("test", counters);
		final JavaInformations javaInformations = new JavaInformations(null, true);
		final List<JavaInformations> javaInformationsList = Collections
				.singletonList(javaInformations);
		counter.addRequest("test1", 0, 0, 0, false, 1000);
		collector.collectWithoutErrors(javaInformationsList);
		counter.clear();
		collector.collectWithoutErrors(javaInformationsList);
		toPdf(collector, true, javaInformationsList, null);

		final JRobin jrobin = collector.getCounterJRobins().iterator().next();
		final byte[] graph = jrobin.graph(Period.JOUR.getRange(), 50, 50);
		final Map<String, byte[]> graphs = new HashMap<String, byte[]>();
		graphs.put("1", graph);
		graphs.put("2", graph);
		graphs.put("3", graph);
		graphs.put("4", graph);

		toPdf(collector, true, javaInformationsList, graphs);

		// pour les PDFs suivants, inutile de regénérer toutes les images,
		// ce qui prendrait beaucoup de temps, donc on utilise preInitGraphs
		final Map<String, byte[]> emptyGraphs = Collections.emptyMap();
		counter.bindContext("test 1", "complete test 1", null, -1, -1);
		sqlCounter.bindContext("sql1", "sql 1", null, -1, -1);
		sqlCounter.addRequest("sql1", 100, 100, 100, false, -1);
		counter.addRequest("test 1", 0, 0, 0, false, 1000);
		counter.addRequest("test2", 1000, 500, 500, false, 1000);
		counter.addRequest(buildLongRequestName(), 10000, 5000, 5000, true, 10000);
		collector.collectWithoutErrors(javaInformationsList);
		toPdf(collector, true, javaInformationsList, emptyGraphs);

		toPdf(collector, false, javaInformationsList, emptyGraphs);

		// errorCounter
		errorCounter.addRequestForSystemError("error", -1, -1, -1, null);
		errorCounter.addRequestForSystemError("error2", -1, -1, -1, "ma stack-trace");
		toPdf(collector, false, javaInformationsList, emptyGraphs);

		rootContexts(counter, collector, javaInformations);

		cache(collector);

		job(collector);

		Utils.setProperty(Parameter.NO_DATABASE, Boolean.TRUE.toString());
		toPdf(collector, false, javaInformationsList, emptyGraphs);
		Utils.setProperty(Parameter.NO_DATABASE, Boolean.FALSE.toString());

		I18N.bindLocale(Locale.CHINA);
		try {
			toPdf(collector, false, javaInformationsList, emptyGraphs);
		} finally {
			I18N.unbindLocale();
		}
	}

	private void cache(Collector collector) throws IOException {
		final String cacheName = "testcache";
		final CacheManager cacheManager = CacheManager.getInstance();
		cacheManager.addCache(cacheName);
		try {
			cacheManager.getCache(cacheName).put(new Element(1, Math.random()));
			cacheManager.getCache(cacheName).get(1);
			cacheManager.getCache(cacheName).get(0);
			cacheManager.addCache("testcache2");
			// JavaInformations doit être réinstancié pour récupérer les caches
			final List<JavaInformations> javaInformationsList = Collections
					.singletonList(new JavaInformations(null, true));
			final Map<String, byte[]> graphs = Collections.emptyMap();
			toPdf(collector, false, javaInformationsList, graphs);
		} finally {
			cacheManager.removeCache(cacheName);
			cacheManager.removeCache("testcache2");
		}

		final javax.cache.CacheManager jcacheManager = Caching.getCachingProvider()
				.getCacheManager();
		final MutableConfiguration<Object, Object> conf = new MutableConfiguration<Object, Object>();
		conf.setManagementEnabled(true);
		conf.setStatisticsEnabled(true);
		jcacheManager.createCache(cacheName, conf);
		try {
			jcacheManager.getCache(cacheName).put(1, Math.random());
			jcacheManager.getCache(cacheName).get(1);
			jcacheManager.getCache(cacheName).get(0);
			jcacheManager.createCache("testcache2", conf);
			// JavaInformations doit être réinstancié pour récupérer les caches
			final List<JavaInformations> javaInformationsList = Collections
					.singletonList(new JavaInformations(null, true));
			final Map<String, byte[]> graphs = Collections.emptyMap();
			toPdf(collector, false, javaInformationsList, graphs);
		} finally {
			jcacheManager.destroyCache(cacheName);
			jcacheManager.destroyCache("testcache2");
		}
	}

	private void job(Collector collector) throws IOException, SchedulerException {
		// job quartz
		TestHtmlReport.initJobGlobalListener();
		TestHtmlReport.getJobCounter().clear();

		//Grab the Scheduler instance from the Factory
		final Scheduler scheduler = StdSchedulerFactory.getDefaultScheduler();

		try {
			// and start it off
			scheduler.start();

			//Define job instance
			final Random random = new Random();

			//Define a Trigger that will fire "later"
			final JobDetail job2 = new JobDetail("job" + random.nextInt(), null, JobTestImpl.class);
			final SimpleTrigger trigger2 = new SimpleTrigger("trigger" + random.nextInt(), null,
					new Date(System.currentTimeMillis() + random.nextInt(60000)));
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
			final List<JavaInformations> javaInformationsList = Collections
					.singletonList(new JavaInformations(null, true));
			final Map<String, byte[]> graphs = Collections.emptyMap();
			toPdf(collector, false, javaInformationsList, graphs);

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
			final List<JavaInformations> javaInformationsList2 = Collections
					.singletonList(new JavaInformations(null, true));
			toPdf(collector, false, javaInformationsList2, graphs);
		} finally {
			scheduler.shutdown();
			TestHtmlReport.getJobCounter().clear();
			TestHtmlReport.destroyJobGlobalListener();
		}
	}

	private void rootContexts(Counter counter, Collector collector,
			JavaInformations javaInformations) throws IOException, DocumentException {
		TestCounter.bindRootContexts("first request", counter, 3);
		final Map<String, byte[]> graphs = Collections.emptyMap();
		toPdf(collector, false, Collections.singletonList(javaInformations), graphs);

		final Counter myCounter = new Counter("http", null);
		final Collector collector2 = new Collector("test 2", Arrays.asList(myCounter));
		myCounter.bindContext("my context", "my context", null, -1, -1);
		toPdf(collector2, false, Collections.singletonList(javaInformations), graphs);

		final ByteArrayOutputStream output = new ByteArrayOutputStream();
		final PdfDocumentFactory pdfDocumentFactory = new PdfDocumentFactory(
				collector.getApplication(), null, output);
		final Document document = pdfDocumentFactory.createDocument();
		document.open();
		final PdfCounterRequestContextReport pdfCounterRequestContextReport = new PdfCounterRequestContextReport(
				collector2.getRootCurrentContexts(collector2.getCounters()),
				new ArrayList<PdfCounterReport>(), new ArrayList<ThreadInformations>(), false,
				pdfDocumentFactory, document);
		pdfCounterRequestContextReport.toPdf();
		document.close();
		assertNotEmptyAndClear(output);
	}

	private void assertNotEmptyAndClear(ByteArrayOutputStream output) {
		assertTrue("rapport vide", output.size() > 0);
		output.reset();
	}

	private static String buildLongRequestName() {
		// plus de 1000 caractères
		final StringBuilder sb = new StringBuilder(1100);
		for (int i = 0; i < 1100 / 2; i++) {
			sb.append("a ");
		}
		return sb.toString();
	}

	/** Test.
	 * @throws IOException e
	 * @throws DocumentException e */
	@Test
	public void testPdfCounterReportWithIncludeGraph() throws IOException, DocumentException {
		// counterName doit être http, sql ou ejb pour que les libellés de graph soient trouvés dans les traductions
		final Counter counter = new Counter("http", "db.png");
		final Counter errorCounter = new Counter(Counter.ERROR_COUNTER_NAME, null);
		final List<Counter> counters = Arrays.asList(counter, errorCounter);
		final Collector collector = new Collector(TEST_APP, counters);
		final JavaInformations javaInformations = new JavaInformations(null, true);
		final ByteArrayOutputStream output = new ByteArrayOutputStream();
		final List<JavaInformations> javaInformationsList = Collections
				.singletonList(javaInformations);
		counter.addRequest("test include graph", 1, 1, 1, false, 1000);
		errorCounter.addRequestForSystemError("error", 1, 1, 1, null);
		collector.collectWithoutErrors(javaInformationsList);
		counter.addRequest("test include graph", 1, 1, 1, false, 1000);
		errorCounter.addRequestForSystemError("error", 1, 1, 1, null);
		collector.collectWithoutErrors(javaInformationsList);
		final Document document = new PdfDocumentFactory(TEST_APP, null, output).createDocument();
		document.open();
		final PdfCounterReport pdfCounterReport = new PdfCounterReport(collector, counter,
				Period.TOUT.getRange(), true, document);
		pdfCounterReport.toPdf();
		pdfCounterReport.writeRequestDetails();
		final PdfCounterReport pdfErrorCounterReport = new PdfCounterReport(collector, errorCounter,
				Period.TOUT.getRange(), true, document);
		pdfErrorCounterReport.writeRequestDetails();
		document.close();
		assertNotEmptyAndClear(output);
	}

	/** Test.
	 * @throws IOException e
	 * @throws DocumentException e */
	@Test
	public void testEmptyPdfCounterRequestContext() throws IOException, DocumentException {
		final ByteArrayOutputStream output = new ByteArrayOutputStream();
		final PdfDocumentFactory pdfDocumentFactory = new PdfDocumentFactory(TEST_APP, null,
				output);
		final Document document = pdfDocumentFactory.createDocument();
		document.open();
		final PdfCounterRequestContextReport report = new PdfCounterRequestContextReport(
				Collections.<CounterRequestContext> emptyList(),
				Collections.<PdfCounterReport> emptyList(),
				Collections.<ThreadInformations> emptyList(), true, pdfDocumentFactory, document);
		report.toPdf();
		report.setTimeOfSnapshot(System.currentTimeMillis());
		report.writeContextDetails();
		// on ne peut fermer le document car on n'a rien écrit normalement
		assertNotNull("PdfCounterRequestContextReport", report);
	}

	/** Test.
	 * @throws IOException e
	 * @throws DocumentException e */
	@Test
	public void testPdfThreadInformationsReport() throws IOException, DocumentException {
		final ByteArrayOutputStream output = new ByteArrayOutputStream();
		final PdfDocumentFactory pdfDocumentFactory = new PdfDocumentFactory(TEST_APP, null,
				output);
		final Document document = pdfDocumentFactory.createDocument();
		document.open();
		boolean stackTraceEnabled = true;
		final PdfThreadInformationsReport report = new PdfThreadInformationsReport(
				JavaInformations.buildThreadInformationsList(), stackTraceEnabled,
				pdfDocumentFactory, document);
		report.writeIntro(new JavaInformations(null, true));
		report.toPdf();
		document.close();
		assertNotEmptyAndClear(output);

		final Document document2 = pdfDocumentFactory.createDocument();
		document2.open();
		stackTraceEnabled = false;
		final PdfThreadInformationsReport report2 = new PdfThreadInformationsReport(
				JavaInformations.buildThreadInformationsList(), stackTraceEnabled,
				pdfDocumentFactory, document2);
		report2.toPdf();
		document2.close();
		assertNotEmptyAndClear(output);

		// writeDeadlocks
		final List<ThreadInformations> threads = new ArrayList<ThreadInformations>();
		final Thread thread = Thread.currentThread();
		threads.add(
				new ThreadInformations(thread, null, 10, 10, false, Parameters.getHostAddress()));
		threads.add(
				new ThreadInformations(thread, null, 10, 10, true, Parameters.getHostAddress()));
		final Document document3 = pdfDocumentFactory.createDocument();
		document3.open();
		stackTraceEnabled = false;
		final PdfThreadInformationsReport report3 = new PdfThreadInformationsReport(threads,
				stackTraceEnabled, pdfDocumentFactory, document3);
		report3.writeDeadlocks();
		document3.close();
		assertNotEmptyAndClear(output);

		final Document document4 = pdfDocumentFactory.createDocument();
		document4.open();
		final PdfThreadInformationsReport report4 = new PdfThreadInformationsReport(
				new ArrayList<ThreadInformations>(), stackTraceEnabled, pdfDocumentFactory,
				document4);
		report4.toPdf();
		report4.writeDeadlocks();
		document4.close();
		assertNotEmptyAndClear(output);
	}

	/** Test. */
	@Test
	public void testGetFileName() {
		assertNotNull("filename", PdfReport.getFileName("test"));
	}

	@Test
	public void testSetters() throws Exception {
		final Counter errorCounter = new Counter(Counter.ERROR_COUNTER_NAME, null);
		final List<Counter> counters = Arrays.asList(errorCounter);
		final Collector collector = new Collector("test", counters);
		final JavaInformations javaInformations = new JavaInformations(null, true);
		final List<JavaInformations> javaInformationsList = Collections
				.singletonList(javaInformations);
		final Period period = Period.TOUT;
		final ByteArrayOutputStream output = new ByteArrayOutputStream();
		final List<CounterRequestContext> currentRequests = Collections.emptyList();

		final PdfReport pdfReport = new PdfReport(collector, false, javaInformationsList, period,
				output);
		pdfReport.setCounterRange(Period.TOUT.getRange());
		pdfReport.setCurrentRequests(currentRequests);
		pdfReport.toPdf();
	}

	@Test
	public void testUsPageSize() throws DocumentException, IOException {
		I18N.bindLocale(Locale.US);
		try {
			final ByteArrayOutputStream output = new ByteArrayOutputStream();
			final PdfDocumentFactory pdfDocumentFactory = new PdfDocumentFactory(TEST_APP, null,
					output);
			final Document document = pdfDocumentFactory.createDocument();
			assertEquals("pageSize", document.getPageSize(), PageSize.LETTER);
		} finally {
			I18N.unbindLocale();
		}
	}
}
