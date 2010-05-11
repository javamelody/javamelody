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

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Random;
import java.util.Timer;

import net.sf.ehcache.CacheManager;
import net.sf.ehcache.Element;

import org.junit.Test;
import org.quartz.JobDetail;
import org.quartz.Scheduler;
import org.quartz.SchedulerException;
import org.quartz.SimpleTrigger;
import org.quartz.Trigger;
import org.quartz.impl.StdSchedulerFactory;

import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;

/**
 * Test unitaire de la classe PdfReport.
 * @author Emeric Vernat
 */
//CHECKSTYLE:OFF
public class TestPdfReport {
	private static final String TEST_APP = "test app";

	//CHECKSTYLE:ON
	/** Test.
	 * @throws IOException e
	 * @throws SchedulerException e */
	@Test
	public void testToPdf() throws IOException, SchedulerException {
		final Counter sqlCounter = new Counter("sql", "db.png");
		sqlCounter.setDisplayed(false);
		// counterName doit être http, sql ou ejb pour que les libellés de graph soient trouvés dans les traductions
		final Counter counter = new Counter("http", "db.png", sqlCounter);
		final Counter errorCounter = new Counter(Counter.ERROR_COUNTER_NAME, null);
		final Counter jobCounter = JobGlobalListener.getJobCounter();
		final List<Counter> counters = new ArrayList<Counter>();
		counters.add(counter);
		counters.add(sqlCounter);
		counters.add(errorCounter);
		counters.add(jobCounter);
		counter.addRequest("test1", 0, 0, false, 1000);
		counter.addRequest("test2", 1000, 500, false, 1000);
		counter.addRequest("test3", 10000, 500, true, 10000);
		final Timer timer = new Timer("test", true);
		try {
			final Collector collector = new Collector("test", counters, timer);
			final JavaInformations javaInformations = new JavaInformations(null, true);
			final ByteArrayOutputStream output = new ByteArrayOutputStream();
			final List<JavaInformations> javaInformationsList = Collections
					.singletonList(javaInformations);
			counter.addRequest("test1", 0, 0, false, 1000);
			collector.collectWithoutErrors(javaInformationsList);
			counter.clear();
			collector.collectWithoutErrors(javaInformationsList);
			PdfReport pdfReport = new PdfReport(collector, true, javaInformationsList, Period.TOUT,
					output);
			pdfReport.toPdf();
			assertNotEmptyAndClear(output);

			counter.bindContext("test 1", "complete test 1");
			sqlCounter.addRequest("sql1", 100, 100, false, -1);
			counter.addRequest("test 1", 0, 0, false, 1000);
			counter.addRequest("test2", 1000, 500, false, 1000);
			counter.addRequest(buildLongRequestName(), 10000, 5000, true, 10000);
			collector.collectWithoutErrors(javaInformationsList);
			pdfReport = new PdfReport(collector, true, javaInformationsList, Period.TOUT, output);
			pdfReport.toPdf();
			assertNotEmptyAndClear(output);

			pdfReport = new PdfReport(collector, false, javaInformationsList, Period.TOUT, output);
			pdfReport.toPdf();
			assertNotEmptyAndClear(output);

			// errorCounter
			errorCounter.addRequestForSystemError("error", -1, -1, null);
			errorCounter.addRequestForSystemError("error2", -1, -1, "ma stack-trace");
			pdfReport = new PdfReport(collector, false, javaInformationsList, Period.TOUT, output);
			pdfReport.toPdf();
			assertNotEmptyAndClear(output);

			rootContexts(counter, collector, timer, javaInformations, output);

			cache(collector, output);

			job(collector, output);
		} finally {
			timer.cancel();
		}
	}

	private void cache(Collector collector, ByteArrayOutputStream output) throws IOException {
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
			final PdfReport pdfReport = new PdfReport(collector, false, javaInformationsList,
					Period.TOUT, output);
			pdfReport.toPdf();
			assertNotEmptyAndClear(output);
		} finally {
			cacheManager.removeCache(cacheName);
			cacheManager.removeCache("testcache2");
		}
	}

	private void job(Collector collector, ByteArrayOutputStream output) throws IOException,
			SchedulerException {
		// job quartz
		JobGlobalListener.initJobGlobalListener();
		JobGlobalListener.getJobCounter().clear();

		//Grab the Scheduler instance from the Factory
		final Scheduler scheduler = StdSchedulerFactory.getDefaultScheduler();

		try {
			// and start it off
			scheduler.start();

			//Define job instance
			final Random random = new Random();

			//Define a Trigger that will fire "later"
			final JobDetail job2 = new JobDetail("job" + random.nextInt(), null, JobTestImpl.class);
			final Trigger trigger2 = new SimpleTrigger("trigger" + random.nextInt(), null,
					new Date(System.currentTimeMillis() + random.nextInt(60000)));
			scheduler.scheduleJob(job2, trigger2);

			// JavaInformations doit être réinstancié pour récupérer les jobs
			// (mais "Aucun job" dans le counter)
			final List<JavaInformations> javaInformationsList = Collections
					.singletonList(new JavaInformations(null, true));
			final PdfReport pdfReport = new PdfReport(collector, false, javaInformationsList,
					Period.TOUT, output);
			pdfReport.toPdf();
			assertNotEmptyAndClear(output);

			//Define a Trigger that will fire "now"
			final JobDetail job = new JobDetail("job" + random.nextInt(), null, JobTestImpl.class);
			final Trigger trigger = new SimpleTrigger("trigger" + random.nextInt(), null,
					new Date());
			//Schedule the job with the trigger
			scheduler.scheduleJob(job, trigger);

			// JobTestImpl fait un sleep de 2s au plus, donc on l'attend pour le compter
			try {
				Thread.sleep(2100);
			} catch (InterruptedException e) {
				throw new IllegalStateException(e);
			}

			// JavaInformations doit être réinstancié pour récupérer les jobs
			final List<JavaInformations> javaInformationsList2 = Collections
					.singletonList(new JavaInformations(null, true));
			final PdfReport pdfReport2 = new PdfReport(collector, false, javaInformationsList2,
					Period.TOUT, output);
			pdfReport2.toPdf();
			assertNotEmptyAndClear(output);
		} finally {
			scheduler.shutdown();
			JobGlobalListener.getJobCounter().clear();
			JobGlobalListener.destroyJobGlobalListener();
		}
	}

	private void rootContexts(Counter counter, Collector collector, Timer timer,
			JavaInformations javaInformations, ByteArrayOutputStream output) throws IOException {
		PdfReport pdfReport;
		TestCounter.bindRootContexts("first request", counter, 3);
		pdfReport = new PdfReport(collector, false, Collections.singletonList(javaInformations),
				Period.TOUT, output);
		pdfReport.toPdf();
		assertNotEmptyAndClear(output);

		final Counter myCounter = new Counter("http", null);
		final Collector collector2 = new Collector("test 2", Arrays.asList(myCounter), timer);
		myCounter.bindContext("my context", "my context");
		pdfReport = new PdfReport(collector2, false, Collections.singletonList(javaInformations),
				Period.TOUT, output);
		pdfReport.toPdf();
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
		final Timer timer = new Timer("test timer", true);
		try {
			final List<Counter> counters = Arrays.asList(counter, errorCounter);
			final Collector collector = new Collector(TEST_APP, counters, timer);
			final JavaInformations javaInformations = new JavaInformations(null, true);
			final ByteArrayOutputStream output = new ByteArrayOutputStream();
			final List<JavaInformations> javaInformationsList = Collections
					.singletonList(javaInformations);
			counter.addRequest("test include graph", 1, 1, false, 1000);
			errorCounter.addRequestForSystemError("error", 1, 1, null);
			collector.collectWithoutErrors(javaInformationsList);
			counter.addRequest("test include graph", 1, 1, false, 1000);
			errorCounter.addRequestForSystemError("error", 1, 1, null);
			collector.collectWithoutErrors(javaInformationsList);
			final Document document = new PdfDocumentFactory(TEST_APP, output).createDocument();
			document.open();
			final PdfCounterReport pdfCounterReport = new PdfCounterReport(collector, counter,
					Period.TOUT.getRange(), true, document);
			pdfCounterReport.toPdf();
			pdfCounterReport.writeRequestDetails();
			final PdfCounterReport pdfErrorCounterReport = new PdfCounterReport(collector,
					errorCounter, Period.TOUT.getRange(), true, document);
			pdfErrorCounterReport.writeRequestDetails();
			document.close();
			assertNotEmptyAndClear(output);
		} finally {
			timer.cancel();
		}
	}

	/** Test.
	 * @throws IOException e
	 * @throws DocumentException e */
	@Test
	public void testEmptyPdfCounterRequestContext() throws IOException, DocumentException {
		final ByteArrayOutputStream output = new ByteArrayOutputStream();
		final PdfDocumentFactory pdfDocumentFactory = new PdfDocumentFactory(TEST_APP, output);
		final Document document = pdfDocumentFactory.createDocument();
		document.open();
		final PdfCounterRequestContextReport report = new PdfCounterRequestContextReport(
				Collections.<CounterRequestContext> emptyList(), Collections
						.<PdfCounterReport> emptyList(), Collections
						.<ThreadInformations> emptyList(), true, pdfDocumentFactory, document);
		report.toPdf();
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
		final PdfDocumentFactory pdfDocumentFactory = new PdfDocumentFactory(TEST_APP, output);
		final Document document = pdfDocumentFactory.createDocument();
		document.open();
		boolean stackTraceEnabled = true;
		final PdfThreadInformationsReport report = new PdfThreadInformationsReport(JavaInformations
				.buildThreadInformationsList(), stackTraceEnabled, pdfDocumentFactory, document);
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
	}

	/** Test. */
	@Test
	public void testGetFileName() {
		assertNotNull("filename", PdfReport.getFileName("test"));
	}
}
