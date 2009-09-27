/*
 * Copyright 2008-2009 by Emeric Vernat, Bull
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
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.List;
import java.util.Timer;

import net.sf.ehcache.CacheManager;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * Test unitaire de la classe Collector.
 * @author Emeric Vernat
 */
public class TestCollector {
	private Timer timer;

	/** Test. */
	@Before
	public void setUp() {
		final File[] files = Parameters.getStorageDirectory("test").listFiles();
		if (files != null) {
			for (final File file : files) {
				if ((file.getName().endsWith(".rrd") || file.getName().endsWith(".ser.gz"))
						&& !file.delete()) {
					file.deleteOnExit();
				}
			}
		}
		timer = new Timer("test timer", true);
	}

	/** Test. */
	@After
	public void tearDown() {
		timer.cancel();
	}

	private Collector createCollectorWithOneCounter() {
		final Counter counter = createCounter();
		return new Collector("test collector", Collections.singletonList(counter), timer);
	}

	/** Test. */
	@Test
	public void testNewCollector() {
		try {
			assertNotNull("collector", createCollectorWithOneCounter());
		} finally {
			timer.cancel();
		}
	}

	/** Test. */
	@Test
	public void testToString() {
		try {
			final Collector collector = createCollectorWithOneCounter();
			final String toString = collector.toString();
			if (toString == null || toString.isEmpty()) {
				fail("toString non null et non vide");
			}
			if (new JavaInformations(null, false).toString().isEmpty()) {
				fail("toString java vide");
			}
			if (new ThreadInformations(Thread.currentThread(), Arrays.asList(Thread.currentThread()
					.getStackTrace()), 100, 1000).toString().isEmpty()) {
				fail("toString thread vide");
			}
			if (new SessionInformations(new SessionTestImpl(true), true).toString().isEmpty()) {
				fail("toString session vide");
			}
			CacheManager.getInstance().addCache("testToString");
			if (new CacheInformations(CacheManager.getInstance().getEhcache("testToString"))
					.toString().isEmpty()) {
				fail("toString cache vide");
			}
		} finally {
			timer.cancel();
		}
	}

	/** Test. */
	@Test
	public void testClearCounter() {
		try {
			final Counter counter = createCounter();
			final Collector collector = new Collector("test collector", Collections
					.singletonList(counter), timer);
			counter.addRequest("test clear", 0, 0, false, 1000);
			collector.clearCounter(counter.getName());
			if (counter.getRequestsCount() != 0) {
				fail("counter vide");
			}
			collector.clearCounter("nothing");
		} finally {
			timer.cancel();
		}
	}

	private Counter createCounter() {
		return new Counter("http", null);
	}

	/** Test. */
	@Test
	public void testGetApplication() {
		try {
			assertEquals("getApplication", "test collector", createCollectorWithOneCounter()
					.getApplication());
		} finally {
			timer.cancel();
		}
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testCollectWithoutErrors() throws IOException {
		try {
			final Counter counter = createCounter();
			final Collector collector = new Collector("test remove", Collections
					.singletonList(counter), timer);
			if (collector.getCounters().size() == 0) {
				fail("getCounters");
			}
			counter.addRequest("test1", 0, 0, false, 1000);
			collector.collectWithoutErrors(Collections.singletonList(new JavaInformations(null,
					true)));
			counter.addRequest("test2", 0, 0, false, 1000);
			counter.addRequest("test3", 1000, 500, false, 1000);
			counter.addRequest("test4", 10000, 200, true, 10000);
			collector.collectWithoutErrors(Collections.singletonList(new JavaInformations(null,
					false)));
			if (collector.getLastCollectDuration() == 0) {
				fail("getLastCollectDuration");
			}

			if (collector.getCounterJRobins().size() == 0) {
				fail("getCounterJRobins");
			}
			for (final JRobin jrobin : collector.getCounterJRobins()) {
				final JRobin robin = collector.getJRobin(jrobin.getName());
				assertNotNull("getJRobin non null", robin);
				jrobin.graph(Period.JOUR, 500, 200);
				jrobin.graph(Period.JOUR, 80, 80);
			}
			for (final CounterRequest request : counter.getRequests()) {
				assertNotNull("getJRobin non null", collector.getJRobin(request.getId()));
			}
			assertNull("getJRobin null", collector.getJRobin("n'importe quoi"));
		} finally {
			timer.cancel();
		}
	}

	/** Test. */
	@Test
	public void testRemoveRequest() {
		try {
			final Counter counter = new Counter("error", null);
			counter.setMaxRequestsCount(1);
			final Collector collector = new Collector("test", Collections.singletonList(counter),
					timer);

			// test removeRequest dans collectCounterData
			counter.addRequest("test 1", 0, 0, false, 1000);
			counter.addRequest("test 2", 0, 0, false, 1000);
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
		} finally {
			timer.cancel();
		}
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testGetPeriodCountersToBeDisplayed() throws IOException {
		try {
			final Counter counter = createCounter();
			final Collector collector = new Collector("test", Collections.singletonList(counter),
					timer);
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

			assertEquals("jour", 1, collector.getPeriodCountersToBeDisplayed(Period.JOUR).size());
			assertEquals("semaine", 1, collector.getPeriodCountersToBeDisplayed(Period.SEMAINE)
					.size());
			assertEquals("mois", 1, collector.getPeriodCountersToBeDisplayed(Period.MOIS).size());
			assertEquals("année", 1, collector.getPeriodCountersToBeDisplayed(Period.ANNEE).size());
			assertEquals("tout", 1, collector.getPeriodCountersToBeDisplayed(Period.TOUT).size());
		} finally {
			timer.cancel();
		}
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testBuildNewDayCounter() throws IOException {
		try {
			final Counter counter = createCounter();
			counter.setApplication("test counter");
			final File storageDir = Parameters.getStorageDirectory(counter.getApplication());
			final File obsoleteFile = new File(storageDir, "obsolete.ser.gz");
			final File notObsoleteFile = new File(storageDir, "notobsolete.ser.gz");
			if (!storageDir.exists() && !storageDir.mkdirs()) {
				fail("mkdir");
			}
			if (!obsoleteFile.exists() && !obsoleteFile.createNewFile()) {
				fail("createNewFile");
			}
			if (!notObsoleteFile.exists() && !notObsoleteFile.createNewFile()) {
				fail("createNewFile");
			}
			final Calendar nowMinus1YearAnd2Days = Calendar.getInstance();
			nowMinus1YearAnd2Days.add(Calendar.YEAR, -1);
			nowMinus1YearAnd2Days.add(Calendar.DAY_OF_YEAR, -2);
			if (!obsoleteFile.setLastModified(nowMinus1YearAnd2Days.getTimeInMillis())) {
				fail("setLastModified");
			}
			final Counter newDayCounter = new PeriodCounterFactory(counter).buildNewDayCounter();
			assertNotNull("buildNewDayCounter", newDayCounter);
			// le fichier doit avoir été supprimé
			if (obsoleteFile.exists()) {
				fail("obsolete file still exists");
			}
			if (!notObsoleteFile.delete()) {
				notObsoleteFile.deleteOnExit();
			}
		} finally {
			timer.cancel();
		}
	}

	/** Test. */
	@Test
	public void testStop() {
		try {
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
			final Collector collector2 = new Collector("test stop", Collections
					.singletonList(counter), timer);
			counter.addRequest("test stop", 0, 0, false, 1000);
			collector2.collectWithoutErrors(Collections.singletonList(new JavaInformations(null,
					true)));
			collector2.stop();
			setProperty(Parameter.STORAGE_DIRECTORY, "monitoring");
		} finally {
			timer.cancel();
		}
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
		assertNotNull("JavaInformations", javaInformations);
	}

	/** Test. */
	@Test
	public void testThreadInformations() {
		assertTrue("getCurrentThreadCpuTime", ThreadInformations.getCurrentThreadCpuTime() > 0);
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testCollectorServer() throws IOException {
		try {
			// ce test ne fait que vérifier s'il n'y a pas d'erreur inattendue
			// car sans serveur d'application monitoré il ne peut rien faire d'autre
			final String application = "testapp";
			Parameters.removeCollectorApplication(application);
			// pour être sûr qu'il y a une application
			final List<URL> urls = Parameters.parseUrl("http://localhost:8090/test");
			Parameters.addCollectorApplication(application, urls);

			final CollectorServer collectorServer = new CollectorServer();
			collectorServer.collectWithoutErrors();
			Parameters.removeCollectorApplication(application);
			try {
				collectorServer.addCollectorApplication(application, urls);
			} catch (final Exception e) {
				// exception car il n'y a pas de serveur à cette adresse
				assertNotNull("exception", e);
			}
			collectorServer.getCollectorByApplication(application);
			collectorServer.getJavaInformationsByApplication(application);
			collectorServer.isApplicationDataAvailable(application);
			collectorServer.getFirstApplication();
			collectorServer.scheduleReportMailForCollectorServer(application);
			collectorServer.removeCollectorApplication(application);
			Parameters.addCollectorApplication(application, urls);
		} finally {
			timer.cancel();
		}
	}

	private static void setProperty(Parameter parameter, String value) {
		System.setProperty(Parameters.PARAMETER_SYSTEM_PREFIX + parameter.getCode(), value);
	}
}
