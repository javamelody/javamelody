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

import static org.easymock.EasyMock.createNiceMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.File;
import java.io.IOException;
import java.util.Collections;
import java.util.Date;
import java.util.Locale;
import java.util.Random;

import javax.cache.Caching;
import javax.cache.configuration.MutableConfiguration;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.quartz.JobBuilder;
import org.quartz.JobDetail;
import org.quartz.Scheduler;
import org.quartz.SchedulerException;
import org.quartz.Trigger;
import org.quartz.TriggerBuilder;
import org.quartz.impl.StdSchedulerFactory;

import jakarta.servlet.ServletContext;
import jakarta.servlet.http.HttpSession;
import net.bull.javamelody.JobTestImpl;
import net.bull.javamelody.Parameter;
import net.bull.javamelody.Utils;
import net.bull.javamelody.internal.common.Parameters;
import net.sf.ehcache.CacheManager;
import net.sf.ehcache.Element;

/**
 * Test unitaire de la classe Action.
 * @author Emeric Vernat
 */
class TestAction {
	private static final String ALL = "all";

	/** Check. */
	@BeforeEach
	void setUp() {
		Utils.initialize();
	}

	/** Test. */
	@Test
	void testValueOfIgnoreCase() {
		for (final Action action : Action.values()) {
			assertSame(action, Action.valueOfIgnoreCase(action.toString().toLowerCase(Locale.getDefault())), "same");
		}
	}

	/** Test. */
	@Test
	void testGetContextName() {
		for (final Action action : Action.values()) {
			assertNotNull(action.getContextName("test"), "getContextName");
			assertNotNull(action.getContextName(ALL), "getContextName");
		}
	}

	/** Test.
	 * @throws IOException e
	 * @throws SchedulerException e */
	@Test
	void testExecute() throws IOException, SchedulerException {
		final Counter counter = new Counter(Counter.HTTP_COUNTER_NAME, null);
		counter.addRequest("test1", 0, 1, 1, false, 1000);
		counter.addRequest("test2", 1000, 900, 900, false, 1000);
		counter.addRequest("test3", 10000, 1000, 1000, true, 10000);
		final Collector collector = new Collector("test", Collections.singletonList(counter),
				new SamplingProfiler());
		final String counterName = counter.getName();
		final String sessionId = "sessionId";
		final String threadId = "threadId";
		final String jobId = "jobId";
		final String cacheId = "test clear";
		final HttpSession session = createNiceMock(HttpSession.class);
		expect(session.getId()).andReturn("Mock").anyTimes();
		replay(session);

		assertNotNull(Action.GC.execute(collector, null, counterName, sessionId,
				threadId, jobId, cacheId), "message GC");
		assertNotNull(Action.GC.execute(collector, null, null, counterName, sessionId,
				threadId, jobId, cacheId), "message GC");
		assertNotNull(Action.CLEAR_COUNTER.execute(collector, null, null,
				counterName, sessionId, threadId, jobId, cacheId), "message CLEAR_COUNTER");
		assertNotNull(Action.CLEAR_COUNTER.execute(collector, null, null,
				ALL, sessionId, threadId, jobId, cacheId), "message CLEAR_COUNTER");

		caches(collector, counterName, sessionId, threadId, jobId, cacheId);

		assertNotNull(Action.CLEAR_HOTSPOTS.execute(collector, null, null,
				counterName, sessionId, threadId, jobId, cacheId), "message CLEAR_HOTSPOTS");
		assertNotNull(Action.PURGE_OBSOLETE_FILES.execute(collector,
				null, null, counterName, sessionId, threadId, jobId, cacheId), "message PURGE_OBSOLETE_FILES");
		final String heapDump1 = Action.HEAP_DUMP.execute(collector, null, null, counterName,
				sessionId, threadId, jobId, cacheId);
		assertNotNull(heapDump1, "message HEAP_DUMP");
		String heapDump2;
		// on le refait une deuxième fois dans la même seconde pour tester le nom du fichier
		do {
			heapDump2 = Action.HEAP_DUMP.execute(collector, null, null, counterName, sessionId,
					threadId, jobId, cacheId);
			assertNotNull(heapDump2, "message HEAP_DUMP");
		} while (heapDump1.equals(heapDump2));
		final File[] files = Parameters.TEMPORARY_DIRECTORY.listFiles();
		if (files != null) {
			for (final File file : files) {
				if (!file.isDirectory() && file.getName().startsWith("heapdump")
						&& !file.delete()) {
					file.deleteOnExit();
				}
			}
		}

		invalidateSessions(collector, counterName, sessionId, threadId, jobId, cacheId, session);

		killThread(collector, counterName, sessionId, threadId, jobId, cacheId);

		sendThreadInterrupt(collector, counterName, sessionId, threadId, jobId, cacheId);

		jobs(collector, counterName, sessionId, threadId, jobId, cacheId);

		mailTest(collector);

		verify(session);
	}

	private void caches(final Collector collector, final String counterName, final String sessionId,
			final String threadId, final String jobId, final String cacheId) throws IOException {
		if (CacheManager.getInstance().getCache(cacheId) == null) {
			CacheManager.getInstance().addCache(cacheId);
		}
		assertNotNull(Action.CLEAR_CACHES.execute(collector, null, null,
				counterName, sessionId, threadId, jobId, cacheId), "message CLEAR_CACHES");
		assertNotNull(Action.CLEAR_CACHE.execute(collector, null, null,
				counterName, sessionId, threadId, jobId, cacheId), "message CLEAR_CACHE");
		assertNotNull(Action.CLEAR_CACHE.execute(collector, null, null,
				counterName, sessionId, threadId, jobId, "inconnu"), "message CLEAR_CACHE");
		assertNotNull(Action.CLEAR_CACHE_KEY.execute(collector, null, null,
				counterName, sessionId, threadId, jobId, cacheId, "inconnue"), "message CLEAR_CACHE_KEY");
		CacheManager.getInstance().getCache(cacheId).put(new Element("1", "value"));
		CacheManager.getInstance().getCache(cacheId).put(new Element("2", "value"));
		CacheManager.getInstance().getCache(cacheId).put(new Element(3, "value"));
		assertNotNull(Action.CLEAR_CACHE_KEY.execute(collector, null, null,
				counterName, sessionId, threadId, jobId, "inconnu", "inconnue"), "message CLEAR_CACHE_KEY");
		assertNotNull(Action.CLEAR_CACHE_KEY.execute(collector, null, null,
				counterName, sessionId, threadId, jobId, cacheId, "inconnue"), "message CLEAR_CACHE_KEY");
		assertNotNull(Action.CLEAR_CACHE_KEY.execute(collector, null, null,
				counterName, sessionId, threadId, jobId, cacheId, "1"), "message CLEAR_CACHE_KEY");
		assertNotNull(Action.CLEAR_CACHE_KEY.execute(collector, null, null,
				counterName, sessionId, threadId, jobId, cacheId, "3"), "message CLEAR_CACHE_KEY");
		CacheManager.getInstance().removeCache(cacheId);

		final javax.cache.CacheManager jcacheManager = Caching.getCachingProvider()
				.getCacheManager();
		if (jcacheManager.getCache(cacheId) == null) {
			final MutableConfiguration<Object, Object> conf = new MutableConfiguration<>();
			conf.setManagementEnabled(true);
			conf.setStatisticsEnabled(true);
			jcacheManager.createCache(cacheId, conf);
		}
		assertNotNull(Action.CLEAR_JCACHES.execute(collector, null, null,
				counterName, sessionId, threadId, jobId, cacheId), "message CLEAR_JCACHES");
		assertNotNull(Action.CLEAR_JCACHE.execute(collector, null, null,
				counterName, sessionId, threadId, jobId, cacheId), "message CLEAR_JCACHE");
		assertNotNull(Action.CLEAR_JCACHE.execute(collector, null, null,
				counterName, sessionId, threadId, jobId, "inconnu"), "message CLEAR_JCACHE");
		assertNotNull(Action.CLEAR_JCACHE_KEY.execute(collector, null, null,
				counterName, sessionId, threadId, jobId, "inconnu", "inconnue"), "message CLEAR_JCACHE_KEY");
		assertNotNull(Action.CLEAR_JCACHE_KEY.execute(collector, null, null,
				counterName, sessionId, threadId, jobId, cacheId, "inconnue"), "message CLEAR_JCACHE_KEY");
		jcacheManager.getCache(cacheId).put("1", "value");
		jcacheManager.getCache(cacheId).put("2", "value");
		jcacheManager.getCache(cacheId).put(3, "value");
		assertNotNull(Action.CLEAR_JCACHE_KEY.execute(collector, null, null,
				counterName, sessionId, threadId, jobId, cacheId, "inconnue"), "message CLEAR_JCACHE_KEY");
		assertNotNull(Action.CLEAR_JCACHE_KEY.execute(collector, null, null,
				counterName, sessionId, threadId, jobId, cacheId, "1"), "message CLEAR_JCACHE_KEY");
		assertNotNull(Action.CLEAR_JCACHE_KEY.execute(collector, null, null,
				counterName, sessionId, threadId, jobId, cacheId, "3"), "message CLEAR_JCACHE_KEY");
		jcacheManager.destroyCache(cacheId);
	}

	private void invalidateSessions(Collector collector, String counterName, String sessionId,
			String threadId, String jobId, String cacheId, HttpSession session) throws IOException {
		assertNotNull(Action.INVALIDATE_SESSIONS.execute(collector,
				null, session, counterName, sessionId, threadId, jobId, cacheId), "message INVALIDATE_SESSIONS");
		assertNotNull(Action.INVALIDATE_SESSION.execute(collector,
				null, null, counterName, sessionId, threadId, jobId, cacheId), "message INVALIDATE_SESSION");

		assertNotNull(Action.LOGOUT.execute(collector, null, session,
				counterName, sessionId, threadId, jobId, cacheId), "message LOGOUT");
		assertNotNull(Action.LOGOUT.execute(collector, null, null,
				counterName, sessionId, threadId, jobId, cacheId), "message LOGOUT");
	}

	private void mailTest(Collector collector) throws IOException {
		final ServletContext context = createNiceMock(ServletContext.class);
		expect(context.getMajorVersion()).andReturn(5).anyTimes();
		expect(context.getMinorVersion()).andReturn(0).anyTimes();
		expect(context.getServletContextName()).andReturn("test webapp").anyTimes();
		expect(context.getServerInfo()).andReturn("mockJetty").anyTimes();
		expect(context.getContextPath()).andReturn("/test").anyTimes();
		replay(context);
		Parameters.initialize(context);
		try {
			Action.MAIL_TEST.execute(collector, null, null, null, null, null, null, null);
		} catch (final IllegalStateException e) {
			// ok, il manque un paramètre
			assertNotNull(e, e.toString());
		}
		Utils.setProperty(Parameter.MAIL_SESSION, "mail/Session");
		try {
			Action.MAIL_TEST.execute(collector, null, null, null, null, null, null, null);
		} catch (final IllegalStateException e) {
			// ok, il manque encore un paramètre
			assertNotNull(e, e.toString());
		}
		Utils.setProperty(Parameter.ADMIN_EMAILS, "admin@blah blah.fr");
		try {
			Action.MAIL_TEST.execute(collector, null, null, null, null, null, null, null);
		} catch (final Exception e) {
			// ok, de toute façon il n'y a pas de Session dans JNDI
			assertNotNull(e, e.toString());
		}
		verify(context);
	}

	private void jobs(Collector collector, String counterName, String sessionId, String threadId,
			String jobId, String cacheId) throws IOException, SchedulerException {
		try {
			assertNotNull(Action.PAUSE_JOB.execute(collector, null, null,
					counterName, sessionId, threadId, jobId, cacheId), "message PAUSE_JOB 1");
		} catch (final IllegalArgumentException e) {
			assertNotNull(e, e.toString());
		}
		try {
			assertNotNull(Action.RESUME_JOB.execute(collector, null, null,
					counterName, sessionId, threadId, jobId, cacheId), "message RESUME_JOB 1");
		} catch (final IllegalArgumentException e) {
			assertNotNull(e, e.toString());
		}
		assertNotNull(Action.PAUSE_JOB.execute(collector, null, null,
				counterName, sessionId, threadId, ALL, cacheId), "message PAUSE_JOB 2");
		assertNotNull(Action.RESUME_JOB.execute(collector, null, null,
				counterName, sessionId, threadId, ALL, cacheId), "message RESUME_JOB 2");
		assertNull(Action.PAUSE_JOB.execute(collector, null, null,
				counterName, sessionId, threadId, "nopid_noip_id", cacheId), "message PAUSE_JOB 3");
		assertNull(Action.RESUME_JOB.execute(collector, null, null,
				counterName, sessionId, threadId, "nopid_noip_id", cacheId), "message RESUME_JOB 3");
		assertNull(Action.PAUSE_JOB.execute(collector, null, null,
				counterName, sessionId, threadId, PID.getPID() + "_noip_id", cacheId), "message PAUSE_JOB 4");
		assertNull(Action.RESUME_JOB.execute(collector, null, null,
				counterName, sessionId, threadId, PID.getPID() + "_noip_id", cacheId), "message RESUME_JOB 4");
		String globalJobId = PID.getPID() + '_' + Parameters.getHostAddress() + '_' + 10000;
		assertNotNull(Action.PAUSE_JOB.execute(collector, null, null,
				counterName, sessionId, threadId, globalJobId, cacheId), "message PAUSE_JOB 5");
		assertNotNull(Action.RESUME_JOB.execute(collector, null, null,
				counterName, sessionId, threadId, globalJobId, cacheId), "message RESUME_JOB 5");

		//Grab the Scheduler instance from the Factory
		final Scheduler scheduler = StdSchedulerFactory.getDefaultScheduler();

		try {
			// and start it off
			scheduler.start();

			//Define job instance
			final Random random = new Random();

			final JobDetail job = JobBuilder.newJob(JobTestImpl.class)
					.withIdentity("job" + random.nextInt()).build();

			//Define a Trigger that will fire "later"
			final Trigger trigger = TriggerBuilder.newTrigger()
					.withIdentity("trigger" + random.nextInt())
					.startAt(new Date(System.currentTimeMillis() + 60000)).build();
			//Schedule the job with the trigger
			scheduler.scheduleJob(job, trigger);

			assertNotNull(Action.PAUSE_JOB.execute(collector, null, null,
					counterName, sessionId, threadId, globalJobId, cacheId), "message PAUSE_JOB 5");
			assertNotNull(Action.RESUME_JOB.execute(collector, null, null,
					counterName, sessionId, threadId, globalJobId, cacheId), "message RESUME_JOB 5");

			globalJobId = PID.getPID() + '_' + Parameters.getHostAddress() + '_'
					+ job.getKey().getName().hashCode();
			assertNotNull(Action.PAUSE_JOB.execute(collector, null, null,
					counterName, sessionId, threadId, globalJobId, cacheId), "message PAUSE_JOB 6");
			assertNotNull(Action.RESUME_JOB.execute(collector, null, null,
					counterName, sessionId, threadId, globalJobId, cacheId), "message RESUME_JOB 6");

			assertNotNull(Action.PAUSE_JOB.execute(collector, null, null,
					counterName, sessionId, threadId, ALL, cacheId), "message PAUSE_JOB 7");
			assertNotNull(Action.RESUME_JOB.execute(collector, null, null,
					counterName, sessionId, threadId, ALL, cacheId), "message RESUME_JOB 7");
		} finally {
			scheduler.shutdown();
		}
	}

	private void killThread(Collector collector, String counterName, String sessionId,
			String threadId, String jobId, String cacheId) throws IOException {
		try {
			assertNull(Action.KILL_THREAD.execute(collector, null, null,
					counterName, sessionId, threadId, jobId, cacheId), "message KILL_THREAD 1");
		} catch (final IllegalArgumentException e) {
			assertNotNull(e, e.toString());
		}
		assertNull(Action.KILL_THREAD.execute(collector, null, null,
				counterName, sessionId, "nopid_noip_id", jobId, cacheId), "message KILL_THREAD 2");
		assertNull(Action.KILL_THREAD.execute(collector, null, null,
				counterName, sessionId, PID.getPID() + "_noip_id", jobId, cacheId), "message KILL_THREAD 3");
		final Thread myThread = new Thread(new Runnable() {
			/** {@inheritDoc} */
			@Override
			public void run() {
				try {
					Thread.sleep(10000);
				} catch (final InterruptedException e) {
					throw new IllegalStateException(e);
				}
			}
		});
		myThread.setName("thread test");
		myThread.start();
		String globalThreadId = PID.getPID() + '_' + Parameters.getHostAddress() + '_'
				+ myThread.getId();

		final boolean java20OrLater = "20".compareTo(Parameters.JAVA_VERSION) < 0;
		if (!java20OrLater) {
			assertNotNull(Action.KILL_THREAD.execute(collector, null, null,
					counterName, sessionId, globalThreadId, jobId, cacheId), "message KILL_THREAD 4");
		}
		globalThreadId = PID.getPID() + '_' + Parameters.getHostAddress() + '_' + 10000;
		assertNotNull(Action.KILL_THREAD.execute(collector, null, null,
				counterName, sessionId, globalThreadId, jobId, cacheId), "message KILL_THREAD 5");

		final Thread myThread2 = new Thread(new Runnable() {
			/** {@inheritDoc} */
			@Override
			public void run() {
				try {
					Thread.sleep(10000);
				} catch (final InterruptedException e) {
					assertNotNull(e, e.toString());
				}
			}
		});
		myThread2.setName("javamelody test 2");
		myThread2.start();
		final String globalThreadId2 = PID.getPID() + '_' + Parameters.getHostAddress() + '_'
				+ myThread2.getId();
		assertEquals("I will not kill myself", Action.KILL_THREAD.execute(
				collector, null, null, counterName, sessionId, globalThreadId2, jobId, cacheId),
				"message KILL_THREAD 6");
	}

	private void sendThreadInterrupt(Collector collector, String counterName, String sessionId,
			String threadId, String jobId, String cacheId) throws IOException {
		try {
			assertNull(Action.SEND_THREAD_INTERRUPT.execute(
					collector, null, null, counterName, sessionId, threadId, jobId, cacheId),
					"message SEND_THREAD_INTERRUPT 1");
		} catch (final IllegalArgumentException e) {
			assertNotNull(e, e.toString());
		}
		assertNull(Action.SEND_THREAD_INTERRUPT.execute(
				collector, null, null, counterName, sessionId, "nopid_noip_id", jobId, cacheId),
				"message SEND_THREAD_INTERRUPT 2");
		assertNull(Action.SEND_THREAD_INTERRUPT.execute(collector, null, null, counterName, sessionId,
						PID.getPID() + "_noip_id", jobId, cacheId), "message SEND_THREAD_INTERRUPT 3");
		final Thread myThread = new Thread(new Runnable() {
			/** {@inheritDoc} */
			@Override
			public void run() {
				try {
					Thread.sleep(10000);
				} catch (final InterruptedException e) {
					assertNotNull(e, e.toString());
				}
			}
		});
		myThread.setName("thread test 2");
		myThread.start();
		String globalThreadId = PID.getPID() + '_' + Parameters.getHostAddress() + '_'
				+ myThread.getId();
		assertNotNull(Action.SEND_THREAD_INTERRUPT.execute(
				collector, null, null, counterName, sessionId, globalThreadId, jobId, cacheId), "message SEND_THREAD_INTERRUPT 4");
		globalThreadId = PID.getPID() + '_' + Parameters.getHostAddress() + '_' + 10000;
		assertNotNull(Action.SEND_THREAD_INTERRUPT.execute(
				collector, null, null, counterName, sessionId, globalThreadId, jobId, cacheId),
				"message SEND_THREAD_INTERRUPT 5");

		final Thread myThread2 = new Thread(new Runnable() {
			/** {@inheritDoc} */
			@Override
			public void run() {
				try {
					Thread.sleep(10000);
				} catch (final InterruptedException e) {
					assertNotNull(e, e.toString());
				}
			}
		});
		myThread2.setName("javamelody test 2");
		myThread2.start();
		final String globalThreadId2 = PID.getPID() + '_' + Parameters.getHostAddress() + '_'
				+ myThread2.getId();
		assertEquals("I will not interrupt myself",
				Action.SEND_THREAD_INTERRUPT.execute(collector, null, null, counterName, sessionId,
						globalThreadId2, jobId, cacheId),
				"message SEND_THREAD_INTERRUPT 6");
	}

	/** Test. */
	@Test
	void testCheckSystemActionsEnabled() {
		boolean systemActionsEnabled = true;
		try {
			Action.checkSystemActionsEnabled();
		} catch (final Exception e) {
			systemActionsEnabled = false;
		}
		// test: par défaut les actions systèmes sont considérées activées
		if (!systemActionsEnabled) {
			fail("checkSystemActionsEnabled");
		}
		Utils.setProperty(Parameter.SYSTEM_ACTIONS_ENABLED, "false");
		systemActionsEnabled = true;
		try {
			Action.checkSystemActionsEnabled();
		} catch (final Exception e) {
			systemActionsEnabled = false;
		}
		if (systemActionsEnabled) {
			fail("checkSystemActionsEnabled");
		}
	}
}
