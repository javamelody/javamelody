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
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.util.Collections;
import java.util.Date;
import java.util.Locale;
import java.util.Random;

import javax.cache.Caching;
import javax.cache.configuration.MutableConfiguration;
import javax.servlet.ServletContext;
import javax.servlet.http.HttpSession;

import org.junit.Before;
import org.junit.Test;
import org.quartz.JobDetail;
import org.quartz.Scheduler;
import org.quartz.SchedulerException;
import org.quartz.SimpleTrigger;
import org.quartz.Trigger;
import org.quartz.impl.StdSchedulerFactory;

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
public class TestAction {
	private static final String ALL = "all";

	/** Check. */
	@Before
	public void setUp() {
		Utils.initialize();
	}

	/** Test. */
	@Test
	public void testValueOfIgnoreCase() {
		for (final Action action : Action.values()) {
			assertSame("same", action,
					Action.valueOfIgnoreCase(action.toString().toLowerCase(Locale.getDefault())));
		}
	}

	/** Test. */
	@Test
	public void testGetContextName() {
		for (final Action action : Action.values()) {
			assertNotNull("getContextName", action.getContextName("test"));
			assertNotNull("getContextName", action.getContextName(ALL));
		}
	}

	/** Test.
	 * @throws IOException e
	 * @throws SchedulerException e */
	@Test
	public void testExecute() throws IOException, SchedulerException {
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

		assertNotNull("message GC", Action.GC.execute(collector, null, counterName, sessionId,
				threadId, jobId, cacheId));
		assertNotNull("message GC", Action.GC.execute(collector, null, null, counterName, sessionId,
				threadId, jobId, cacheId));
		assertNotNull("message CLEAR_COUNTER", Action.CLEAR_COUNTER.execute(collector, null, null,
				counterName, sessionId, threadId, jobId, cacheId));
		assertNotNull("message CLEAR_COUNTER", Action.CLEAR_COUNTER.execute(collector, null, null,
				ALL, sessionId, threadId, jobId, cacheId));

		caches(collector, counterName, sessionId, threadId, jobId, cacheId);

		assertNotNull("message CLEAR_HOTSPOTS", Action.CLEAR_HOTSPOTS.execute(collector, null, null,
				counterName, sessionId, threadId, jobId, cacheId));
		assertNotNull("message PURGE_OBSOLETE_FILES", Action.PURGE_OBSOLETE_FILES.execute(collector,
				null, null, counterName, sessionId, threadId, jobId, cacheId));
		final String heapDump1 = Action.HEAP_DUMP.execute(collector, null, null, counterName,
				sessionId, threadId, jobId, cacheId);
		assertNotNull("message HEAP_DUMP", heapDump1);
		String heapDump2;
		// on le refait une deuxième fois dans la même seconde pour tester le nom du fichier
		do {
			heapDump2 = Action.HEAP_DUMP.execute(collector, null, null, counterName, sessionId,
					threadId, jobId, cacheId);
			assertNotNull("message HEAP_DUMP", heapDump2);
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
		assertNotNull("message CLEAR_CACHES", Action.CLEAR_CACHES.execute(collector, null, null,
				counterName, sessionId, threadId, jobId, cacheId));
		assertNotNull("message CLEAR_CACHE", Action.CLEAR_CACHE.execute(collector, null, null,
				counterName, sessionId, threadId, jobId, cacheId));
		assertNotNull("message CLEAR_CACHE", Action.CLEAR_CACHE.execute(collector, null, null,
				counterName, sessionId, threadId, jobId, "inconnu"));
		assertNotNull("message CLEAR_CACHE", Action.CLEAR_CACHE_KEY.execute(collector, null, null,
				counterName, sessionId, threadId, jobId, cacheId, "inconnue"));
		CacheManager.getInstance().getCache(cacheId).put(new Element("1", "value"));
		CacheManager.getInstance().getCache(cacheId).put(new Element("2", "value"));
		CacheManager.getInstance().getCache(cacheId).put(new Element(Integer.valueOf(3), "value"));
		assertNotNull("message CLEAR_CACHE", Action.CLEAR_CACHE_KEY.execute(collector, null, null,
				counterName, sessionId, threadId, jobId, "inconnu", "inconnue"));
		assertNotNull("message CLEAR_CACHE", Action.CLEAR_CACHE_KEY.execute(collector, null, null,
				counterName, sessionId, threadId, jobId, cacheId, "inconnue"));
		assertNotNull("message CLEAR_CACHE", Action.CLEAR_CACHE_KEY.execute(collector, null, null,
				counterName, sessionId, threadId, jobId, cacheId, "1"));
		assertNotNull("message CLEAR_CACHE", Action.CLEAR_CACHE_KEY.execute(collector, null, null,
				counterName, sessionId, threadId, jobId, cacheId, "3"));
		CacheManager.getInstance().removeCache(cacheId);

		final javax.cache.CacheManager jcacheManager = Caching.getCachingProvider()
				.getCacheManager();
		if (jcacheManager.getCache(cacheId) == null) {
			final MutableConfiguration<Object, Object> conf = new MutableConfiguration<Object, Object>();
			conf.setManagementEnabled(true);
			conf.setStatisticsEnabled(true);
			jcacheManager.createCache(cacheId, conf);
		}
		assertNotNull("message CLEAR_CACHES", Action.CLEAR_JCACHES.execute(collector, null, null,
				counterName, sessionId, threadId, jobId, cacheId));
		assertNotNull("message CLEAR_CACHE", Action.CLEAR_JCACHE.execute(collector, null, null,
				counterName, sessionId, threadId, jobId, cacheId));
		assertNotNull("message CLEAR_CACHE", Action.CLEAR_JCACHE.execute(collector, null, null,
				counterName, sessionId, threadId, jobId, "inconnu"));
		assertNotNull("message CLEAR_CACHE", Action.CLEAR_JCACHE_KEY.execute(collector, null, null,
				counterName, sessionId, threadId, jobId, "inconnu", "inconnue"));
		assertNotNull("message CLEAR_CACHE", Action.CLEAR_JCACHE_KEY.execute(collector, null, null,
				counterName, sessionId, threadId, jobId, cacheId, "inconnue"));
		jcacheManager.getCache(cacheId).put("1", "value");
		jcacheManager.getCache(cacheId).put("2", "value");
		jcacheManager.getCache(cacheId).put(Integer.valueOf(3), "value");
		assertNotNull("message CLEAR_CACHE", Action.CLEAR_JCACHE_KEY.execute(collector, null, null,
				counterName, sessionId, threadId, jobId, cacheId, "inconnue"));
		assertNotNull("message CLEAR_CACHE", Action.CLEAR_JCACHE_KEY.execute(collector, null, null,
				counterName, sessionId, threadId, jobId, cacheId, "1"));
		assertNotNull("message CLEAR_CACHE", Action.CLEAR_JCACHE_KEY.execute(collector, null, null,
				counterName, sessionId, threadId, jobId, cacheId, "3"));
		jcacheManager.destroyCache(cacheId);
	}

	private void invalidateSessions(Collector collector, String counterName, String sessionId,
			String threadId, String jobId, String cacheId, HttpSession session) throws IOException {
		assertNotNull("message INVALIDATE_SESSIONS", Action.INVALIDATE_SESSIONS.execute(collector,
				null, session, counterName, sessionId, threadId, jobId, cacheId));
		assertNotNull("message INVALIDATE_SESSION", Action.INVALIDATE_SESSION.execute(collector,
				null, null, counterName, sessionId, threadId, jobId, cacheId));

		assertNotNull("message INVALIDATE_SESSION", Action.LOGOUT.execute(collector, null, session,
				counterName, sessionId, threadId, jobId, cacheId));
		assertNotNull("message INVALIDATE_SESSION", Action.LOGOUT.execute(collector, null, null,
				counterName, sessionId, threadId, jobId, cacheId));
	}

	private void mailTest(Collector collector) throws IOException {
		final ServletContext context = createNiceMock(ServletContext.class);
		expect(context.getMajorVersion()).andReturn(2).anyTimes();
		expect(context.getMinorVersion()).andReturn(5).anyTimes();
		expect(context.getServletContextName()).andReturn("test webapp").anyTimes();
		expect(context.getServerInfo()).andReturn("mockJetty").anyTimes();
		expect(context.getContextPath()).andReturn("/test").anyTimes();
		replay(context);
		Parameters.initialize(context);
		try {
			Action.MAIL_TEST.execute(collector, null, null, null, null, null, null, null);
		} catch (final IllegalStateException e) {
			// ok, il manque un paramètre
			assertNotNull(e.toString(), e);
		}
		Utils.setProperty(Parameter.MAIL_SESSION, "mail/Session");
		try {
			Action.MAIL_TEST.execute(collector, null, null, null, null, null, null, null);
		} catch (final IllegalStateException e) {
			// ok, il manque encore un paramètre
			assertNotNull(e.toString(), e);
		}
		Utils.setProperty(Parameter.ADMIN_EMAILS, "admin@blah blah.fr");
		try {
			Action.MAIL_TEST.execute(collector, null, null, null, null, null, null, null);
		} catch (final Exception e) {
			// ok, de toute façon il n'y a pas de Session dans JNDI
			assertNotNull(e.toString(), e);
		}
		verify(context);
	}

	private void jobs(Collector collector, String counterName, String sessionId, String threadId,
			String jobId, String cacheId) throws IOException, SchedulerException {
		try {
			assertNotNull("message PAUSE_JOB 1", Action.PAUSE_JOB.execute(collector, null, null,
					counterName, sessionId, threadId, jobId, cacheId));
		} catch (final IllegalArgumentException e) {
			assertNotNull(e.toString(), e);
		}
		try {
			assertNotNull("message RESUME_JOB 1", Action.RESUME_JOB.execute(collector, null, null,
					counterName, sessionId, threadId, jobId, cacheId));
		} catch (final IllegalArgumentException e) {
			assertNotNull(e.toString(), e);
		}
		assertNotNull("message PAUSE_JOB 2", Action.PAUSE_JOB.execute(collector, null, null,
				counterName, sessionId, threadId, ALL, cacheId));
		assertNotNull("message RESUME_JOB 2", Action.RESUME_JOB.execute(collector, null, null,
				counterName, sessionId, threadId, ALL, cacheId));
		assertNull("message PAUSE_JOB 3", Action.PAUSE_JOB.execute(collector, null, null,
				counterName, sessionId, threadId, "nopid_noip_id", cacheId));
		assertNull("message RESUME_JOB 3", Action.RESUME_JOB.execute(collector, null, null,
				counterName, sessionId, threadId, "nopid_noip_id", cacheId));
		assertNull("message PAUSE_JOB 4", Action.PAUSE_JOB.execute(collector, null, null,
				counterName, sessionId, threadId, PID.getPID() + "_noip_id", cacheId));
		assertNull("message RESUME_JOB 4", Action.RESUME_JOB.execute(collector, null, null,
				counterName, sessionId, threadId, PID.getPID() + "_noip_id", cacheId));
		String globalJobId = PID.getPID() + '_' + Parameters.getHostAddress() + '_' + 10000;
		assertNotNull("message PAUSE_JOB 5", Action.PAUSE_JOB.execute(collector, null, null,
				counterName, sessionId, threadId, globalJobId, cacheId));
		assertNotNull("message RESUME_JOB 5", Action.RESUME_JOB.execute(collector, null, null,
				counterName, sessionId, threadId, globalJobId, cacheId));

		//Grab the Scheduler instance from the Factory
		final Scheduler scheduler = StdSchedulerFactory.getDefaultScheduler();

		try {
			// and start it off
			scheduler.start();

			//Define job instance
			final Random random = new Random();

			final JobDetail job = new JobDetail("job" + random.nextInt(), null, JobTestImpl.class);

			//Define a Trigger that will fire "later"
			final Trigger trigger = new SimpleTrigger("trigger" + random.nextInt(), null,
					new Date(System.currentTimeMillis() + 60000));
			//Schedule the job with the trigger
			scheduler.scheduleJob(job, trigger);

			assertNotNull("message PAUSE_JOB 5", Action.PAUSE_JOB.execute(collector, null, null,
					counterName, sessionId, threadId, globalJobId, cacheId));
			assertNotNull("message RESUME_JOB 5", Action.RESUME_JOB.execute(collector, null, null,
					counterName, sessionId, threadId, globalJobId, cacheId));

			globalJobId = PID.getPID() + '_' + Parameters.getHostAddress() + '_'
					+ job.getFullName().hashCode();
			assertNotNull("message PAUSE_JOB 6", Action.PAUSE_JOB.execute(collector, null, null,
					counterName, sessionId, threadId, globalJobId, cacheId));
			assertNotNull("message RESUME_JOB 6", Action.RESUME_JOB.execute(collector, null, null,
					counterName, sessionId, threadId, globalJobId, cacheId));

			assertNotNull("message PAUSE_JOB 7", Action.PAUSE_JOB.execute(collector, null, null,
					counterName, sessionId, threadId, ALL, cacheId));
			assertNotNull("message RESUME_JOB 7", Action.RESUME_JOB.execute(collector, null, null,
					counterName, sessionId, threadId, ALL, cacheId));
		} finally {
			scheduler.shutdown();
		}
	}

	private void killThread(Collector collector, String counterName, String sessionId,
			String threadId, String jobId, String cacheId) throws IOException {
		try {
			assertNull("message KILL_THREAD 1", Action.KILL_THREAD.execute(collector, null, null,
					counterName, sessionId, threadId, jobId, cacheId));
		} catch (final IllegalArgumentException e) {
			assertNotNull(e.toString(), e);
		}
		assertNull("message KILL_THREAD 2", Action.KILL_THREAD.execute(collector, null, null,
				counterName, sessionId, "nopid_noip_id", jobId, cacheId));
		assertNull("message KILL_THREAD 3", Action.KILL_THREAD.execute(collector, null, null,
				counterName, sessionId, PID.getPID() + "_noip_id", jobId, cacheId));
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
		assertNotNull("message KILL_THREAD 4", Action.KILL_THREAD.execute(collector, null, null,
				counterName, sessionId, globalThreadId, jobId, cacheId));
		globalThreadId = PID.getPID() + '_' + Parameters.getHostAddress() + '_' + 10000;
		assertNotNull("message KILL_THREAD 5", Action.KILL_THREAD.execute(collector, null, null,
				counterName, sessionId, globalThreadId, jobId, cacheId));

		final Thread myThread2 = new Thread(new Runnable() {
			/** {@inheritDoc} */
			@Override
			public void run() {
				try {
					Thread.sleep(10000);
				} catch (final InterruptedException e) {
					assertNotNull(e.toString(), e);
				}
			}
		});
		myThread2.setName("javamelody test 2");
		myThread2.start();
		final String globalThreadId2 = PID.getPID() + '_' + Parameters.getHostAddress() + '_'
				+ myThread2.getId();
		assertEquals("message KILL_THREAD 6", "I will not kill myself", Action.KILL_THREAD.execute(
				collector, null, null, counterName, sessionId, globalThreadId2, jobId, cacheId));
	}

	private void sendThreadInterrupt(Collector collector, String counterName, String sessionId,
			String threadId, String jobId, String cacheId) throws IOException {
		try {
			assertNull("message SEND_THREAD_INTERRUPT 1", Action.SEND_THREAD_INTERRUPT.execute(
					collector, null, null, counterName, sessionId, threadId, jobId, cacheId));
		} catch (final IllegalArgumentException e) {
			assertNotNull(e.toString(), e);
		}
		assertNull("message SEND_THREAD_INTERRUPT 2", Action.SEND_THREAD_INTERRUPT.execute(
				collector, null, null, counterName, sessionId, "nopid_noip_id", jobId, cacheId));
		assertNull("message SEND_THREAD_INTERRUPT 3",
				Action.SEND_THREAD_INTERRUPT.execute(collector, null, null, counterName, sessionId,
						PID.getPID() + "_noip_id", jobId, cacheId));
		final Thread myThread = new Thread(new Runnable() {
			/** {@inheritDoc} */
			@Override
			public void run() {
				try {
					Thread.sleep(10000);
				} catch (final InterruptedException e) {
					assertNotNull(e.toString(), e);
				}
			}
		});
		myThread.setName("thread test 2");
		myThread.start();
		String globalThreadId = PID.getPID() + '_' + Parameters.getHostAddress() + '_'
				+ myThread.getId();
		assertNotNull("message SEND_THREAD_INTERRUPT 4", Action.SEND_THREAD_INTERRUPT.execute(
				collector, null, null, counterName, sessionId, globalThreadId, jobId, cacheId));
		globalThreadId = PID.getPID() + '_' + Parameters.getHostAddress() + '_' + 10000;
		assertNotNull("message SEND_THREAD_INTERRUPT 5", Action.SEND_THREAD_INTERRUPT.execute(
				collector, null, null, counterName, sessionId, globalThreadId, jobId, cacheId));

		final Thread myThread2 = new Thread(new Runnable() {
			/** {@inheritDoc} */
			@Override
			public void run() {
				try {
					Thread.sleep(10000);
				} catch (final InterruptedException e) {
					assertNotNull(e.toString(), e);
				}
			}
		});
		myThread2.setName("javamelody test 2");
		myThread2.start();
		final String globalThreadId2 = PID.getPID() + '_' + Parameters.getHostAddress() + '_'
				+ myThread2.getId();
		assertEquals("message SEND_THREAD_INTERRUPT 6", "I will not interrupt myself",
				Action.SEND_THREAD_INTERRUPT.execute(collector, null, null, counterName, sessionId,
						globalThreadId2, jobId, cacheId));
	}

	/** Test. */
	@Test
	public void testCheckSystemActionsEnabled() {
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
