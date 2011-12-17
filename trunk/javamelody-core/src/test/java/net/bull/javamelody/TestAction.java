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
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.util.Collections;
import java.util.Date;
import java.util.Locale;
import java.util.Random;

import net.sf.ehcache.CacheManager;

import org.junit.Before;
import org.junit.Test;
import org.quartz.JobDetail;
import org.quartz.Scheduler;
import org.quartz.SchedulerException;
import org.quartz.SimpleTrigger;
import org.quartz.Trigger;
import org.quartz.impl.StdSchedulerFactory;

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
		counter.addRequest("test1", 0, 1, false, 1000);
		counter.addRequest("test2", 1000, 900, false, 1000);
		counter.addRequest("test3", 10000, 1000, true, 10000);
		final Collector collector = new Collector("test", Collections.singletonList(counter));
		final String counterName = counter.getName();
		final String sessionId = "sessionId";
		final String threadId = "threadId";
		final String jobId = "jobId";

		assertNotNull("message GC",
				Action.GC.execute(collector, null, counterName, sessionId, threadId, jobId));
		assertNotNull("message CLEAR_COUNTER", Action.CLEAR_COUNTER.execute(collector, null,
				counterName, sessionId, threadId, jobId));
		assertNotNull("message CLEAR_COUNTER",
				Action.CLEAR_COUNTER.execute(collector, null, ALL, sessionId, threadId, jobId));
		if (CacheManager.getInstance().getCache("test clear") == null) {
			CacheManager.getInstance().addCache("test clear");
		}
		assertNotNull("message CLEAR_CACHES", Action.CLEAR_CACHES.execute(collector, null,
				counterName, sessionId, threadId, jobId));
		final String heapDump1 = Action.HEAP_DUMP.execute(collector, null, counterName, sessionId,
				threadId, jobId);
		assertNotNull("message HEAP_DUMP", heapDump1);
		String heapDump2;
		// on le refait une deuxième fois dans la même seconde pour tester le nom du fichier
		do {
			heapDump2 = Action.HEAP_DUMP.execute(collector, null, counterName, sessionId, threadId,
					jobId);
			assertNotNull("message HEAP_DUMP", heapDump2);
		} while (heapDump1.equals(heapDump2));
		for (final File file : Parameters.TEMPORARY_DIRECTORY.listFiles()) {
			if (!file.isDirectory() && file.getName().startsWith("heapdump") && !file.delete()) {
				file.deleteOnExit();
			}
		}
		assertNotNull("message INVALIDATE_SESSIONS", Action.INVALIDATE_SESSIONS.execute(collector,
				null, counterName, sessionId, threadId, jobId));
		assertNotNull("message INVALIDATE_SESSION", Action.INVALIDATE_SESSION.execute(collector,
				null, counterName, sessionId, threadId, jobId));

		killThread(collector, counterName, sessionId, threadId, jobId);

		jobs(collector, counterName, sessionId, threadId, jobId);

		mailTest(collector);
	}

	private void mailTest(Collector collector) throws IOException {
		try {
			Action.MAIL_TEST.execute(collector, null, null, null, null, null);
		} catch (final IllegalStateException e) {
			// ok, il manque un paramètre
			assertNotNull(e.toString(), e);
		}
		Utils.setProperty(Parameter.MAIL_SESSION, "mail/Session");
		try {
			Action.MAIL_TEST.execute(collector, null, null, null, null, null);
		} catch (final IllegalStateException e) {
			// ok, il manque encore un paramètre
			assertNotNull(e.toString(), e);
		}
		Utils.setProperty(Parameter.ADMIN_EMAILS, "admin@blah blah.fr");
		try {
			Action.MAIL_TEST.execute(collector, null, null, null, null, null);
		} catch (final Exception e) {
			// ok, de toute façon il n'y a pas de Session dans JNDI
			assertNotNull(e.toString(), e);
		}
	}

	private void jobs(Collector collector, String counterName, String sessionId, String threadId,
			String jobId) throws IOException, SchedulerException {
		try {
			assertNotNull("message PAUSE_JOB 1", Action.PAUSE_JOB.execute(collector, null,
					counterName, sessionId, threadId, jobId));
		} catch (final IllegalArgumentException e) {
			assertNotNull(e.toString(), e);
		}
		try {
			assertNotNull("message RESUME_JOB 1", Action.RESUME_JOB.execute(collector, null,
					counterName, sessionId, threadId, jobId));
		} catch (final IllegalArgumentException e) {
			assertNotNull(e.toString(), e);
		}
		assertNotNull("message PAUSE_JOB 2",
				Action.PAUSE_JOB.execute(collector, null, counterName, sessionId, threadId, ALL));
		assertNotNull("message RESUME_JOB 2",
				Action.RESUME_JOB.execute(collector, null, counterName, sessionId, threadId, ALL));
		assertNull("message PAUSE_JOB 3", Action.PAUSE_JOB.execute(collector, null, counterName,
				sessionId, threadId, "nopid_noip_id"));
		assertNull("message RESUME_JOB 3", Action.RESUME_JOB.execute(collector, null, counterName,
				sessionId, threadId, "nopid_noip_id"));
		assertNull(
				"message PAUSE_JOB 4",
				Action.PAUSE_JOB.execute(collector, null, counterName, sessionId, threadId,
						PID.getPID() + "_noip_id"));
		assertNull(
				"message RESUME_JOB 4",
				Action.RESUME_JOB.execute(collector, null, counterName, sessionId, threadId,
						PID.getPID() + "_noip_id"));
		String globalJobId = PID.getPID() + '_' + Parameters.getHostAddress() + '_' + 10000;
		assertNotNull("message PAUSE_JOB 5", Action.PAUSE_JOB.execute(collector, null, counterName,
				sessionId, threadId, globalJobId));
		assertNotNull("message RESUME_JOB 5", Action.RESUME_JOB.execute(collector, null,
				counterName, sessionId, threadId, globalJobId));

		//Grab the Scheduler instance from the Factory
		final Scheduler scheduler = StdSchedulerFactory.getDefaultScheduler();

		try {
			// and start it off
			scheduler.start();

			//Define job instance
			final Random random = new Random();

			final JobDetail job = new JobDetail("job" + random.nextInt(), null, JobTestImpl.class);

			//Define a Trigger that will fire "later"
			final Trigger trigger = new SimpleTrigger("trigger" + random.nextInt(), null, new Date(
					System.currentTimeMillis() + 60000));
			//Schedule the job with the trigger
			scheduler.scheduleJob(job, trigger);

			assertNotNull("message PAUSE_JOB 5", Action.PAUSE_JOB.execute(collector, null,
					counterName, sessionId, threadId, globalJobId));
			assertNotNull("message RESUME_JOB 5", Action.RESUME_JOB.execute(collector, null,
					counterName, sessionId, threadId, globalJobId));

			globalJobId = PID.getPID() + '_' + Parameters.getHostAddress() + '_'
					+ job.getFullName().hashCode();
			assertNotNull("message PAUSE_JOB 6", Action.PAUSE_JOB.execute(collector, null,
					counterName, sessionId, threadId, globalJobId));
			assertNotNull("message RESUME_JOB 6", Action.RESUME_JOB.execute(collector, null,
					counterName, sessionId, threadId, globalJobId));
		} finally {
			scheduler.shutdown();
		}
	}

	private void killThread(Collector collector, String counterName, String sessionId,
			String threadId, String jobId) throws IOException {
		try {
			assertNull("message KILL_THREAD 1", Action.KILL_THREAD.execute(collector, null,
					counterName, sessionId, threadId, jobId));
		} catch (final IllegalArgumentException e) {
			assertNotNull(e.toString(), e);
		}
		assertNull("message KILL_THREAD 2", Action.KILL_THREAD.execute(collector, null,
				counterName, sessionId, "nopid_noip_id", jobId));
		assertNull(
				"message KILL_THREAD 3",
				Action.KILL_THREAD.execute(collector, null, counterName, sessionId, PID.getPID()
						+ "_noip_id", jobId));
		final Thread myThread = new Thread(new Runnable() {
			/** {@inheritDoc} */
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
		assertNotNull("message KILL_THREAD 4", Action.KILL_THREAD.execute(collector, null,
				counterName, sessionId, globalThreadId, jobId));
		globalThreadId = PID.getPID() + '_' + Parameters.getHostAddress() + '_' + 10000;
		assertNotNull("message KILL_THREAD 5", Action.KILL_THREAD.execute(collector, null,
				counterName, sessionId, globalThreadId, jobId));
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
		Utils.setProperty(Parameter.SYSTEM_ACTIONS_ENABLED, "true");
		systemActionsEnabled = true;
		try {
			Action.checkSystemActionsEnabled();
		} catch (final Exception e) {
			systemActionsEnabled = false;
		}
		if (!systemActionsEnabled) {
			fail("checkSystemActionsEnabled");
		}
	}
}
