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

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.util.Collections;
import java.util.Locale;
import java.util.Timer;

import net.sf.ehcache.CacheManager;

import org.junit.Test;

/**
 * Test unitaire de la classe Action.
 * @author Emeric Vernat
 */
public class TestAction {
	/** Test. */
	@Test
	public void testValueOfIgnoreCase() {
		for (final Action action : Action.values()) {
			assertSame("same", action, Action.valueOfIgnoreCase(action.toString().toLowerCase(
					Locale.getDefault())));
		}
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testExecute() throws IOException {
		final Counter counter = new Counter("test html report", null);
		counter.addRequest("test1", 0, 1, false, 1000);
		counter.addRequest("test2", 1000, 900, false, 1000);
		counter.addRequest("test3", 10000, 1000, true, 10000);
		final Timer timer = new Timer("test", true);
		try {
			final Collector collector = new Collector("test", Collections.singletonList(counter),
					timer);
			final String counterName = counter.getName();
			final String sessionId = "sessionId";
			final String threadId = "threadId";
			final String jobId = "jobId";

			assertNotNull("message GC", Action.GC.execute(collector, counterName, sessionId,
					threadId, jobId));
			assertNotNull("message CLEAR_COUNTER", Action.CLEAR_COUNTER.execute(collector,
					counterName, sessionId, threadId, jobId));
			if (CacheManager.getInstance().getCache("test clear") == null) {
				CacheManager.getInstance().addCache("test clear");
			}
			assertNotNull("message CLEAR_CACHES", Action.CLEAR_CACHES.execute(collector,
					counterName, sessionId, threadId, jobId));
			final String heapDump1 = Action.HEAP_DUMP.execute(collector, counterName, sessionId,
					threadId, jobId);
			assertNotNull("message HEAP_DUMP", heapDump1);
			String heapDump2;
			// on le refait une deuxième fois dans la même seconde pour tester le nom du fichier
			do {
				heapDump2 = Action.HEAP_DUMP.execute(collector, counterName, sessionId, threadId,
						jobId);
				assertNotNull("message HEAP_DUMP", heapDump2);
			} while (heapDump1.equals(heapDump2));
			for (final File file : Parameters.TEMPORARY_DIRECTORY.listFiles()) {
				if (!file.isDirectory() && file.getName().startsWith("heapdump") && !file.delete()) {
					file.deleteOnExit();
				}
			}
			assertNotNull("message INVALIDATE_SESSIONS", Action.INVALIDATE_SESSIONS.execute(
					collector, counterName, sessionId, threadId, jobId));
			assertNotNull("message INVALIDATE_SESSION", Action.INVALIDATE_SESSION.execute(
					collector, counterName, sessionId, threadId, jobId));

			try {
				assertNull("message KILL_THREAD", Action.KILL_THREAD.execute(collector,
						counterName, sessionId, threadId, jobId));
			} catch (final IllegalArgumentException e) {
				assertNotNull(e.toString(), e);
			}
			assertNull("message KILL_THREAD", Action.KILL_THREAD.execute(collector, counterName,
					sessionId, "nopid_noip_id", jobId));
			assertNull("message KILL_THREAD", Action.KILL_THREAD.execute(collector, counterName,
					sessionId, PID.getPID() + "_noip_id", jobId));
			final Thread myThread = new Thread(new Runnable() {
				/** {@inheritDoc} */
				public void run() {
					try {
						Thread.sleep(10000);
					} catch (final InterruptedException e) {
						throw new RuntimeException(e);
					}
				}
			});
			myThread.setName("thread test");
			myThread.start();
			String globalThreadId = PID.getPID() + '_' + Parameters.getHostAddress() + '_'
					+ myThread.getId();
			assertNotNull("message KILL_THREAD", Action.KILL_THREAD.execute(collector, counterName,
					sessionId, globalThreadId, jobId));
			globalThreadId = PID.getPID() + '_' + Parameters.getHostAddress() + '_' + 10000;
			assertNotNull("message KILL_THREAD", Action.KILL_THREAD.execute(collector, counterName,
					sessionId, globalThreadId, jobId));
		} finally {
			timer.cancel();
		}
	}

	/** Test. */
	@Test
	public void testCheckSystemActionsEnabled() {
		// remove pour être sûr par rapport aux autres tests unitaires
		System.getProperties().remove(
				Parameters.PARAMETER_SYSTEM_PREFIX + Parameter.SYSTEM_ACTIONS_ENABLED.getCode());
		boolean systemActionsEnabled = true;
		try {
			Action.checkSystemActionsEnabled();
		} catch (final Exception e) {
			systemActionsEnabled = false;
		}
		// test: par défaut les actions systèmes sont considérées désactivées
		if (systemActionsEnabled) {
			fail("checkSystemActionsEnabled");
		}
		System.setProperty(Parameters.PARAMETER_SYSTEM_PREFIX
				+ Parameter.SYSTEM_ACTIONS_ENABLED.getCode(), "true");
		systemActionsEnabled = true;
		try {
			Action.checkSystemActionsEnabled();
		} catch (final Exception e) {
			systemActionsEnabled = false;
		} finally {
			System.setProperty(Parameters.PARAMETER_SYSTEM_PREFIX
					+ Parameter.SYSTEM_ACTIONS_ENABLED.getCode(), "false");
		}
		if (!systemActionsEnabled) {
			fail("checkSystemActionsEnabled");
		}
	}
}
