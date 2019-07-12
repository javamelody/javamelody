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
package net.bull.javamelody;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.Date;
import java.util.Random;

import org.junit.Before;
import org.junit.Test;
import org.quartz.JobDetail;
import org.quartz.Scheduler;
import org.quartz.SchedulerException;
import org.quartz.SimpleTrigger;
import org.quartz.Trigger;
import org.quartz.impl.StdSchedulerFactory;

import net.bull.javamelody.internal.model.Counter;

/**
 * Test unitaire de la classe JobGlobalListener.
 * @author Emeric Vernat
 */
public class TestJobGlobalListener {
	/** Check. */
	@Before
	public void setUp() {
		Utils.initialize();
	}

	/** Test. */
	@Test
	public void testGetJobCounter() {
		assertNotNull("getJobCounter", JobGlobalListener.getJobCounter());
	}

	/** Test.
	 * @throws SchedulerException e
	 * @throws InterruptedException e */
	@Test
	public void testJobGlobalListener() throws SchedulerException, InterruptedException {
		final Counter jobCounter = JobGlobalListener.getJobCounter();
		jobCounter.clear();
		jobCounter.setDisplayed(true);
		// job quartz
		JobGlobalListener.initJobGlobalListener();

		//Grab the Scheduler instance from the Factory
		final Scheduler scheduler = StdSchedulerFactory.getDefaultScheduler();

		try {
			// and start it off
			scheduler.start();

			final Random random = new Random();
			// on lance 10 jobs pour être à peu près sûr qu'il y en a un qui fait une erreur
			// (aléatoirement il y en a 2/10 qui font une erreur)
			for (int i = 0; i < 10; i++) {
				//Define job instance
				final JobDetail job = new JobDetail("job" + random.nextInt(), null,
						JobTestImpl.class);

				//Define a Trigger that will fire "now"
				final Trigger trigger = new SimpleTrigger("trigger" + random.nextInt(), null,
						new Date());
				//Schedule the job with the trigger
				scheduler.scheduleJob(job, trigger);
			}

			// JobTestImpl fait un sleep de 2s au plus, donc on attend les jobs pour les compter
			Thread.sleep(3000);

			assertTrue("requestsCount", jobCounter.getRequestsCount() > 0);
		} finally {
			scheduler.shutdown();
			JobGlobalListener.destroyJobGlobalListener();
		}
	}

	/** Test. */
	@Test
	public void testExecutionVetoed() {
		new JobGlobalListener().jobExecutionVetoed(null);
	}

	/** Test. */
	@Test
	public void testDefaultListenerDisabled() {
		Utils.setProperty(Parameter.QUARTZ_DEFAULT_LISTENER_DISABLED, "true");
		JobGlobalListener.initJobGlobalListener();
		JobGlobalListener.destroyJobGlobalListener();
	}
}
