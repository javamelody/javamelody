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
import static org.junit.Assert.assertSame;

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

			assertSame("requestsCount", 10, jobCounter.getRequestsCount());
		} finally {
			scheduler.shutdown();
			JobGlobalListener.destroyJobGlobalListener();
		}
	}
}
