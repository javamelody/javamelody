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

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;

import org.quartz.JobDetail;
import org.quartz.JobExecutionContext;
import org.quartz.JobExecutionException;
import org.quartz.JobListener;
import org.quartz.Scheduler;
import org.quartz.SchedulerException;
import org.quartz.impl.StdSchedulerFactory;

/**
 * Listener sur les exécutions de jobs quartz, configuré automatiquement par MonitoringFilter.
 * @author Emeric Vernat
 */
final class JobGlobalListener implements JobListener {
	private static final Counter JOB_COUNTER = new Counter(Counter.JOB_COUNTER_NAME, "jobs.png",
			JdbcWrapper.SINGLETON.getSqlCounter());

	static Counter getJobCounter() {
		return JOB_COUNTER;
	}

	static void initJobGlobalListener() {
		try {
			final JobGlobalListener jobGlobalListener = new JobGlobalListener();
			final Scheduler defaultScheduler = StdSchedulerFactory.getDefaultScheduler();
			defaultScheduler.addGlobalJobListener(jobGlobalListener);
			for (final Scheduler scheduler : JobInformations.getAllSchedulers()) {
				if (scheduler != defaultScheduler) {
					scheduler.addGlobalJobListener(jobGlobalListener);
				}
			}
		} catch (final SchedulerException e) {
			throw new IllegalStateException(e);
		}
	}

	@SuppressWarnings("unchecked")
	static void destroyJobGlobalListener() {
		try {
			for (final Scheduler scheduler : JobInformations.getAllSchedulers()) {
				final List<JobListener> globalJobListeners = scheduler.getGlobalJobListeners();
				for (final JobListener jobListener : new ArrayList<JobListener>(globalJobListeners)) {
					if (jobListener instanceof JobGlobalListener) {
						scheduler.removeGlobalJobListener(jobListener);
					}
				}
			}
		} catch (final SchedulerException e) {
			throw new IllegalStateException(e);
		}
	}

	/** {@inheritDoc} */
	public void jobToBeExecuted(JobExecutionContext context) {
		// on calcule nous même le fullName du job pour être sûr que c'est le même que celui calculé
		// dans HtmlJobInformationsReport.getCounterRequest
		final JobDetail jobDetail = context.getJobDetail();
		final String jobFullName = jobDetail.getGroup() + '.' + jobDetail.getName();
		JOB_COUNTER.bindContextIncludingCpu(jobFullName);
	}

	/** {@inheritDoc} */
	public void jobExecutionVetoed(JobExecutionContext context) {
		JOB_COUNTER.unbindContext();
	}

	/** {@inheritDoc} */
	public void jobWasExecuted(JobExecutionContext context, JobExecutionException jobException) {
		// sera recalculé: final long jobRunTime = context.getJobRunTime();
		final String stackTrace;
		if (jobException == null) {
			stackTrace = null;
		} else {
			final StringWriter stackTraceWriter = new StringWriter(200);
			jobException.printStackTrace(new PrintWriter(stackTraceWriter));
			stackTrace = stackTraceWriter.toString();
		}
		// on enregistre la requête dans les statistiques
		JOB_COUNTER.addRequestForCurrentContext(stackTrace);
	}

	/** {@inheritDoc} */
	public String getName() {
		return getClass().getName();
	}
}
