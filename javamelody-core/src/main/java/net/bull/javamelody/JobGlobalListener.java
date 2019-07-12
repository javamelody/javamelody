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

import java.io.PrintWriter;
import java.io.StringWriter;

import org.quartz.JobDetail;
import org.quartz.JobExecutionContext;
import org.quartz.JobExecutionException;
import org.quartz.JobListener;
import org.quartz.SchedulerException;

import net.bull.javamelody.internal.common.LOG;
import net.bull.javamelody.internal.model.Counter;
import net.bull.javamelody.internal.model.QuartzAdapter;

/**
 * Listener sur les exécutions de jobs quartz, configuré automatiquement par {@link MonitoringFilter}.
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
			QuartzAdapter.getSingleton().addGlobalJobListener(jobGlobalListener);
			LOG.debug("job global listener initialized");
		} catch (final SchedulerException e) {
			// initialisation du JobGlobalListener échouée, tant pis, il n'y aura pas les temps pour quartz
			// (cela peut arriver, en particulier en développement dans Grails)
			LOG.info("initialization of job global listener failed, skipping", e);
		}
	}

	static void destroyJobGlobalListener() {
		try {
			QuartzAdapter.getSingleton().removeGlobalJobListener(JobGlobalListener.class);
		} catch (final SchedulerException e) {
			throw new IllegalStateException(e);
		}
	}

	/** {@inheritDoc} */
	@Override
	public void jobToBeExecuted(JobExecutionContext context) {
		// on calcule nous même le fullName du job pour être sûr que c'est le même que celui calculé
		// dans HtmlJobInformationsReport.getCounterRequest
		final JobDetail jobDetail = QuartzAdapter.getSingleton().getContextJobDetail(context);
		final String jobFullName = QuartzAdapter.getSingleton().getJobFullName(jobDetail);
		JOB_COUNTER.bindContextIncludingCpu(jobFullName);
	}

	/** {@inheritDoc} */
	@Override
	public void jobExecutionVetoed(JobExecutionContext context) {
		JOB_COUNTER.unbindContext();
	}

	/** {@inheritDoc} */
	@Override
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
	@Override
	public String getName() {
		return getClass().getName();
	}
}
