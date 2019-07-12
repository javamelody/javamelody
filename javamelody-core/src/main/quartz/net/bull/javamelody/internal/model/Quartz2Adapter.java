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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import net.bull.javamelody.Parameter;
import net.bull.javamelody.internal.common.LOG;

import org.quartz.CronTrigger;
import org.quartz.JobDetail;
import org.quartz.JobExecutionContext;
import org.quartz.JobKey;
import org.quartz.JobListener;
import org.quartz.ListenerManager;
import org.quartz.Matcher;
import org.quartz.Scheduler;
import org.quartz.SchedulerException;
import org.quartz.SimpleTrigger;
import org.quartz.Trigger;
import org.quartz.impl.StdSchedulerFactory;
import org.quartz.impl.matchers.EverythingMatcher;
import org.quartz.impl.matchers.GroupMatcher;

/**
 * Classe ayant la même API, que le QuartzAdapter par défaut des versions avant 2,<br/>
 * mais avec une autre implémentation adaptée aux versions 2.0.0, 2.1.2 et suivantes.
 *
 * @author rogerc@customercentrix.com
 * @author Emeric Vernat
 */
class Quartz2Adapter extends QuartzAdapter {
	protected Quartz2Adapter() {
		super();
	}

	@Override
	String getJobName(JobDetail jobDetail) {
		return jobDetail.getKey().getName();
	}

	@Override
	String getJobGroup(JobDetail jobDetail) {
		return jobDetail.getKey().getGroup();
	}

	@Override
	public String getJobFullName(JobDetail jobDetail) {
		return getJobGroup(jobDetail) + '.' + getJobName(jobDetail);
	}

	// les getters suivants sont identiques à ceux de la classe parente,
	// mais ils sont nécessaires ici car les types des objets ont changé de classes à interfaces à partir de Quartz v2
	@Override
	String getJobDescription(JobDetail jobDetail) {
		return jobDetail.getDescription();
	}

	@Override
	Class<?> getJobClass(JobDetail jobDetail) {
		return jobDetail.getJobClass();
	}

	@Override
	Date getTriggerPreviousFireTime(Trigger trigger) {
		return trigger.getPreviousFireTime();
	}

	@Override
	Date getTriggerNextFireTime(Trigger trigger) {
		return trigger.getNextFireTime();
	}
	
	@Override
	String getCronTriggerExpression(CronTrigger trigger) {
		// getCronExpression gives a PMD false+
		return trigger.getCronExpression(); // NOPMD
	}

	@Override
	long getSimpleTriggerRepeatInterval(SimpleTrigger trigger) {
		return trigger.getRepeatInterval(); // NOPMD
	}

	@Override
	public JobDetail getContextJobDetail(JobExecutionContext context) {
		return context.getJobDetail();
	}

	@Override
	Date getContextFireTime(JobExecutionContext context) {
		return context.getFireTime();
	}

	@Override
	public void addGlobalJobListener(JobListener jobGlobalListener) throws SchedulerException {
		final Scheduler defaultScheduler;
		final List<Matcher<JobKey>> allJobs = new ArrayList<Matcher<JobKey>>();
		allJobs.add(EverythingMatcher.allJobs());
		if (Parameter.QUARTZ_DEFAULT_LISTENER_DISABLED.getValueAsBoolean()) {
			defaultScheduler = null;
			LOG.debug("Initialization of Quartz default listener has been disabled");
		} else {
			defaultScheduler = StdSchedulerFactory.getDefaultScheduler();
			defaultScheduler.getListenerManager().addJobListener(jobGlobalListener, allJobs);
		}
		for (final Scheduler scheduler : JobInformations.getAllSchedulers()) {
			if (scheduler != defaultScheduler) {
				scheduler.getListenerManager().addJobListener(jobGlobalListener, allJobs);
			}
		}
	}

	@Override
	public void removeGlobalJobListener(Class<? extends JobListener> jobListenerClass)
			throws SchedulerException {
		for (final Scheduler scheduler : JobInformations.getAllSchedulers()) {
			final ListenerManager listenerManager = scheduler.getListenerManager();
			final List<JobListener> globalJobListeners = listenerManager.getJobListeners();
			for (final JobListener jobListener : new ArrayList<JobListener>(globalJobListeners)) {
				if (jobListenerClass.isInstance(jobListener)) {
					listenerManager.removeJobListener(jobListener.getName());
				}
			}
		}
	}

	@Override
	List<JobDetail> getAllJobsOfScheduler(Scheduler scheduler) throws SchedulerException {
		final List<JobDetail> result = new ArrayList<JobDetail>();
		for (final String jobGroupName : scheduler.getJobGroupNames()) {
			final GroupMatcher<JobKey> groupMatcher = GroupMatcher.groupEquals(jobGroupName);
			for (final JobKey jobKey : scheduler.getJobKeys(groupMatcher)) {
				final JobDetail jobDetail;
				try {
					jobDetail = scheduler.getJobDetail(jobKey);
					// le job peut être terminé et supprimé depuis la ligne ci-dessus
					if (jobDetail != null) {
						result.add(jobDetail);
					}
				} catch (final Exception e) {
					// si les jobs sont persistés en base de données, il peut y avoir une exception
					// dans getJobDetail, par exemple si la classe du job n'existe plus dans l'application
					LOG.debug(e.toString(), e);
				}
			}
		}
		return result;
	}

	@Override
	@SuppressWarnings("unchecked")
	List<Trigger> getTriggersOfJob(JobDetail jobDetail, Scheduler scheduler)
			throws SchedulerException {
		return (List<Trigger>) scheduler.getTriggersOfJob(jobDetail.getKey());
	}

	@Override
	boolean isTriggerPaused(Trigger trigger, Scheduler scheduler) throws SchedulerException {
		return scheduler.getTriggerState(trigger.getKey()) == Trigger.TriggerState.PAUSED;
	}

	@Override
	void pauseJob(JobDetail jobDetail, Scheduler scheduler) throws SchedulerException {
		scheduler.pauseJob(jobDetail.getKey());
	}

	@Override
	void resumeJob(JobDetail jobDetail, Scheduler scheduler) throws SchedulerException {
		scheduler.resumeJob(jobDetail.getKey());
	}
}
