/*
 * Copyright 2008-2012 by Emeric Vernat
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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

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
	String getJobFullName(JobDetail jobDetail) {
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
	JobDetail getContextJobDetail(JobExecutionContext context) {
		return context.getJobDetail();
	}

	@Override
	Date getContextFireTime(JobExecutionContext context) {
		return context.getFireTime();
	}

	@Override
	void addGlobalJobListener(JobListener jobGlobalListener) throws SchedulerException {
		final Scheduler defaultScheduler;
		final List<Matcher<JobKey>> allJobs = new ArrayList<Matcher<JobKey>>();
		allJobs.add(EverythingMatcher.allJobs());
		if (Boolean.parseBoolean(Parameters
				.getParameter(Parameter.QUARTZ_DEFAULT_LISTENER_DISABLED))) {
			defaultScheduler = null;
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
	void removeGlobalJobListener() throws SchedulerException {
		for (final Scheduler scheduler : JobInformations.getAllSchedulers()) {
			final ListenerManager listenerManager = scheduler.getListenerManager();
			final List<JobListener> globalJobListeners = listenerManager.getJobListeners();
			for (final JobListener jobListener : new ArrayList<JobListener>(globalJobListeners)) {
				if (jobListener instanceof JobGlobalListener) {
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
