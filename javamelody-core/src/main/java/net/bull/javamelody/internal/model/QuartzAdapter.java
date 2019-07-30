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
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import org.quartz.CronTrigger;
import org.quartz.JobDetail;
import org.quartz.JobExecutionContext;
import org.quartz.JobListener;
import org.quartz.Scheduler;
import org.quartz.SchedulerException;
import org.quartz.SimpleTrigger;
import org.quartz.Trigger;
import org.quartz.impl.StdSchedulerFactory;

import net.bull.javamelody.Parameter;
import net.bull.javamelody.internal.common.LOG;

/**
 * Classe permettant de fournir une API adaptée aux différentes versions de <a href='http://www.quartz-scheduler.org/'>Quartz</a>.<br/>
 * L'implémentation par défaut est adaptée à Quartz avant la version 2.<br/>
 * Une autre implémentation avec la même API sera fournie et sera adaptée à la version 2 et aux suivantes.
 * @author Emeric Vernat
 */
public class QuartzAdapter {
	private static final boolean QUARTZ_2 = isQuartz2();
	private static final QuartzAdapter SINGLETON = createSingleton();

	protected QuartzAdapter() {
		super();
	}

	public static QuartzAdapter getSingleton() {
		return SINGLETON;
	}

	private static boolean isQuartz2() {
		try {
			Class.forName("org.quartz.JobKey");
			return true;
		} catch (final ClassNotFoundException e) {
			return false;
		}
	}

	private static QuartzAdapter createSingleton() {
		if (QUARTZ_2) {
			try {
				final Class<?> clazz = Class
						.forName("net.bull.javamelody.internal.model.Quartz2Adapter");
				return (QuartzAdapter) clazz.newInstance();
			} catch (final Exception e) {
				throw new IllegalStateException(e);
			}
		}
		return new QuartzAdapter();
	}

	String getJobName(JobDetail jobDetail) {
		return jobDetail.getName();
	}

	String getJobGroup(JobDetail jobDetail) {
		return jobDetail.getGroup();
	}

	public String getJobFullName(JobDetail jobDetail) {
		return getJobGroup(jobDetail) + '.' + getJobName(jobDetail);
	}

	String getJobDescription(JobDetail jobDetail) {
		return jobDetail.getDescription();
	}

	Class<?> getJobClass(JobDetail jobDetail) {
		return jobDetail.getJobClass();
	}

	Date getTriggerPreviousFireTime(Trigger trigger) {
		return trigger.getPreviousFireTime();
	}

	Date getTriggerNextFireTime(Trigger trigger) {
		return trigger.getNextFireTime();
	}

	String getCronTriggerExpression(CronTrigger trigger) {
		// getCronExpression gives a PMD false+
		return trigger.getCronExpression();
	}

	long getSimpleTriggerRepeatInterval(SimpleTrigger trigger) {
		return trigger.getRepeatInterval();
	}

	public JobDetail getContextJobDetail(JobExecutionContext context) {
		return context.getJobDetail();
	}

	Date getContextFireTime(JobExecutionContext context) {
		return context.getFireTime();
	}

	public void addGlobalJobListener(JobListener jobGlobalListener) throws SchedulerException {
		final Scheduler defaultScheduler;
		if (Parameter.QUARTZ_DEFAULT_LISTENER_DISABLED.getValueAsBoolean()) {
			defaultScheduler = null;
			LOG.debug("Initialization of Quartz default listener has been disabled");
		} else {
			defaultScheduler = StdSchedulerFactory.getDefaultScheduler();
			defaultScheduler.addGlobalJobListener(jobGlobalListener);
		}
		for (final Scheduler scheduler : JobInformations.getAllSchedulers()) {
			if (scheduler != defaultScheduler) {
				scheduler.addGlobalJobListener(jobGlobalListener);
			}
		}
	}

	@SuppressWarnings("unchecked")
	public void removeGlobalJobListener(Class<? extends JobListener> jobListenerClass)
			throws SchedulerException {
		for (final Scheduler scheduler : JobInformations.getAllSchedulers()) {
			final List<JobListener> globalJobListeners = scheduler.getGlobalJobListeners();
			for (final JobListener jobListener : new ArrayList<JobListener>(globalJobListeners)) {
				if (jobListenerClass.isInstance(jobListener)) {
					try {
						scheduler.removeGlobalJobListener(jobListener);
					} catch (final NoSuchMethodError e1) {
						// pour Quartz 1.7, 1.8 et +,
						// cette méthode n'existe pas avant Quartz 1.6
						try {
							final Class<? extends Scheduler> schedulerClass = scheduler.getClass();
							schedulerClass.getMethod("removeGlobalJobListener", String.class)
									.invoke(scheduler, jobListener.getName());
						} catch (final Exception e2) {
							throw new IllegalArgumentException(e2);
						}
					}
				}
			}
		}
	}

	List<JobDetail> getAllJobsOfScheduler(Scheduler scheduler) throws SchedulerException {
		final List<JobDetail> result = new ArrayList<JobDetail>();
		for (final String jobGroupName : scheduler.getJobGroupNames()) {
			for (final String jobName : scheduler.getJobNames(jobGroupName)) {
				final JobDetail jobDetail;
				try {
					jobDetail = scheduler.getJobDetail(jobName, jobGroupName);
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

	List<Trigger> getTriggersOfJob(JobDetail jobDetail, Scheduler scheduler)
			throws SchedulerException {
		return Arrays.asList(scheduler.getTriggersOfJob(jobDetail.getName(), jobDetail.getGroup()));
	}

	boolean isTriggerPaused(Trigger trigger, Scheduler scheduler) throws SchedulerException {
		return scheduler.getTriggerState(trigger.getName(),
				trigger.getGroup()) == Trigger.STATE_PAUSED;
	}

	void pauseJob(JobDetail jobDetail, Scheduler scheduler) throws SchedulerException {
		scheduler.pauseJob(jobDetail.getName(), jobDetail.getGroup());
	}

	void resumeJob(JobDetail jobDetail, Scheduler scheduler) throws SchedulerException {
		scheduler.resumeJob(jobDetail.getName(), jobDetail.getGroup());
	}
}
