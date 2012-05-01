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

/**
 * Classe permettant de fournir une API adaptée aux différentes versions de Quartz.<br/>
 * L'implémentation par défaut est adaptée à Quartz avant la version 2.<br/>
 * Une autre implémentation avec la même API sera fournie et sera adaptée à la version 2 et aux suivantes.
 * @author Emeric Vernat
 */
class QuartzAdapter {
	private static final boolean QUARTZ_2 = isQuartz2();
	private static final QuartzAdapter SINGLETON = createSingleton();

	protected QuartzAdapter() {
		super();
	}

	static QuartzAdapter getSingleton() {
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
				return (QuartzAdapter) Class.forName("net.bull.javamelody.Quartz2Adapter")
						.newInstance();
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

	String getJobFullName(JobDetail jobDetail) {
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
		return trigger.getCronExpression(); // NOPMD
	}

	long getSimpleTriggerRepeatInterval(SimpleTrigger trigger) {
		return trigger.getRepeatInterval(); // NOPMD
	}

	JobDetail getContextJobDetail(JobExecutionContext context) {
		return context.getJobDetail();
	}

	Date getContextFireTime(JobExecutionContext context) {
		return context.getFireTime();
	}

	void addGlobalJobListener(JobListener jobGlobalListener) throws SchedulerException {
		final Scheduler defaultScheduler;
		if (Boolean.parseBoolean(Parameters
				.getParameter(Parameter.QUARTZ_DEFAULT_LISTENER_DISABLED))) {
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
	void removeGlobalJobListener() throws SchedulerException {
		for (final Scheduler scheduler : JobInformations.getAllSchedulers()) {
			final List<JobListener> globalJobListeners = scheduler.getGlobalJobListeners();
			for (final JobListener jobListener : new ArrayList<JobListener>(globalJobListeners)) {
				if (jobListener instanceof JobGlobalListener) {
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
							throw new IllegalArgumentException(e2); // NOPMD
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
		return scheduler.getTriggerState(trigger.getName(), trigger.getGroup()) == Trigger.STATE_PAUSED;
	}

	void pauseJob(JobDetail jobDetail, Scheduler scheduler) throws SchedulerException {
		scheduler.pauseJob(jobDetail.getName(), jobDetail.getGroup());
	}

	void resumeJob(JobDetail jobDetail, Scheduler scheduler) throws SchedulerException {
		scheduler.resumeJob(jobDetail.getName(), jobDetail.getGroup());
	}
}
