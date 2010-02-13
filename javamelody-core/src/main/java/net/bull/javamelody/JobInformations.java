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

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.quartz.JobDetail;
import org.quartz.JobExecutionContext;
import org.quartz.Scheduler;
import org.quartz.Trigger;
import org.quartz.impl.SchedulerRepository;

/**
 * Informations sur un job.
 * L'état d'une instance est initialisé à son instanciation et non mutable;
 * il est donc de fait thread-safe.
 * Cet état est celui d'un job à un instant t.
 * Les instances sont sérialisables pour pouvoir être transmises au serveur de collecte.
 * Pour l'instant seul quartz est géré.
 * @author Emeric Vernat
 */
class JobInformations implements Serializable {
	private static final long serialVersionUID = -2826168112578815952L;
	private static final boolean QUARTZ_AVAILABLE = isQuartzAvailable();
	private final String group;
	private final String name;
	private final String description;
	private final String jobClassName;
	private final Date previousFireTime;
	private final Date nextFireTime;
	private final long elapsedTime;
	private final boolean paused;
	private final String globalJobId;

	JobInformations(JobDetail jobDetail, JobExecutionContext jobExecutionContext,
			Scheduler scheduler) throws Exception {
		// pas throws SchedulerException ici sinon NoClassDefFoundError
		super();
		assert jobDetail != null;
		assert scheduler != null;
		// rq: jobExecutionContext est non null si le job est en cours d'exécution ou null sinon
		this.group = jobDetail.getGroup();
		this.name = jobDetail.getName();
		this.description = jobDetail.getDescription();
		this.jobClassName = jobDetail.getJobClass().getName();
		if (jobExecutionContext == null) {
			elapsedTime = -1;
		} else {
			elapsedTime = System.currentTimeMillis() - jobExecutionContext.getFireTime().getTime();
		}

		Date triggerNextFireTime = null;
		Date triggerPreviousFireTime = null;
		boolean jobPaused = true;
		for (final Trigger trigger : scheduler.getTriggersOfJob(name, group)) {
			if (triggerNextFireTime == null || triggerNextFireTime.after(trigger.getNextFireTime())) {
				triggerNextFireTime = trigger.getNextFireTime();
			}
			if (triggerPreviousFireTime == null
					|| triggerPreviousFireTime.before(trigger.getPreviousFireTime())) {
				triggerPreviousFireTime = trigger.getPreviousFireTime();
			}
			//						if (trigger instanceof CronTrigger) {
			//							((CronTrigger) trigger).getCronExpression();
			//						}
			if (scheduler.getTriggerState(trigger.getName(), trigger.getGroup()) != Trigger.STATE_PAUSED) {
				jobPaused = false;
			}
		}
		this.nextFireTime = triggerNextFireTime;
		this.previousFireTime = triggerPreviousFireTime;
		this.paused = jobPaused;
		this.globalJobId = buildGlobalJobId(jobDetail);
	}

	private static boolean isQuartzAvailable() {
		try {
			Class.forName("org.quartz.Job");
			return true;
		} catch (final ClassNotFoundException e) {
			return false;
		}
	}

	@SuppressWarnings("unchecked")
	static List<JobInformations> buildJobInformationsList() {
		if (!QUARTZ_AVAILABLE) {
			return Collections.emptyList();
		}
		final List<JobInformations> result = new ArrayList<JobInformations>();
		try {
			for (final Scheduler scheduler : getAllSchedulers()) {
				final Map<String, JobExecutionContext> currentlyExecutingJobsByFullName = new LinkedHashMap<String, JobExecutionContext>();
				for (final JobExecutionContext currentlyExecutingJob : (List<JobExecutionContext>) scheduler
						.getCurrentlyExecutingJobs()) {
					currentlyExecutingJobsByFullName.put(currentlyExecutingJob.getJobDetail()
							.getFullName(), currentlyExecutingJob);
				}
				for (final String jobGroupName : scheduler.getJobGroupNames()) {
					for (final String jobName : scheduler.getJobNames(jobGroupName)) {
						final JobDetail jobDetail = scheduler.getJobDetail(jobName, jobGroupName);
						final JobExecutionContext jobExecutionContext = currentlyExecutingJobsByFullName
								.get(jobDetail.getFullName());
						result.add(new JobInformations(jobDetail, jobExecutionContext, scheduler));
					}
				}
			}
		} catch (final Exception e) {
			throw new RuntimeException(e);
		}
		return result;
	}

	@SuppressWarnings("unchecked")
	static List<Scheduler> getAllSchedulers() {
		return new ArrayList<Scheduler>(SchedulerRepository.getInstance().lookupAll());
	}

	String getGlobalJobId() {
		return globalJobId;
	}

	String getName() {
		return name;
	}

	String getGroup() {
		return group;
	}

	String getDescription() {
		return description;
	}

	String getJobClassName() {
		return jobClassName;
	}

	long getElapsedTime() {
		return elapsedTime;
	}

	boolean isCurrentlyExecuting() {
		return elapsedTime >= 0;
	}

	Date getNextFireTime() {
		return nextFireTime;
	}

	Date getPreviousFireTime() {
		return previousFireTime;
	}

	boolean isPaused() {
		return paused;
	}

	private static String buildGlobalJobId(JobDetail jobDetail) {
		return PID.getPID() + '_' + Parameters.getHostAddress() + '_'
				+ jobDetail.getFullName().hashCode();
	}

	/** {@inheritDoc} */
	@Override
	public String toString() {
		return getClass().getSimpleName() + "[name=" + getName() + ", group=" + getGroup() + ']';
	}
}
