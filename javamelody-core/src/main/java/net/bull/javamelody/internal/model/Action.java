/*
 * Copyright 2008-2017 by Emeric Vernat
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

import java.io.File;
import java.io.IOException;
import java.lang.management.ManagementFactory;
import java.lang.reflect.Method;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.Locale;

import javax.management.JMException;
import javax.management.ObjectName;
import javax.management.openmbean.CompositeData;
import javax.servlet.http.HttpSession;

import org.quartz.JobDetail;
import org.quartz.Scheduler;

import net.bull.javamelody.Parameter;
import net.bull.javamelody.SessionListener;
import net.bull.javamelody.internal.common.I18N;
import net.bull.javamelody.internal.common.InputOutput;
import net.bull.javamelody.internal.common.LOG;
import net.bull.javamelody.internal.common.Parameters;
import net.bull.javamelody.internal.web.MailReport;
import net.sf.ehcache.Cache;
import net.sf.ehcache.CacheManager;

/**
 * Énumération des actions possibles dans l'IHM.
 * @author Emeric Vernat
 * @author <a href="mailto:davidkarlsen@gmail.com">David J. M. Karlsen (IBM heapdump support)<a>
 */
public enum Action {
	/** Test d'envoi du rapport pdf par mail. */
	MAIL_TEST(""),

	/** Réinitialisation d'un compteur non périodique. */
	CLEAR_COUNTER("http"),

	/** Garbage Collect. */
	GC("systeminfo"),

	/** Invalidations des sessions http. */
	INVALIDATE_SESSIONS("systeminfo"),

	/** Invalidation d'une session http. */
	INVALIDATE_SESSION(""),

	/** Invalidation de la session http courante. */
	LOGOUT(""),

	/** Heap dump. */
	HEAP_DUMP("systeminfo"),

	/** Purge le contenu de tous les caches (ie, for ALL_CACHE_MANAGERS {cacheManager.clearAll()}). */
	CLEAR_CACHES("caches"),

	/** Purge le contenu  d'un cache. */
	CLEAR_CACHE("caches"),

	/** Purge la clé d'un cache. */
	CLEAR_CACHE_KEY("caches"),

	/** Tue un thread java. */
	KILL_THREAD("threads"),

	/** Envoi un signal interrupt à un thread java. */
	SEND_THREAD_INTERRUPT("threads"),

	/** Met un job quartz en pause. */
	PAUSE_JOB("jobs"),

	/** Enlève la pause d'un job quartz. */
	RESUME_JOB("jobs"),

	/** Réinitialisation des hotspots. */
	CLEAR_HOTSPOTS(""),

	/** Purge les fichiers .rrd et .ser.gz obsolètes. */
	PURGE_OBSOLETE_FILES("bottom");

	/**
	 * Booléen selon que l'action 'Garbage collector' est possible.
	 */
	public static final boolean GC_ENABLED = !ManagementFactory.getRuntimeMXBean()
			.getInputArguments().contains("-XX:+DisableExplicitGC");

	static final String JAVA_VENDOR = System.getProperty("java.vendor");

	private static final String ALL = "all";

	/**
	 * Nom du contexte dans lequel est exécutée l'action
	 * (servira dans l'url pour replacer la page html sur l'anchor de même nom)
	 */
	private final String contextName;

	Action(String contextName) {
		this.contextName = contextName;
	}

	public String getContextName(String counterName) {
		if (this == CLEAR_COUNTER && !ALL.equalsIgnoreCase(counterName)) {
			return counterName;
		}
		return contextName;
	}

	/**
	 * Convertit le code d'une action en énumération de l'action.
	 * @param action String
	 * @return Action
	 */
	public static Action valueOfIgnoreCase(String action) {
		return valueOf(action.toUpperCase(Locale.ENGLISH).trim());
	}

	/**
	 * Vérifie que le paramètre pour activer les actions systèmes est positionné.
	 */
	public static void checkSystemActionsEnabled() {
		if (!Parameters.isSystemActionsEnabled()) {
			throw new IllegalStateException(I18N.getString("Actions_non_activees"));
		}
	}

	// méthode conservée pour compatibilité ascendante, notamment dans Jenkins
	public String execute(Collector collector, CollectorServer collectorServer, String counterName,
			String sessionId, String threadId, String jobId, String cacheId) throws IOException {
		return execute(collector, collectorServer, null, counterName, sessionId, threadId, jobId,
				cacheId, null);
	}

	// CHECKSTYLE:OFF
	// since 1.49
	String execute(Collector collector, CollectorServer collectorServer, HttpSession currentSession,
			String counterName, String sessionId, String threadId, String jobId, String cacheId)
			throws IOException {
		// CHECKSTYLE:ON
		return execute(collector, collectorServer, currentSession, counterName, sessionId, threadId,
				jobId, cacheId, null);
	}

	/**
	 * Exécute l'action.
	 * @param collector Collector pour une réinitialisation et test de mail
	 * @param collectorServer Serveur de collecte pour test de mail (null s'il n'y en a pas)
	 * @param currentSession session http de l'utilisateur exécutant l'action (null sinon)
	 * @param counterName Nom du compteur pour une réinitialisation
	 * @param sessionId Identifiant de session pour invalidation (null sinon)
	 * @param threadId Identifiant du thread sous la forme pid_ip_id
	 * @param jobId Identifiant du job sous la forme pid_ip_id
	 * @param cacheId Identifiant du cache à vider
	 * @param cacheKey Identifiant d'une clé de cache à vider
	 * @return Message de résultat
	 * @throws IOException e
	 * @since 1.66
	 */
	// CHECKSTYLE:OFF
	public String execute(Collector collector, CollectorServer collectorServer, // NOPMD
			HttpSession currentSession, String counterName, String sessionId, String threadId,
			String jobId, String cacheId, String cacheKey) throws IOException {
		// CHECKSTYLE:ON
		final String messageForReport;
		switch (this) {
		case CLEAR_COUNTER:
			assert collector != null;
			assert counterName != null;
			messageForReport = clearCounter(collector, counterName);
			break;
		case MAIL_TEST:
			assert collector != null;
			messageForReport = mailTest(collector, collectorServer);
			break;
		case GC:
			if (GC_ENABLED) {
				// garbage collector
				final long kbFreed = gc();
				final long stillUsed = (Runtime.getRuntime().totalMemory()
						- Runtime.getRuntime().freeMemory()) / 1024;
				messageForReport = I18N.getFormattedString("ramasse_miette_execute", kbFreed,
						stillUsed);
			} else {
				messageForReport = I18N.getString("ramasse_miette_desactive");
			}
			break;
		case HEAP_DUMP:
			if (JAVA_VENDOR.contains("IBM")) {
				ibmHeapDump();
				messageForReport = I18N.getString("heap_dump_genere_ibm");
			} else {
				// heap dump à générer dans le répertoire temporaire sur le serveur
				// avec un suffixe contenant le host, la date et l'heure et avec une extension hprof
				// (utiliser jvisualvm du jdk ou MAT d'eclipse en standalone ou en plugin)
				final File heapDump = heapDump();
				final File zipFile = new File(heapDump.getParentFile(),
						heapDump.getName() + ".zip");
				InputOutput.zipFile(heapDump, zipFile);
				InputOutput.deleteFile(heapDump);
				String message = "";
				if (Parameter.HEAP_DUMP_S3_BUCKETNAME.getValue() != null) {
					try {
						S3.upload(zipFile, Parameter.HEAP_DUMP_S3_BUCKETNAME.getValue());
						message = I18N.getFormattedString("heap_dump_uploaded_to_s3",
								zipFile.getName()) + ' ';
					} catch (final IOException e) {
						message = "Failed to upload heap dump to S3 - " + e.getMessage() + '\n';
					}
				}
				final String path = zipFile.getPath();
				messageForReport = message
						+ I18N.getFormattedString("heap_dump_genere", path.replace('\\', '/'));
			}
			break;
		case INVALIDATE_SESSIONS:
			// invalidation des sessions http
			SessionListener.invalidateAllSessionsExceptCurrentSession(currentSession);
			messageForReport = I18N.getString("sessions_http_invalidees");
			break;
		case INVALIDATE_SESSION:
			// invalidation d'une session http
			assert sessionId != null;
			SessionListener.invalidateSession(sessionId);
			messageForReport = I18N.getString("session_http_invalidee");
			break;
		case LOGOUT:
			// invalidation de la session http courante
			if (currentSession != null) {
				SessionListener.invalidateSession(currentSession.getId());
			}
			messageForReport = I18N.getString("logged_out");
			break;
		case CLEAR_CACHES:
			clearCaches();
			messageForReport = I18N.getString("caches_purges");
			break;
		case CLEAR_CACHE:
			clearCache(cacheId);
			messageForReport = I18N.getFormattedString("cache_purge", cacheId);
			break;
		case CLEAR_CACHE_KEY:
			clearCacheKey(cacheId, cacheKey);
			messageForReport = I18N.getFormattedString("cache_key_purge", cacheId, cacheKey);
			break;
		case KILL_THREAD:
			assert threadId != null;
			messageForReport = killThread(threadId);
			break;
		case SEND_THREAD_INTERRUPT:
			assert threadId != null;
			messageForReport = sendThreadInterrupt(threadId);
			break;
		case PAUSE_JOB:
			assert jobId != null;
			messageForReport = pauseJob(jobId);
			break;
		case RESUME_JOB:
			assert jobId != null;
			messageForReport = resumeJob(jobId);
			break;
		case CLEAR_HOTSPOTS:
			assert collector.getSamplingProfiler() != null;
			collector.getSamplingProfiler().clear();
			messageForReport = I18N.getString("hotspots_cleared");
			break;
		case PURGE_OBSOLETE_FILES:
			assert collector != null;
			collector.deleteObsoleteFiles();
			messageForReport = I18N.getString("fichiers_obsoletes_purges") + '\n'
					+ I18N.getString("Usage_disque") + ": "
					+ (collector.getDiskUsage() / 1024 / 1024 + 1) + ' ' + I18N.getString("Mo");
			break;
		default:
			throw new IllegalStateException(toString());
		}
		if (messageForReport != null) {
			// log pour information en debug
			LOG.debug("Action '" + this + "' executed. Result: "
					+ messageForReport.replace('\n', ' '));
		}
		return messageForReport;
	}

	private String clearCounter(Collector collector, String counterName) {
		final String messageForReport;
		if (ALL.equalsIgnoreCase(counterName)) {
			for (final Counter counter : collector.getCounters()) {
				collector.clearCounter(counter.getName());
			}
			messageForReport = I18N.getFormattedString("Toutes_statistiques_reinitialisees",
					counterName);
		} else {
			// l'action Réinitialiser a été appelée pour un compteur
			collector.clearCounter(counterName);
			messageForReport = I18N.getFormattedString("Statistiques_reinitialisees", counterName);
		}
		return messageForReport;
	}

	private String mailTest(Collector collector, CollectorServer collectorServer) {
		// note: a priori, inutile de traduire cela
		if (!Parameters.isPdfEnabled()) {
			throw new IllegalStateException("itext classes not found: add the itext dependency");
		}
		if (Parameter.MAIL_SESSION.getValue() == null) {
			throw new IllegalStateException(
					"mail-session has no value: add the mail-session parameter");
		}
		if (Parameter.ADMIN_EMAILS.getValue() == null) {
			throw new IllegalStateException(
					"admin-emails has no value: add the admin-emails parameter");
		}
		try {
			if (collectorServer == null) {
				// serveur local
				new MailReport().sendReportMailForLocalServer(collector, Period.JOUR);
			} else {
				// serveur de collecte
				new MailReport().sendReportMail(collector, true, collectorServer
						.getJavaInformationsByApplication(collector.getApplication()), Period.JOUR);
			}
		} catch (final Exception e) {
			throw new RuntimeException(e); // NOPMD
		}
		return "Mail sent with pdf report for the day to admins";
	}

	private File heapDump() throws IOException {
		try {
			final ObjectName objectName = new ObjectName(
					"com.sun.management:type=HotSpotDiagnostic");
			final CompositeData vmOption = (CompositeData) MBeansAccessor.invoke(objectName,
					"getVMOption", new Object[] { "HeapDumpPath" }, new Class[] { String.class });
			final String heapDumpPath;
			if (vmOption == null) {
				heapDumpPath = null;
			} else {
				heapDumpPath = (String) vmOption.get("value");
			}
			final String path;
			if (heapDumpPath == null || heapDumpPath.isEmpty()) {
				path = Parameters.TEMPORARY_DIRECTORY.getPath();
			} else {
				// -XX:HeapDumpPath=/tmp par exemple a été spécifié comme paramètre de VM.
				// Dans ce cas, on prend en compte ce paramètre "standard" de la JVM Hotspot
				final File file = new File(heapDumpPath);
				if (file.exists()) {
					if (file.isDirectory()) {
						path = heapDumpPath;
					} else {
						path = file.getParent();
					}
				} else {
					if (!file.mkdirs()) {
						throw new IllegalStateException("Can't create directory " + file.getPath());
					}
					path = heapDumpPath;
				}
			}
			final DateFormat dateFormat = new SimpleDateFormat("yyyyMMdd_HHmmss",
					Locale.getDefault());
			final File heapDumpFile = new File(path, "heapdump-" + Parameters.getHostName() + '-'
					+ PID.getPID() + '-' + dateFormat.format(new Date()) + ".hprof");
			if (heapDumpFile.exists()) {
				try {
					// si le fichier existe déjà, un heap dump a déjà été généré dans la même seconde
					// donc on attends 1 seconde pour créer le fichier avec un nom différent
					Thread.sleep(1000);
				} catch (final InterruptedException e) {
					throw new IllegalStateException(e);
				}
				return heapDump();
			}
			final boolean gcBeforeHeapDump = true;
			MBeansAccessor.invoke(objectName, "dumpHeap",
					new Object[] { heapDumpFile.getPath(), gcBeforeHeapDump },
					new Class[] { String.class, boolean.class });
			return heapDumpFile;
		} catch (final JMException e) {
			throw new IllegalStateException(e);
		}
	}

	private void ibmHeapDump() {
		try {
			final Class<?> dumpClass = getClass().getClassLoader().loadClass("com.ibm.jvm.Dump"); // NOPMD
			final Class<?>[] argTypes = null;
			final Method dump = dumpClass.getMethod("HeapDump", argTypes);
			final Object[] args = null;
			dump.invoke(null, args);
		} catch (final Exception e) {
			throw new IllegalStateException(e);
		}
	}

	// cette méthode doit s'appeler "gc" pour que findbugs ne fasse pas de warning
	@SuppressWarnings("all")
	private long gc() {
		final long before = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory();
		Runtime.getRuntime().gc();
		final long after = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory();
		return (before - after) / 1024;
	}

	private void clearCaches() {
		final List<CacheManager> allCacheManagers = CacheManager.ALL_CACHE_MANAGERS;
		for (final CacheManager cacheManager : allCacheManagers) {
			cacheManager.clearAll();
		}
	}

	private void clearCache(String cacheId) {
		final List<CacheManager> allCacheManagers = CacheManager.ALL_CACHE_MANAGERS;
		for (final CacheManager cacheManager : allCacheManagers) {
			final Cache cache = cacheManager.getCache(cacheId);
			if (cache != null) {
				cache.removeAll();
			}
		}
	}

	private void clearCacheKey(String cacheId, String cacheKey) {
		final List<CacheManager> allCacheManagers = CacheManager.ALL_CACHE_MANAGERS;
		for (final CacheManager cacheManager : allCacheManagers) {
			final Cache cache = cacheManager.getCache(cacheId);
			if (cache != null) {
				final boolean removed = cache.remove(cacheKey);
				if (!removed) {
					// if keys are not Strings, we have to find the initial key
					for (final Object key : cache.getKeys()) {
						if (key != null && key.toString().equals(cacheKey)) {
							cache.remove(key);
							break;
						}
					}
				}
			}
		}
	}

	private String killThread(String globalThreadId) {
		final Long threadId = getThreadIdFromGlobalThreadIdIfSameJvm(globalThreadId);
		if (threadId != null) {
			final List<Thread> threads = JavaInformations.getThreadsFromThreadGroups();
			for (final Thread thread : threads) {
				if (thread.getId() == threadId.longValue()) {
					if (thread.getName().startsWith("javamelody")) {
						return "I will not kill myself";
					}
					stopThread(thread);
					return I18N.getFormattedString("Thread_tue", thread.getName());
				}
			}
			return I18N.getString("Thread_non_trouve");
		}

		// cette action ne concernait pas cette JVM, donc on ne fait rien
		return null;
	}

	@SuppressWarnings("deprecation")
	private void stopThread(Thread thread) {
		// I know that it is unsafe and the user has been warned
		thread.stop();
	}

	private String sendThreadInterrupt(String globalThreadId) {
		final Long threadId = getThreadIdFromGlobalThreadIdIfSameJvm(globalThreadId);
		if (threadId != null) {
			final List<Thread> threads = JavaInformations.getThreadsFromThreadGroups();
			for (final Thread thread : threads) {
				if (thread.getId() == threadId.longValue()) {
					if (thread.getName().startsWith("javamelody")) {
						return "I will not interrupt myself";
					}
					thread.interrupt();
					return I18N.getFormattedString("thread_interrupt_sent", thread.getName());
				}
			}
			return I18N.getString("Thread_non_trouve");
		}

		// cette action ne concernait pas cette JVM, donc on ne fait rien
		return null;
	}

	private Long getThreadIdFromGlobalThreadIdIfSameJvm(String globalThreadId) {
		final String[] values = globalThreadId.split("_");
		if (values.length != 3) {
			throw new IllegalArgumentException(globalThreadId);
		}
		// rq : la syntaxe vérifiée ici doit être conforme à ThreadInformations.buildGlobalThreadId
		if (values[0].equals(PID.getPID()) && values[1].equals(Parameters.getHostAddress())) {
			return Long.valueOf(values[2]);
		}
		return null;
	}

	private String pauseJob(String jobId) {
		if (ALL.equalsIgnoreCase(jobId)) {
			pauseAllJobs();
			return I18N.getString("all_jobs_paused");
		}

		final String[] values = jobId.split("_");
		if (values.length != 3) {
			throw new IllegalArgumentException(jobId);
		}
		// rq : la syntaxe vérifiée ici doit être conforme à JobInformations.buildGlobalJobId
		if (values[0].equals(PID.getPID()) && values[1].equals(Parameters.getHostAddress())) {
			if (pauseJobById(Integer.parseInt(values[2]))) {
				return I18N.getString("job_paused");
			}
			return I18N.getString("job_notfound");
		}

		// cette action ne concernait pas cette JVM, donc on ne fait rien
		return null;
	}

	private boolean pauseJobById(int myJobId) {
		try {
			final QuartzAdapter quartzAdapter = QuartzAdapter.getSingleton();
			for (final Scheduler scheduler : JobInformations.getAllSchedulers()) {
				for (final JobDetail jobDetail : quartzAdapter.getAllJobsOfScheduler(scheduler)) {
					if (quartzAdapter.getJobFullName(jobDetail).hashCode() == myJobId) {
						quartzAdapter.pauseJob(jobDetail, scheduler);
						return true;
					}
				}
			}
			return false;
		} catch (final Exception e) {
			throw new IllegalStateException(e);
		}
	}

	private void pauseAllJobs() {
		try {
			for (final Scheduler scheduler : JobInformations.getAllSchedulers()) {
				scheduler.pauseAll();
			}
		} catch (final Exception e) {
			throw new IllegalStateException(e);
		}
	}

	private String resumeJob(String jobId) {
		if (ALL.equalsIgnoreCase(jobId)) {
			resumeAllJobs();
			return I18N.getString("all_jobs_resumed");
		}
		final String[] values = jobId.split("_");
		if (values.length != 3) {
			throw new IllegalArgumentException(jobId);
		}
		// rq : la syntaxe vérifiée ici doit être conforme à JobInformations.buildGlobalJobId
		if (values[0].equals(PID.getPID()) && values[1].equals(Parameters.getHostAddress())) {
			if (resumeJobById(Integer.parseInt(values[2]))) {
				return I18N.getString("job_resumed");
			}
			return I18N.getString("job_notfound");
		}

		// cette action ne concernait pas cette JVM, donc on ne fait rien
		return null;
	}

	private boolean resumeJobById(int myJobId) {
		try {
			final QuartzAdapter quartzAdapter = QuartzAdapter.getSingleton();
			for (final Scheduler scheduler : JobInformations.getAllSchedulers()) {
				for (final JobDetail jobDetail : quartzAdapter.getAllJobsOfScheduler(scheduler)) {
					if (quartzAdapter.getJobFullName(jobDetail).hashCode() == myJobId) {
						quartzAdapter.resumeJob(jobDetail, scheduler);
						return true;
					}
				}
			}
			return false;
		} catch (final Exception e) {
			throw new IllegalStateException(e);
		}
	}

	private void resumeAllJobs() {
		try {
			for (final Scheduler scheduler : JobInformations.getAllSchedulers()) {
				scheduler.resumeAll();
			}
		} catch (final Exception e) {
			throw new IllegalStateException(e);
		}
	}
}
