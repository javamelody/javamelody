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

import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import org.apache.log4j.Logger;

import net.bull.javamelody.Parameter;
import net.bull.javamelody.internal.common.LOG;
import net.bull.javamelody.internal.common.Mailer;
import net.bull.javamelody.internal.common.Parameters;
import net.bull.javamelody.internal.model.SamplingProfiler.SampledMethod;
import net.bull.javamelody.internal.web.MailReport;

/**
 * Collecteur de données du serveur de collecte centralisé.
 * @author Emeric Vernat
 */
public class CollectorServer {
	static final Logger LOGGER = Logger.getLogger("javamelody");

	private static final int NB_COLLECT_THREADS = 10;

	private final Map<String, Throwable> lastCollectExceptionsByApplication = new ConcurrentHashMap<String, Throwable>();
	private final Map<String, RemoteCollector> remoteCollectorsByApplication = new ConcurrentHashMap<String, RemoteCollector>();

	private final ExecutorService executorService = Executors
			.newFixedThreadPool(NB_COLLECT_THREADS);

	private final Timer timer;

	/**
	 * Constructeur.
	 * @throws IOException e
	 */
	public CollectorServer() throws IOException {
		super();
		boolean initOk = false;
		this.timer = new Timer("collector", true);
		try {
			LOGGER.info(
					"reading applications list from: " + Parameters.getCollectorApplicationsFile());
			final Map<String, List<URL>> urlsByApplication = Parameters
					.getCollectorUrlsByApplications();
			LOGGER.info("monitored applications: " + urlsByApplication.keySet());
			LOGGER.info("urls of monitored applications: " + urlsByApplication);

			final int periodMillis = Parameters.getResolutionSeconds() * 1000;
			LOGGER.info("resolution of the monitoring in seconds: "
					+ Parameters.getResolutionSeconds());
			final TimerTask collectTask = new TimerTask() {
				/** {@inheritDoc} */
				@Override
				public void run() {
					// il ne doit pas y avoir d'erreur dans cette task
					collectWithoutErrors();
					// cette collecte ne peut interférer avec un autre thread,
					// car les compteurs sont mis à jour et utilisés par le même timer
					// et donc le même thread (les différentes tasks ne peuvent se chevaucher)
				}
			};
			// on schedule la tâche de fond,
			// avec une exécution de suite en asynchrone pour initialiser les données
			timer.schedule(collectTask, 100, periodMillis);
			JRobin.initBackendFactory(timer);

			UpdateChecker.init(timer, null, UpdateChecker.COLLECTOR_SERVER_APPLICATION_TYPE);
			initOk = true;
		} finally {
			if (!initOk) {
				// si exception dans initialisation, on annule la création du timer
				// (sinon tomcat ne serait pas content)
				timer.cancel();
			}
		}
	}

	public void collectWithoutErrors() {
		// clone pour éviter ConcurrentModificationException
		final Map<String, List<URL>> clone;
		try {
			clone = new LinkedHashMap<String, List<URL>>(
					Parameters.getCollectorUrlsByApplications());
		} catch (final IOException e) {
			LOGGER.warn(e.getMessage(), e);
			return;
		}
		for (final Map.Entry<String, List<URL>> entry : clone.entrySet()) {
			final String application = entry.getKey();
			final List<URL> urls = entry.getValue();
			executorService.submit(new Runnable() {
				@Override
				public void run() {
					collectForApplicationWithoutErrors(application, urls);
				}
			});
		}
	}

	public String collectForApplicationForAction(String application, List<URL> urls)
			throws IOException {
		return collectForApplication(new RemoteCollector(application, urls));
	}

	void collectForApplicationWithoutErrors(String application, List<URL> urls) {
		try {
			collectForApplication(application, urls);
			final boolean becameAvailable = lastCollectExceptionsByApplication
					.containsKey(application);
			lastCollectExceptionsByApplication.remove(application);

			if (becameAvailable) {
				final String subject = "The application " + application
						+ " is available again for the monitoring server";
				notifyAdmins(subject, subject);
			}
		} catch (final Throwable e) { // NOPMD
			// si erreur sur une webapp (indisponibilité par exemple), on continue avec les autres
			// et il ne doit y avoir aucune erreur dans cette task
			try {
				LOGGER.warn("exception while collecting data for application " + application);
				LOGGER.warn(e.toString(), e);
				final boolean becameUnavailable = !lastCollectExceptionsByApplication
						.containsKey(application);
				lastCollectExceptionsByApplication.put(application, e);

				if (becameUnavailable) {
					final String subject = "The application " + application
							+ " is unavailable for the monitoring server";
					final String message = subject + "\n\nCause:\n" + e.toString();
					notifyAdmins(subject, message);
				}
			} catch (final Throwable e2) { // NOPMD
				// tant pis, on continue quand même
				return;
			}
		}
	}

	String collectForApplication(String application, List<URL> urls) throws IOException {
		final boolean remoteCollectorAvailable = isApplicationDataAvailable(application);
		final RemoteCollector remoteCollector;
		if (!remoteCollectorAvailable) {
			remoteCollector = new RemoteCollector(application, urls);
		} else {
			remoteCollector = getRemoteCollectorByApplication(application);
		}

		final String messageForReport = collectForApplication(remoteCollector);

		if (!remoteCollectorAvailable) {
			// on initialise les remoteCollectors au fur et à mesure
			// puisqu'on ne peut pas forcément au démarrage
			// car la webapp à monitorer peut être indisponible
			remoteCollectorsByApplication.put(application, remoteCollector);

			if (Parameter.MAIL_SESSION.getValue() != null
					&& Parameter.ADMIN_EMAILS.getValue() != null) {
				scheduleReportMailForCollectorServer(application);
				LOGGER.info("Periodic report scheduled for the application " + application + " to "
						+ Parameter.ADMIN_EMAILS.getValue());
			}
		}
		return messageForReport;
	}

	private String collectForApplication(RemoteCollector remoteCollector) throws IOException {
		final String application = remoteCollector.getApplication();
		final List<URL> urls = remoteCollector.getURLs();
		LOGGER.info("collect for the application " + application + " on " + urls);
		assert application != null;
		assert urls != null;
		final long start = System.currentTimeMillis();

		final String messageForReport = remoteCollector.collectData();
		final List<JavaInformations> javaInformationsList = remoteCollector
				.getJavaInformationsList();
		final Collector collector = remoteCollector.getCollector();
		collector.collectWithoutErrors(javaInformationsList);
		LOGGER.info("collect for the application " + application + " done in "
				+ (System.currentTimeMillis() - start) + "ms");
		if (LOGGER.isDebugEnabled()) {
			LOGGER.debug("counters " + application + " : " + collector.getCounters());
			LOGGER.debug("javaInformations " + application + " : " + javaInformationsList);
			if (messageForReport != null) {
				LOGGER.debug(
						"message " + application + " : " + messageForReport.replace("\n", ", "));
			}
		}
		return messageForReport;
	}

	public List<SessionInformations> collectSessionInformations(String application,
			String sessionId) throws IOException {
		return getRemoteCollectorByApplication(application).collectSessionInformations(sessionId);
	}

	public List<SampledMethod> collectHotspots(String application) throws IOException {
		return getRemoteCollectorByApplication(application).collectHotspots();
	}

	public HeapHistogram collectHeapHistogram(String application) throws IOException {
		return getRemoteCollectorByApplication(application).collectHeapHistogram();
	}

	public DatabaseInformations collectDatabaseInformations(String application, int requestIndex)
			throws IOException {
		return getRemoteCollectorByApplication(application)
				.collectDatabaseInformations(requestIndex);
	}

	public List<List<ConnectionInformations>> collectConnectionInformations(String application)
			throws IOException {
		return getRemoteCollectorByApplication(application).collectConnectionInformations();
	}

	public String collectSqlRequestExplainPlan(String application, String sqlRequest)
			throws IOException {
		return getRemoteCollectorByApplication(application)
				.collectSqlRequestExplainPlan(sqlRequest);
	}

	public Map<String, List<ProcessInformations>> collectProcessInformations(String application)
			throws IOException {
		return getRemoteCollectorByApplication(application).collectProcessInformations();
	}

	public List<JndiBinding> collectJndiBindings(String application, String path)
			throws IOException {
		return getRemoteCollectorByApplication(application).collectJndiBindings(path);
	}

	public Map<String, List<MBeanNode>> collectMBeans(String application) throws IOException {
		return getRemoteCollectorByApplication(application).collectMBeans();
	}

	public Map<JavaInformations, List<CounterRequestContext>> collectCurrentRequests(
			String application) throws IOException {
		return getRemoteCollectorByApplication(application).collectCurrentRequests();
	}

	public List<List<ThreadInformations>> getThreadInformationsLists(String application) {
		return getRemoteCollectorByApplication(application).getThreadInformationsLists();
	}

	public void addCollectorApplication(String application, List<URL> urls) throws IOException {
		final Map<String, List<URL>> collectorUrlsByApplications = Parameters
				.getCollectorUrlsByApplications();
		for (final URL addedUrl : urls) {
			final String addedUrlInExternalForm = addedUrl.toExternalForm();
			for (final Map.Entry<String, List<URL>> entry : collectorUrlsByApplications
					.entrySet()) {
				for (final URL existingUrl : entry.getValue()) {
					if (existingUrl.toExternalForm().equals(addedUrlInExternalForm)) {
						throw new IOException("The URL "
								+ addedUrlInExternalForm.substring(0,
										addedUrlInExternalForm.lastIndexOf('/'))
								+ " has already been added in the application " + entry.getKey()
								+ ". You can't monitor an application instance twice.");
					}
				}
			}
		}
		final List<URL> currentUrls = getUrlsByApplication(application);
		final List<URL> nodesUrls;
		if (currentUrls != null) {
			nodesUrls = new ArrayList<URL>(currentUrls);
			nodesUrls.addAll(urls);
			removeCollectorApplication(application);
		} else {
			nodesUrls = urls;
		}
		collectForApplication(application, nodesUrls);
		Parameters.addCollectorApplication(application, nodesUrls);
	}

	public void removeCollectorApplication(String application) throws IOException {
		Parameters.removeCollectorApplication(application);
		final RemoteCollector remoteCollector = remoteCollectorsByApplication.remove(application);
		if (remoteCollector != null && remoteCollector.getCollector() != null) {
			remoteCollector.getCollector().stop();
		}
	}

	public void removeCollectorApplicationNodes(String appName, List<URL> nodeUrls)
			throws IOException {
		final List<URL> currentUrls = getUrlsByApplication(appName);
		if (currentUrls != null) {
			final List<URL> newUrls = new ArrayList<URL>(currentUrls);
			newUrls.removeAll(nodeUrls);
			removeCollectorApplication(appName);
			if (!newUrls.isEmpty()) {
				addCollectorApplication(appName, newUrls);
			}
		}
	}

	/**
	 * Retourne le {@link Collector} pour une application à partir de son code.
	 * @param application Code de l'application
	 * @return Collector
	 */
	public Collector getCollectorByApplication(String application) {
		// application peut être null
		if (application == null) {
			return null;
		}
		final RemoteCollector remoteCollector = remoteCollectorsByApplication.get(application);
		if (remoteCollector == null) {
			return null;
		}
		return remoteCollector.getCollector();
	}

	/**
	 * Retourne la liste des informations java à partir du code de l'application.
	 * @param application Code de l'application
	 * @return Liste de JavaInformations
	 */
	public List<JavaInformations> getJavaInformationsByApplication(String application) {
		// application peut être null
		if (application == null) {
			return null;
		}
		final RemoteCollector remoteCollector = remoteCollectorsByApplication.get(application);
		if (remoteCollector == null) {
			return null;
		}
		return remoteCollector.getJavaInformationsList();
	}

	private RemoteCollector getRemoteCollectorByApplication(String application) {
		assert application != null;
		final RemoteCollector remoteCollector = remoteCollectorsByApplication.get(application);
		assert remoteCollector != null;
		return remoteCollector;
	}

	/**
	 * Retourne true si les données d'une application sont disponibles (c'est-à-dire si au moins
	 * une communication avec l'application a pu avoir lieu).
	 * @param application Code l'application
	 * @return boolean
	 */
	public boolean isApplicationDataAvailable(String application) {
		assert application != null;
		return remoteCollectorsByApplication.containsKey(application);
	}

	/**
	 * Retourne le code de la première application dans la liste.
	 * @return String
	 */
	public String getFirstApplication() {
		if (remoteCollectorsByApplication.isEmpty()) {
			return null;
		}
		return remoteCollectorsByApplication.keySet().iterator().next();
	}

	/**
	 * Retourne la map des dernières erreurs de collecte par codes d'applications ou null
	 * si la dernière collecte pour l'application s'est exécutée sans exception.
	 * @return Map
	 */
	public Map<String, Throwable> getLastCollectExceptionsByApplication() {
		return Collections.unmodifiableMap(lastCollectExceptionsByApplication);
	}

	private void notifyAdmins(String subject, String message) {
		final String mailSession = Parameter.MAIL_SESSION.getValue();
		final String adminEmails = Parameter.ADMIN_EMAILS.getValue();
		if (mailSession != null && adminEmails != null) {
			final Mailer mailer = new Mailer(mailSession);
			try {
				mailer.send(adminEmails, subject, message, null, false);
			} catch (final Exception e) {
				LOGGER.warn(e.toString(), e);
			}
		}
	}

	void scheduleReportMailForCollectorServer(String application) {
		assert application != null;
		for (final Period period : MailReport.getMailPeriods()) {
			scheduleReportMailForCollectorServer(application, period);
		}
	}

	void scheduleReportMailForCollectorServer(final String application, final Period period) {
		assert application != null;
		assert period != null;
		final TimerTask task = new TimerTask() {
			/** {@inheritDoc} */
			@Override
			public void run() {
				try {
					// envoi du rapport
					final Collector collector = getCollectorByApplication(application);
					final List<JavaInformations> javaInformationsList = getJavaInformationsByApplication(
							application);
					new MailReport().sendReportMail(collector, true, javaInformationsList, period);
				} catch (final Throwable t) { // NOPMD
					// pas d'erreur dans cette task
					LOG.warn("sending mail report failed", t);
				}
				// on reschedule à la même heure la semaine suivante sans utiliser de période de 24h*7
				// car certains jours font 23h ou 25h et on ne veut pas introduire de décalage
				scheduleReportMailForCollectorServer(application, period);
			}
		};

		// schedule 1 fois la tâche
		timer.schedule(task, MailReport.getNextExecutionDate(period));
	}

	/**
	 * Stoppe les collectes dans ce serveur de collecte et purge les données.
	 */
	public void stop() {
		// stoppe le timer
		timer.cancel();
		// stoppe les threads de collecte, en attendant qu'ils terminent les tâches en cours
		executorService.shutdown();
		for (final RemoteCollector remoteCollector : remoteCollectorsByApplication.values()) {
			remoteCollector.getCollector().stop();
		}

		// nettoyage avant le retrait de la webapp au cas où celui-ci ne suffise pas
		remoteCollectorsByApplication.clear();
	}

	public static List<URL> getUrlsByApplication(String application) throws IOException {
		assert application != null;
		return Parameters.getCollectorUrlsByApplications().get(application);
	}
}
