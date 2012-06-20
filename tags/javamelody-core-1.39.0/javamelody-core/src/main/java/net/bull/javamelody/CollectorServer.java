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

import java.io.IOException;
import java.net.URL;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Timer;
import java.util.TimerTask;

import org.apache.log4j.Logger;

/**
 * Collecteur de données du serveur de collecte centralisé.
 * @author Emeric Vernat
 */
class CollectorServer {
	private static final Logger LOGGER = Logger.getLogger("javamelody");

	private final Map<String, RemoteCollector> remoteCollectorsByApplication = new LinkedHashMap<String, RemoteCollector>();
	private final Map<String, Throwable> lastCollectExceptionsByApplication = new LinkedHashMap<String, Throwable>();

	private final Timer timer;

	/**
	 * Constructeur.
	 * @throws IOException e
	 */
	CollectorServer() throws IOException {
		super();
		boolean initOk = false;
		this.timer = new Timer("collector", true);
		try {
			LOGGER.info("reading applications list from: "
					+ Parameters.getCollectorApplicationsFile());
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
			JRobin.setJRobinThreadName("jrobin");
			initOk = true;
		} finally {
			if (!initOk) {
				// si exception dans initialisation, on annule la création du timer
				// (sinon tomcat ne serait pas content)
				timer.cancel();
			}
		}
	}

	void collectWithoutErrors() {
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
			try {
				collectForApplication(application, urls);
				lastCollectExceptionsByApplication.remove(application);
			} catch (final Throwable e) { // NOPMD
				// si erreur sur une webapp (indisponibilité par exemple), on continue avec les autres
				// et il ne doit y avoir aucune erreur dans cette task
				LOGGER.warn(e.getMessage(), e);
				lastCollectExceptionsByApplication.put(application, e);
			}
		}
	}

	String collectForApplicationForAction(String application, List<URL> urls) throws IOException {
		return collectForApplication(new RemoteCollector(application, urls));
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

			if (Parameters.getParameter(Parameter.MAIL_SESSION) != null
					&& Parameters.getParameter(Parameter.ADMIN_EMAILS) != null) {
				scheduleReportMailForCollectorServer(application);
				LOGGER.info("Periodic report scheduled for the application " + application + " to "
						+ Parameters.getParameter(Parameter.ADMIN_EMAILS));
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
				LOGGER.debug("message " + application + " : "
						+ messageForReport.replace("\n", ", "));
			}
		}
		return messageForReport;
	}

	List<SessionInformations> collectSessionInformations(String application, String sessionId)
			throws IOException {
		return getRemoteCollectorByApplication(application).collectSessionInformations(sessionId);
	}

	HeapHistogram collectHeapHistogram(String application) throws IOException {
		return getRemoteCollectorByApplication(application).collectHeapHistogram();
	}

	DatabaseInformations collectDatabaseInformations(String application, int requestIndex)
			throws IOException {
		return getRemoteCollectorByApplication(application).collectDatabaseInformations(
				requestIndex);
	}

	List<List<ConnectionInformations>> collectConnectionInformations(String application)
			throws IOException {
		return getRemoteCollectorByApplication(application).collectConnectionInformations();
	}

	String collectSqlRequestExplainPlan(String application, String sqlRequest) throws IOException {
		return getRemoteCollectorByApplication(application)
				.collectSqlRequestExplainPlan(sqlRequest);
	}

	Map<String, List<ProcessInformations>> collectProcessInformations(String application)
			throws IOException {
		return getRemoteCollectorByApplication(application).collectProcessInformations();
	}

	List<List<ThreadInformations>> getThreadInformationsLists(String application) {
		return getRemoteCollectorByApplication(application).getThreadInformationsLists();
	}

	void addCollectorApplication(String application, List<URL> urls) throws IOException {
		final Map<String, List<URL>> collectorUrlsByApplications = Parameters
				.getCollectorUrlsByApplications();
		for (final URL addedUrl : urls) {
			final String addedUrlInExternalForm = addedUrl.toExternalForm();
			for (final Map.Entry<String, List<URL>> entry : collectorUrlsByApplications.entrySet()) {
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
		collectForApplication(application, urls);
		Parameters.addCollectorApplication(application, urls);
	}

	void removeCollectorApplication(String application) throws IOException {
		Parameters.removeCollectorApplication(application);
		remoteCollectorsByApplication.remove(application);
	}

	/**
	 * Retourne le collector pour une application à partir de son code.
	 * @param application Code de l'application
	 * @return Collector
	 */
	Collector getCollectorByApplication(String application) {
		// application peut être null
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
	List<JavaInformations> getJavaInformationsByApplication(String application) {
		// application peut être null
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
	 * une communication avec l'application a pu avoir lieu)
	 * @param application Code l'application
	 * @return boolean
	 */
	boolean isApplicationDataAvailable(String application) {
		assert application != null;
		return remoteCollectorsByApplication.containsKey(application);
	}

	/**
	 * Retourne le code de la première application dans la liste
	 * @return String
	 */
	String getFirstApplication() {
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
	Map<String, Throwable> getLastCollectExceptionsByApplication() {
		return Collections.unmodifiableMap(lastCollectExceptionsByApplication);
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
					final List<JavaInformations> javaInformationsList = getJavaInformationsByApplication(application);
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
	void stop() {
		timer.cancel();
		for (final RemoteCollector remoteCollector : remoteCollectorsByApplication.values()) {
			remoteCollector.getCollector().stop();
		}

		// nettoyage avant le retrait de la webapp au cas où celui-ci ne suffise pas
		remoteCollectorsByApplication.clear();
	}

	static List<URL> getUrlsByApplication(String application) throws IOException {
		assert application != null;
		return Parameters.getCollectorUrlsByApplications().get(application);
	}
}
