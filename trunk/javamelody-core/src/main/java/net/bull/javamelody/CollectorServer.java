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

import static net.bull.javamelody.HttpParameters.DATABASE_PART;
import static net.bull.javamelody.HttpParameters.PART_PARAMETER;
import static net.bull.javamelody.HttpParameters.REQUEST_PARAMETER;

import java.io.IOException;
import java.io.Serializable;
import java.net.URL;
import java.util.ArrayList;
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

	private final Map<String, Collector> collectorsByApplication = new LinkedHashMap<String, Collector>();
	private final Map<String, List<JavaInformations>> javaInformationsByApplication = new LinkedHashMap<String, List<JavaInformations>>();
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
				assert collectorsByApplication.size() == javaInformationsByApplication.size();
				lastCollectExceptionsByApplication.remove(application);
			} catch (final Throwable e) { // NOPMD
				// si erreur sur une webapp (indisponibilité par exemple), on continue avec les autres
				// et il ne doit y avoir aucune erreur dans cette task
				LOGGER.warn(e.getMessage(), e);
				lastCollectExceptionsByApplication.put(application, e);
			}
		}
	}

	String collectForApplication(String application, List<URL> urls) throws IOException {
		LOGGER.info("collect for the application " + application + " on " + urls);
		assert application != null;
		assert urls != null;
		final long start = System.currentTimeMillis();
		final String messageForReport = collectDataForApplication(application, urls);

		final List<JavaInformations> javaInformationsList = javaInformationsByApplication
				.get(application);
		final Collector collector = collectorsByApplication.get(application);
		if (collector == null) {
			// collector peut être null si ce collectorServer vient d'être arrêté par exemple
			javaInformationsByApplication.remove(application);
			return null;
		}
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

	private String collectDataForApplication(String application, List<URL> urls) throws IOException {
		final List<JavaInformations> javaInformationsList = new ArrayList<JavaInformations>();
		final StringBuilder sb = new StringBuilder();
		Collector collector = collectorsByApplication.get(application);
		for (final URL url : urls) {
			final List<Serializable> serialized = new LabradorRetriever(url).call();
			final List<Counter> counters = new ArrayList<Counter>();
			for (final Serializable serializable : serialized) {
				if (serializable instanceof Counter) {
					final Counter counter = (Counter) serializable;
					counter.setApplication(application);
					counters.add(counter);
				} else if (serializable instanceof JavaInformations) {
					final JavaInformations newJavaInformations = (JavaInformations) serializable;
					javaInformationsList.add(newJavaInformations);
				} else if (serializable instanceof String) {
					sb.append(serializable).append('\n');
				}
			}
			if (collector == null) {
				// on initialise les collectors au fur et à mesure
				// puisqu'on ne peut pas forcément au démarrage
				// car la webapp à monitorer peut être indisponible
				collector = createCollector(application, counters);
				collectorsByApplication.put(application, collector);
			} else {
				addRequestsAndErrors(collector, counters);
			}
		}
		javaInformationsByApplication.put(application, javaInformationsList);
		final String messageForReport;
		if (sb.length() == 0) {
			messageForReport = null;
		} else {
			messageForReport = sb.toString();
		}
		return messageForReport;
	}

	List<SessionInformations> collectSessionInformations(String application, String sessionId)
			throws IOException {
		assert application != null;
		// sessionId est null si on veut toutes les sessions
		if (sessionId == null) {
			// récupération à la demande des sessions
			final List<SessionInformations> sessionsInformations = new ArrayList<SessionInformations>();
			for (final URL url : getUrlsByApplication(application)) {
				final URL sessionsUrl = new URL(url.toString() + '&'
						+ HttpParameters.PART_PARAMETER + '=' + HttpParameters.SESSIONS_PART);
				final LabradorRetriever labradorRetriever = new LabradorRetriever(sessionsUrl);
				final List<SessionInformations> sessions = labradorRetriever.call();
				sessionsInformations.addAll(sessions);
			}
			SessionListener.sortSessions(sessionsInformations);
			return sessionsInformations;
		}
		SessionInformations found = null;
		for (final URL url : getUrlsByApplication(application)) {
			final URL sessionsUrl = new URL(url.toString() + '&' + HttpParameters.PART_PARAMETER
					+ '=' + HttpParameters.SESSIONS_PART + '&'
					+ HttpParameters.SESSION_ID_PARAMETER + '=' + sessionId);
			final LabradorRetriever labradorRetriever = new LabradorRetriever(sessionsUrl);
			final SessionInformations session = (SessionInformations) labradorRetriever.call();
			if (session != null) {
				found = session;
				break;
			}
		}
		// si found est toujours null, alors la session a été invalidée
		return Collections.singletonList(found);
	}

	HeapHistogram collectHeapHistogram(String application) throws IOException {
		assert application != null;
		// récupération à la demande des HeapHistogram
		HeapHistogram heapHistoTotal = null;
		for (final URL url : getUrlsByApplication(application)) {
			final URL heapHistoUrl = new URL(url.toString() + '&' + HttpParameters.PART_PARAMETER
					+ '=' + HttpParameters.HEAP_HISTO_PART);
			final LabradorRetriever labradorRetriever = new LabradorRetriever(heapHistoUrl);
			final HeapHistogram heapHisto = labradorRetriever.call();
			if (heapHistoTotal == null) {
				heapHistoTotal = heapHisto;
			} else {
				heapHistoTotal.add(heapHisto);
			}
		}
		return heapHistoTotal;
	}

	DatabaseInformations collectDatabaseInformations(String application, int requestIndex)
			throws IOException {
		final URL url = getUrlsByApplication(application).get(0);
		final URL processesUrl = new URL(url.toString() + '&' + PART_PARAMETER + '='
				+ DATABASE_PART + '&' + REQUEST_PARAMETER + '=' + requestIndex);
		return new LabradorRetriever(processesUrl).call();
	}

	private void addRequestsAndErrors(Collector collector, List<Counter> counters) {
		for (final Counter newCounter : counters) {
			final Counter counter = collector.getCounterByName(newCounter.getName());
			// counter.isDisplayed() peut changer pour spring, ejb ou services selon l'utilisation
			counter.setDisplayed(newCounter.isDisplayed());
			counter.addRequestsAndErrors(newCounter);
		}
	}

	private Collector createCollector(String application, List<Counter> counters) {
		final Collector collector = new Collector(application, counters, timer);
		if (Parameters.getParameter(Parameter.MAIL_SESSION) != null
				&& Parameters.getParameter(Parameter.ADMIN_EMAILS) != null) {
			scheduleReportMailForCollectorServer(application);
			LOGGER.info("Periodic report scheduled for the application " + application + " to "
					+ Parameters.getParameter(Parameter.ADMIN_EMAILS));
		}
		return collector;
	}

	void addCollectorApplication(String application, List<URL> urls) throws IOException {
		collectForApplication(application, urls);
		Parameters.addCollectorApplication(application, urls);
	}

	void removeCollectorApplication(String application) throws IOException {
		Parameters.removeCollectorApplication(application);
		collectorsByApplication.remove(application);
		javaInformationsByApplication.remove(application);
	}

	/**
	 * Retourne le collector pour une application à partir de son code.
	 * @param application Code de l'application
	 * @return Collector
	 */
	Collector getCollectorByApplication(String application) {
		return collectorsByApplication.get(application);
	}

	/**
	 * Retourne la liste des informations java à partir du code de l'application.
	 * @param application Code de l'application
	 * @return Liste de JavaInformations
	 */
	List<JavaInformations> getJavaInformationsByApplication(String application) {
		return javaInformationsByApplication.get(application);
	}

	/**
	 * Retourne la map des dernières erreurs de collecte par codes d'applications ou null
	 * si la dernière collecte pour l'application s'est exécutée sans exception.
	 * @return Map
	 */
	Map<String, Throwable> getLastCollectExceptionsByApplication() {
		return Collections.unmodifiableMap(lastCollectExceptionsByApplication);
	}

	/**
	 * Retourne le code de la première application dans la liste
	 * @return String
	 */
	String getFirstApplication() {
		if (collectorsByApplication.isEmpty()) {
			return null;
		}
		return collectorsByApplication.keySet().iterator().next();
	}

	/**
	 * Retourne true si les données d'une application sont disponibles (c'est-à-dire si au moins
	 * une communication avec l'application a pu avoir lieu)
	 * @param application Code l'application
	 * @return boolean
	 */
	boolean isApplicationDataAvailable(String application) {
		assert application != null;
		return collectorsByApplication.containsKey(application)
				&& javaInformationsByApplication.containsKey(application);
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
		for (final Collector collector : collectorsByApplication.values()) {
			collector.stop();
		}
		timer.cancel();

		// nettoyage avant le retrait de la webapp au cas où celui-ci ne suffise pas
		collectorsByApplication.clear();
		javaInformationsByApplication.clear();
	}

	static List<URL> getUrlsByApplication(String application) throws IOException {
		assert application != null;
		return Parameters.getCollectorUrlsByApplications().get(application);
	}
}
