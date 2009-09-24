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

import java.io.IOException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Timer;
import java.util.concurrent.ConcurrentHashMap;

import net.bull.javamelody.Counter.CounterRequestContextComparator;

/**
 * Collecteur de données sur les compteurs, avec son propre thread, pour remplir les courbes.
 * @author Emeric Vernat
 */
final class Collector {
	// période entre 2 collectes en milli-secondes
	private final int periodMillis;
	private final Timer timer;
	private final String application;
	private final List<Counter> counters;
	private final Map<String, JRobin> requestJRobinsById = new ConcurrentHashMap<String, JRobin>();
	// les instances jrobins des compteurs sont créées à l'initialisation
	private final Map<String, JRobin> counterJRobins = new LinkedHashMap<String, JRobin>();
	// globalRequestsByCounter, requestsById, dayCountersByCounter et cpuTimeMillis
	// sont utilisés par un seul thread lors des collectes,
	// (et la méthode centrale "collect" est synchronisée pour éviter un accès concurrent
	// avec la mise à jour avant le rapport html)
	private final Map<Counter, CounterRequest> globalRequestsByCounter = new HashMap<Counter, CounterRequest>();
	private final Map<String, CounterRequest> requestsById = new HashMap<String, CounterRequest>();
	private final Map<Counter, Counter> dayCountersByCounter = new LinkedHashMap<Counter, Counter>();
	private long cpuTimeMillis;
	private long gcTimeMillis;
	private long lastCollectDuration;

	/**
	 * Constructeur.
	 * @param application Code de l'application
	 * @param counters Liste des counters
	 * @param timer Timer pour ordonnancer des tâches sur le thread du monitoring
	 */
	Collector(String application, List<Counter> counters, Timer timer) {
		super();
		assert application != null;
		assert counters != null;
		assert timer != null;
		this.timer = timer;
		this.application = application;
		this.counters = Collections.unmodifiableList(new ArrayList<Counter>(counters));
		// c'est le collector qui fixe le nom de l'application (avant la lecture des éventuels fichiers)
		for (final Counter counter : counters) {
			for (final Counter otherCounter : counters) {
				// on vérifie que les noms des compteurs sont uniques entre eux
				assert counter == otherCounter || !counter.getName().equals(otherCounter.getName());
			}
			counter.setApplication(application);
			final Counter dayCounter = new PeriodCounterFactory(counter)
					.createDayCounterAtDate(new Date());
			dayCountersByCounter.put(counter, dayCounter);
		}
		periodMillis = Parameters.getResolutionSeconds() * 1000;

		try {
			// on relit les compteurs à l'initialisation pour récupérer les stats;
			// d'abord les compteurs non temporels, au cas où les compteurs par jour soient illisibles,
			for (final Counter counter : counters) {
				counter.readFromFile();
			}
			// et seulement ensuite les compteurs du jour
			for (final Counter counter : counters) {
				dayCountersByCounter.get(counter).readFromFile();
			}
		} catch (final IOException e) {
			// lecture échouée, tant pis
			// (on n'interrompt pas toute l'initialisation juste pour un fichier illisible)
			printStackTrace(e);
		}
	}

	/**
	 * Retourne le code de l'application.
	 * @return String
	 */
	String getApplication() {
		return application;
	}

	/**
	 * @return La liste des counters de ce collector
	 */
	List<Counter> getCounters() {
		return counters;
	}

	List<CounterRequestContext> getRootCurrentContexts() {
		final List<CounterRequestContext> rootCurrentContexts = new ArrayList<CounterRequestContext>();
		for (final Counter counter : counters) {
			if (counter.isDisplayed()) {
				// à priori, les contextes root courants sont dans le compteur http
				// mais il est possible qu'il y en ait aussi dans ejb ou sql sans parent dans http
				rootCurrentContexts.addAll(counter.getOrderedRootCurrentContexts());
			}
		}
		if (rootCurrentContexts.size() > 1) {
			Collections.sort(rootCurrentContexts, Collections
					.reverseOrder(new CounterRequestContextComparator(System.currentTimeMillis())));
		}
		return rootCurrentContexts;
	}

	long getLastCollectDuration() {
		return lastCollectDuration;
	}

	private List<Counter> getPeriodCounters(Period period) throws IOException {
		final Collection<Counter> currentDayCounters = dayCountersByCounter.values();
		final List<Counter> result = new ArrayList<Counter>(currentDayCounters.size());
		switch (period) {
		case JOUR:
			result.addAll(currentDayCounters);
			break;
		case SEMAINE:
			for (final Counter dayCounter : currentDayCounters) {
				result.add(new PeriodCounterFactory(dayCounter).getWeekCounter());
			}
			break;
		case MOIS:
			for (final Counter dayCounter : currentDayCounters) {
				result.add(new PeriodCounterFactory(dayCounter).getMonthCounter());
			}
			break;
		case ANNEE:
			for (final Counter dayCounter : currentDayCounters) {
				result.add(new PeriodCounterFactory(dayCounter).getYearCounter());
			}
			break;
		case TOUT:
			result.addAll(counters);
			break;
		default:
			throw new IllegalArgumentException(period.toString());
		}
		return result;
	}

	List<Counter> getPeriodCountersToBeDisplayed(Period period) throws IOException {
		final List<Counter> result = new ArrayList<Counter>(getPeriodCounters(period));
		final Iterator<Counter> it = result.iterator();
		while (it.hasNext()) {
			final Counter counter = it.next();
			if (!counter.isDisplayed()) {
				it.remove();
			}
		}
		return Collections.unmodifiableList(result);
	}

	void collectLocalContextWithoutErrors() {
		// ici on n'inclue pas les informations de la bdd et des threads
		// car on n'en a pas besoin pour la collecte et cela économise des requêtes sql
		final JavaInformations javaInformations = new JavaInformations(Parameters
				.getServletContext(), false);

		collectWithoutErrors(Collections.singletonList(javaInformations));
	}

	void collectWithoutErrors(List<JavaInformations> javaInformationsList) {
		assert javaInformationsList != null;
		final long start = System.currentTimeMillis();
		try {
			collect(javaInformationsList);
		} catch (final Throwable t) { // NOPMD
			printStackTrace(t);
		}
		// note : on n'inclue pas "new JavaInformations" de collectLocalContextWithoutErrors
		// mais il est inférieur à 1 ms (sans bdd)
		lastCollectDuration = Math.max(0, System.currentTimeMillis() - start);
	}

	private void collect(List<JavaInformations> javaInformationsList) throws IOException {
		synchronized (this) {
			collectJavaInformations(javaInformationsList);

			for (final Counter counter : counters) {
				// collecte pour chaque compteur (hits par minute, temps moyen, % d'erreurs système)
				// Rq : il serait possible d'ajouter le débit total en Ko / minute (pour http)
				// mais autant monitorer les vrais débits réseaux au niveau de l'OS
				if (counter.isDisplayed()) {
					// si le compteur n'est pas affiché (par ex ejb), pas de collecte
					// et pas de persistance de fichiers jrobin ou du compteur
					collectCounterData(counter);
				}
			}
		}
	}

	private void collectJavaInformations(List<JavaInformations> javaInformationsList)
			throws IOException {
		if (javaInformationsList.isEmpty()) {
			// si pas d'informations, on ne met pas 0 : on ne met rien
			return;
		}
		long usedMemory = 0;
		long processesCpuTimeMillis = 0;
		long garbageCollectionTimeMillis = 0;
		int availableProcessors = 0;
		long sessionCount = 0;
		long activeThreadCount = 0;
		long activeConnectionCount = 0;
		long usedConnectionCount = 0;
		double systemLoadAverage = 0;
		long unixOpenFileDescriptorCount = 0;

		for (final JavaInformations javaInformations : javaInformationsList) {
			usedMemory += javaInformations.getMemoryInformations().getUsedMemory();
			sessionCount += javaInformations.getSessionCount();
			activeThreadCount += javaInformations.getActiveThreadCount();
			activeConnectionCount += javaInformations.getActiveConnectionCount();
			usedConnectionCount += javaInformations.getUsedConnectionCount();
			availableProcessors += javaInformations.getAvailableProcessors();

			if (javaInformations.getProcessCpuTimeMillis() >= 0) {
				// processesCpuTime n'est supporté que par le jdk sun
				processesCpuTimeMillis += javaInformations.getProcessCpuTimeMillis();
			} else {
				processesCpuTimeMillis = -1;
			}
			if (javaInformations.getMemoryInformations().getGarbageCollectionTimeMillis() >= 0) {
				garbageCollectionTimeMillis += javaInformations.getMemoryInformations()
						.getGarbageCollectionTimeMillis();
			} else {
				garbageCollectionTimeMillis = -1;
			}
			if (javaInformations.getSystemLoadAverage() >= 0) {
				// systemLoadAverage n'est supporté qu'à partir du jdk 1.6 sur linux ou unix
				systemLoadAverage += javaInformations.getSystemLoadAverage();
			} else {
				systemLoadAverage = -1;
			}
			if (javaInformations.getUnixOpenFileDescriptorCount() >= 0) {
				// que sur linx ou unix
				unixOpenFileDescriptorCount += javaInformations.getUnixOpenFileDescriptorCount();
			} else {
				unixOpenFileDescriptorCount = -1;
			}
		}
		// collecte de la mémoire java
		getCounterJRobin("usedMemory").addValue(usedMemory);

		if (availableProcessors == 0) {
			availableProcessors = 1; // il y a au moins 1 coeur
		}
		// collecte du pourcentage d'utilisation cpu
		collectCpu(processesCpuTimeMillis, availableProcessors);
		// collecte du nombre de sessions http
		getCounterJRobin("httpSessions").addValue(sessionCount);
		// collecte de la charge système, du pourcentage de temps en ramasse-miette et des descripteurs de fichiers
		collectForNix(systemLoadAverage, garbageCollectionTimeMillis, availableProcessors,
				unixOpenFileDescriptorCount);

		// collecte du nombre de threads actifs (requêtes http en cours), du nombre de connexions jdbc actives
		// et du nombre de connexions jdbc ouvertes
		getCounterJRobin("activeThreads").addValue(activeThreadCount);
		getCounterJRobin("activeConnections").addValue(activeConnectionCount);
		getCounterJRobin("usedConnections").addValue(usedConnectionCount);

		// on pourrait collecter la valeur 100 dans jrobin pour qu'il fasse la moyenne
		// du pourcentage de disponibilité, mais cela n'aurait pas de sens sans
		// différenciation des indisponibilités prévues de celles non prévues
	}

	private void collectCpu(long processesCpuTimeMillis, int availableProcessors)
			throws IOException {
		if (processesCpuTimeMillis >= 0) {
			// processesCpuTimeMillis est la somme pour tous les serveurs (et pour tous les coeurs)
			// donc ce temps peut être n fois supérieur à periodMillis
			// où n est le nombre total de coeurs sur tous les serveurs (si cluster);
			// et cpuPercentage s'approchera à pleine charge de 100
			// quel que soit le nombre de serveurs ou de coeurs;
			// cpuPercentage ne peut être supérieur à 100
			// car ce serait une valeur aberrante due aux imprécisions de mesure

			// en gros, %cpu = delta(somme(Temps cpu)) / période / nb total de coeurs
			final int cpuPercentage = Math.min((int) ((processesCpuTimeMillis - this.cpuTimeMillis)
					* 100 / periodMillis / availableProcessors), 100);
			getCounterJRobin("cpu").addValue(cpuPercentage);
			this.cpuTimeMillis = processesCpuTimeMillis;
		}
	}

	private void collectForNix(double systemLoadAverage, long garbageCollectionTimeMillis,
			int availableProcessors, long unixOpenFileDescriptorCount) throws IOException {
		if (systemLoadAverage >= 0) {
			getCounterJRobin("systemLoad").addValue(systemLoadAverage);
			// on ne collecte le temps gc et le nombre de file descriptors
			// que si on a systemLoadAverage (linux ou unix) pour faire une ligne
			if (garbageCollectionTimeMillis >= 0) {
				// %gc = delta(somme(Temps GC)) / période / nb total de coeurs
				final int gcPercentage = Math.min(
						(int) ((garbageCollectionTimeMillis - this.gcTimeMillis) * 100
								/ periodMillis / availableProcessors), 100);
				getCounterJRobin("gc").addValue(gcPercentage);
				this.gcTimeMillis = garbageCollectionTimeMillis;
			}
			if (unixOpenFileDescriptorCount >= 0) {
				getCounterJRobin("fileDescriptors").addValue(unixOpenFileDescriptorCount);
			}
		}
	}

	private void collectCounterData(Counter counter) throws IOException {
		// counterName vaut http, sql ou ws par exemple
		final String counterName = counter.getName();
		final boolean errorCounter = counter.isErrorCounter();
		final List<CounterRequest> requests = counter.getRequests();
		if (!errorCounter) {
			// on calcule les totaux depuis le départ
			final CounterRequest newGlobalRequest = new CounterRequest(counterName + " global",
					counterName);
			for (final CounterRequest request : requests) {
				// ici, pas besoin de synchronized sur request puisque ce sont des clones indépendants
				newGlobalRequest.addHits(request);
			}

			// on récupère les instances de jrobin même s'il n'y a pas de hits ou pas de précédents totaux
			// pour être sûr qu'elles soient initialisées (si pas instanciée alors pas de courbe)
			final JRobin hitsJRobin = getCounterJRobin(counterName + "HitsRate");
			final JRobin meanTimesJRobin = getCounterJRobin(counterName + "MeanTimes");
			final JRobin systemErrorsJRobin = getCounterJRobin(counterName + "SystemErrors");

			final CounterRequest globalRequest = globalRequestsByCounter.get(counter);
			if (globalRequest != null) {
				// on clone et on soustrait les précédents totaux
				// pour obtenir les totaux sur la dernière période
				// rq : s'il n'y a de précédents totaux (à l'initialisation)
				// alors on n'inscrit pas de valeurs car les nouveaux hits
				// ne seront connus (en delta) qu'au deuxième passage
				// (au 1er passage, globalRequest contient déjà les données lues sur disque)
				final CounterRequest lastPeriodGlobalRequest = newGlobalRequest.clone();
				lastPeriodGlobalRequest.removeHits(globalRequest);

				final long hits = lastPeriodGlobalRequest.getHits();
				final long hitsParMinute = hits * 60 * 1000 / periodMillis;

				// on remplit le stockage avec les données
				hitsJRobin.addValue(hitsParMinute);
				// s'il n'y a pas eu de hits, alors la moyenne vaut -1 : elle n'a pas de sens
				if (hits > 0) {
					meanTimesJRobin.addValue(lastPeriodGlobalRequest.getMean());
					systemErrorsJRobin.addValue(lastPeriodGlobalRequest.getSystemErrorPercentage());

					// s'il y a eu des requêtes, on persiste le compteur pour ne pas perdre les stats
					// en cas de crash ou d'arrêt brutal (mais normalement ils seront aussi persistés
					// lors de l'arrêt du serveur)
					counter.writeToFile();
				}
			}

			// on sauvegarde les nouveaux totaux pour la prochaine fois
			globalRequestsByCounter.put(counter, newGlobalRequest);
		}

		// données de temps moyen pour les courbes par requête
		collectCounterRequestsAndErrorsData(counter, requests);
	}

	private void collectCounterRequestsAndErrorsData(Counter counter, List<CounterRequest> requests)
			throws IOException {
		final boolean errorCounter = counter.isErrorCounter();
		int size = requests.size();
		final Counter dayCounter = getCurrentDayCounter(counter);
		final int maxRequestsCount = counter.getMaxRequestsCount();
		for (final CounterRequest newRequest : requests) {
			if (size > maxRequestsCount && newRequest.getHits() < 10) {
				// Si le nombre de requêtes est supérieur à 20000
				// on suppose que l'application a des requêtes sql non bindées
				// (bien que cela ne soit en général pas conseillé).
				// En tout cas, on essaye ici d'éviter de saturer
				// la mémoire (et le disque dur) avec toutes ces requêtes
				// différentes en éliminant celles ayant moins de 10 hits.
				removeRequest(counter, newRequest);
				size--;
				continue;
			}

			final String requestStorageId = newRequest.getId();
			// on récupère les instances de jrobin même s'il n'y a pas pas de précédents totaux
			final JRobin requestJRobin = getRequestJRobin(requestStorageId, newRequest.getName());

			final CounterRequest request = requestsById.get(requestStorageId);
			if (request != null) {
				// idem : on clone et on soustrait les requêtes précédentes
				// sauf si c'est l'initialisation
				final CounterRequest lastPeriodRequest = newRequest.clone();
				lastPeriodRequest.removeHits(request);
				if (lastPeriodRequest.getHits() > 0) {
					if (errorCounter) {
						requestJRobin.addValue(lastPeriodRequest.getHits());
					} else {
						// s'il n'y a pas eu de hits, alors la moyenne vaut -1 : elle n'a pas de sens
						requestJRobin.addValue(lastPeriodRequest.getMean());
					}
					// agrégation de la requête sur le compteur pour le jour courant
					dayCounter.addHits(lastPeriodRequest);
				}
			}
			requestsById.put(requestStorageId, newRequest);
		}
		if (dayCounter.isErrorCounter()) {
			dayCounter.addErrors(getDeltaOfErrors(counter, dayCounter));
		}
		dayCounter.writeToFile();
	}

	private List<CounterError> getDeltaOfErrors(Counter counter, Counter dayCounter) {
		final List<CounterError> errors = counter.getErrors();
		if (errors.isEmpty()) {
			return Collections.emptyList();
		}
		final long lastErrorTime;
		final List<CounterError> dayErrors = dayCounter.getErrors();
		if (dayErrors.isEmpty()) {
			lastErrorTime = dayCounter.getStartDate().getTime();
		} else {
			lastErrorTime = dayErrors.get(dayErrors.size() - 1).getTime();
		}
		final List<CounterError> errorsOfDay = new ArrayList<CounterError>();
		for (final CounterError error : errors) {
			// il peut arriver de manquer une erreur dans l'affichage par jour
			// si on récupère la liste et qu'il y a une nouvelle erreur dans la même ms
			// mais tant pis et il y a peu de chance que cela arrive
			if (error.getTime() > lastErrorTime) {
				errorsOfDay.add(error);
			}
		}
		return errorsOfDay;
	}

	private Counter getCurrentDayCounter(Counter counter) throws IOException {
		Counter dayCounter = dayCountersByCounter.get(counter);
		final Calendar calendar = Calendar.getInstance();
		final int currentDayOfYear = calendar.get(Calendar.DAY_OF_YEAR);
		calendar.setTime(dayCounter.getStartDate());
		if (calendar.get(Calendar.DAY_OF_YEAR) != currentDayOfYear) {
			// le jour a changé, on crée un compteur vide qui sera enregistré dans un nouveau fichier
			dayCounter = new PeriodCounterFactory(dayCounter).buildNewDayCounter();
			dayCountersByCounter.put(counter, dayCounter);
		}
		return dayCounter;
	}

	private void removeRequest(Counter counter, CounterRequest newRequest) {
		counter.removeRequest(newRequest.getName());
		requestsById.remove(newRequest.getId());
		final JRobin requestJRobin = requestJRobinsById.remove(newRequest.getId());
		if (requestJRobin != null) {
			requestJRobin.deleteFile();
		}
	}

	private JRobin getRequestJRobin(String requestId, String requestName) throws IOException {
		JRobin jrobin = requestJRobinsById.get(requestId);
		if (jrobin == null) {
			jrobin = JRobin.createInstance(getApplication(), requestId, requestName);
			requestJRobinsById.put(requestId, jrobin);
		}
		return jrobin;
	}

	private JRobin getCounterJRobin(String name) throws IOException {
		JRobin jrobin = counterJRobins.get(name);
		if (jrobin == null) {
			jrobin = JRobin.createInstance(getApplication(), name, null);
			counterJRobins.put(name, jrobin);
		}
		return jrobin;
	}

	JRobin getJRobin(String graphName) {
		JRobin jrobin = counterJRobins.get(graphName);
		if (jrobin == null) {
			jrobin = requestJRobinsById.get(graphName);
			if (jrobin == null) {
				// un graph n'est pas toujours de suite dans jrobin
				return null;
			}
		}
		return jrobin;
	}

	Collection<JRobin> getCounterJRobins() {
		return Collections.unmodifiableCollection(counterJRobins.values());
	}

	/**
	 * Purge les données pour un compteur à partir de son nom.
	 * @param counterName Nom du compteur
	 */
	void clearCounter(String counterName) {
		for (final Counter counter : counters) {
			if (counter.getName().equalsIgnoreCase(counterName)) {
				final List<CounterRequest> requests = counter.getRequests();
				// on réinitialise le counter
				counter.clear();
				// et on purge les données correspondantes du collector utilisées pour les deltas
				globalRequestsByCounter.remove(counter);
				for (final CounterRequest request : requests) {
					requestsById.remove(request.getId());
					requestJRobinsById.remove(request.getId());
				}
				break;
			}
		}
	}

	void stop() {
		try {
			timer.cancel();

			// on persiste les compteurs pour les relire à l'initialisation et ne pas perdre les stats
			for (final Counter counter : counters) {
				counter.writeToFile();
			}
		} catch (final IOException e) {
			// persistance échouée, tant pis
			printStackTrace(e);
		} finally {
			for (final Counter counter : counters) {
				counter.clear();
			}
			// ici on ne fait pas de nettoyage de la liste counters car cette méthode
			// est appelée sur la webapp monitorée quand il y a un serveur de collecte
			// et que cette liste est envoyée au serveur de collecte,
			// et on ne fait pas de nettoyage des maps qui servent dans le cas
			// où le monitoring de la webapp monitorée est appelée par un navigateur
			// directement même si il y a par ailleurs un serveur de collecte
			// (dans ce dernier cas les données sont bien sûr partielles)
		}
	}

	static void stopJRobin() {
		try {
			JRobin.stop();
		} catch (final Throwable t) { // NOPMD
			printStackTrace(t);
		}
	}

	static void detachVirtualMachine() {
		try {
			VirtualMachine.detach();
		} catch (final Throwable t) { // NOPMD
			printStackTrace(t);
		}
	}

	static void printStackTrace(Throwable t) {
		// ne connaissant pas log4j ici, on ne sait pas loguer ailleurs que dans la sortie d'erreur
		t.printStackTrace(System.err);
	}

	/** {@inheritDoc} */
	@Override
	public String toString() {
		return getClass().getSimpleName() + "[application=" + getApplication() + ", periodMillis="
				+ periodMillis + ", counters=" + getCounters() + ']';
	}
}
