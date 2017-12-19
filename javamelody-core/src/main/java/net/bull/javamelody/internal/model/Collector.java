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
import java.util.concurrent.ConcurrentHashMap;

import net.bull.javamelody.JdbcWrapper;
import net.bull.javamelody.internal.common.LOG;
import net.bull.javamelody.internal.common.Parameters;
import net.bull.javamelody.internal.model.Counter.CounterRequestContextComparator;
import net.bull.javamelody.internal.model.SamplingProfiler.SampledMethod;
import net.bull.javamelody.internal.publish.MetricsPublisher;

/**
 * Collecteur de données sur les compteurs, avec son propre thread, pour remplir les courbes.
 * @author Emeric Vernat
 */
public class Collector { // NOPMD
	private static final long NOT_A_NUMBER = Long.MIN_VALUE;

	// période entre 2 collectes en milli-secondes
	private final int periodMillis;
	private final String application;
	private final List<Counter> counters;
	private final SamplingProfiler samplingProfiler;
	private final Map<String, JRobin> requestJRobinsById = new ConcurrentHashMap<String, JRobin>();
	// les instances jrobins des compteurs sont créées à l'initialisation
	private final Map<String, JRobin> counterJRobins = new LinkedHashMap<String, JRobin>();
	private final Map<String, JRobin> otherJRobins = new LinkedHashMap<String, JRobin>();
	// globalRequestsByCounter, requestsById, dayCountersByCounter et cpuTimeMillis
	// sont utilisés par un seul thread lors des collectes,
	// (et la méthode centrale "collect" est synchronisée pour éviter un accès concurrent
	// avec la mise à jour avant le rapport html)
	private final Map<Counter, CounterRequest> globalRequestsByCounter = new HashMap<Counter, CounterRequest>();
	private final Map<String, CounterRequest> requestsById = new HashMap<String, CounterRequest>();
	private final Map<Counter, Counter> dayCountersByCounter = new LinkedHashMap<Counter, Counter>();
	private final Map<Counter, Boolean> firstCollectDoneByCounter = new HashMap<Counter, Boolean>();
	private long transactionCount = NOT_A_NUMBER;
	private long cpuTimeMillis = NOT_A_NUMBER;
	private long gcTimeMillis = NOT_A_NUMBER;
	private long tomcatBytesReceived = NOT_A_NUMBER;
	private long tomcatBytesSent = NOT_A_NUMBER;
	private long lastCollectDuration;
	private long estimatedMemorySize;
	private long diskUsage;
	private Date lastDateOfDeletedObsoleteFiles = new Date();
	private boolean stopped;
	private final boolean noDatabase = Parameters.isNoDatabase();
	private List<MetricsPublisher> metricsPublishers;
	private final WebappVersions webappVersions;
	private final StorageLock storageLock;

	/**
	 * Constructeur.
	 * @param application Code de l'application
	 * @param counters Liste des counters
	 */
	public Collector(String application, List<Counter> counters) {
		this(application, counters, null);
	}

	/**
	 * Constructeur.
	 * @param application Code de l'application
	 * @param counters Liste des counters
	 * @param samplingProfiler SamplingProfiler
	 */
	public Collector(String application, List<Counter> counters,
			SamplingProfiler samplingProfiler) {
		super();
		assert application != null;
		assert counters != null;
		this.application = application;
		this.counters = Collections.unmodifiableList(new ArrayList<Counter>(counters));
		this.samplingProfiler = samplingProfiler;
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
			LOG.debug("counters data read from files in "
					+ Parameters.getStorageDirectory(application));
		} catch (final IOException e) {
			// lecture échouée, tant pis
			// (on n'interrompt pas toute l'initialisation juste pour un fichier illisible)
			LOG.warn("exception while reading counters data from files in "
					+ Parameters.getStorageDirectory(application), e);
		}

		// puis pose le lock
		this.storageLock = new StorageLock(application);

		this.webappVersions = new WebappVersions(application);
	}

	/**
	 * Retourne le code de l'application.
	 * @return String
	 */
	public String getApplication() {
		return application;
	}

	/**
	 * Retourne le {@link SamplingProfiler}.
	 * @return SamplingProfiler
	 */
	public SamplingProfiler getSamplingProfiler() {
		return samplingProfiler;
	}

	public List<SampledMethod> getHotspots() {
		if (samplingProfiler == null) {
			throw new IllegalStateException("Hotspots sampling is not enabled in this server");
		}
		return samplingProfiler.getHotspots(1000);
	}

	public Map<String, Date> getDatesByWebappVersions() {
		return webappVersions.getDatesByVersions();
	}

	/**
	 * @return La liste des {@link Counter}s de ce collector
	 */
	public List<Counter> getCounters() {
		return counters;
	}

	/**
	 * @param counterName Nom d'un counter
	 * @return Le counter de ce collector ayant ce nom ou null si non trouvé
	 */
	public Counter getCounterByName(String counterName) {
		for (final Counter counter : counters) {
			if (counter.getName().equals(counterName)) {
				return counter;
			}
		}
		return null;
	}

	public Counter getCounterByRequestId(CounterRequest request) {
		final String requestId = request.getId();
		for (final Counter counter : counters) {
			if (counter.isRequestIdFromThisCounter(requestId)) {
				return counter;
			}
		}
		return null;
	}

	public List<CounterRequestContext> getRootCurrentContexts(List<Counter> newParentCounters) {
		final List<CounterRequestContext> rootCurrentContexts = new ArrayList<CounterRequestContext>();
		for (final Counter counter : counters) {
			if (counter.isDisplayed()) {
				// a priori, les contextes root courants sont dans le compteur http
				// mais il est possible qu'il y en ait aussi dans ejb ou sql sans parent dans http
				rootCurrentContexts.addAll(counter.getOrderedRootCurrentContexts());
			}
		}
		if (rootCurrentContexts.size() > 1) {
			Collections.sort(rootCurrentContexts, Collections
					.reverseOrder(new CounterRequestContextComparator(System.currentTimeMillis())));

			CounterRequestContext.replaceParentCounters(rootCurrentContexts, newParentCounters);
		}
		return rootCurrentContexts;
	}

	public long getLastCollectDuration() {
		return lastCollectDuration;
	}

	public long getEstimatedMemorySize() {
		return estimatedMemorySize;
	}

	public long getDiskUsage() {
		if (diskUsage == 0) {
			// si diskUsage == 0, le serveur a été démarré ce jour et la taille totale des fichiers
			// n'a pas encore été calculée lors de la purge des fichiers obsolètes,
			// donc on la calcule ici
			final File storageDir = Parameters.getStorageDirectory(application);
			long sum = 0;
			final File[] files = storageDir.listFiles();
			if (files != null) {
				for (final File file : files) {
					sum += file.length();
				}
			}
			diskUsage = sum;
		}
		return diskUsage;
	}

	public List<Counter> getRangeCounters(Range range) throws IOException {
		if (range.getPeriod() == Period.TOUT) {
			return new ArrayList<Counter>(counters);
		}
		final Collection<Counter> currentDayCounters = dayCountersByCounter.values();
		final List<Counter> result = new ArrayList<Counter>(currentDayCounters.size());
		for (final Counter dayCounter : currentDayCounters) {
			final Counter counter = getRangeCounter(range, dayCounter);
			result.add(counter);
		}
		return result;
	}

	private Counter getRangeCounter(Range range, Counter dayCounter) throws IOException {
		final PeriodCounterFactory periodCounterFactory = new PeriodCounterFactory(dayCounter);
		final Counter counter;
		if (range.getPeriod() == null) {
			counter = periodCounterFactory.getCustomCounter(range);
		} else {
			switch (range.getPeriod()) {
			case JOUR:
				counter = periodCounterFactory.getDayCounter();
				break;
			case SEMAINE:
				counter = periodCounterFactory.getWeekCounter();
				break;
			case MOIS:
				counter = periodCounterFactory.getMonthCounter();
				break;
			case ANNEE:
				counter = periodCounterFactory.getYearCounter();
				break;
			case TOUT:
				throw new IllegalStateException(range.getPeriod().toString());
			default:
				throw new IllegalArgumentException(range.getPeriod().toString());
			}
		}
		return counter;
	}

	public List<Counter> getRangeCountersToBeDisplayed(Range range) throws IOException {
		final List<Counter> result = new ArrayList<Counter>(getRangeCounters(range));
		final Iterator<Counter> it = result.iterator();
		while (it.hasNext()) {
			final Counter counter = it.next();
			if (!counter.isDisplayed() || counter.isJobCounter()) {
				it.remove();
			}
		}
		return Collections.unmodifiableList(result);
	}

	public Counter getRangeCounter(Range range, String counterName) throws IOException {
		final Counter counter = getCounterByName(counterName);
		if (counter == null) {
			throw new IllegalArgumentException(counterName);
		}
		if (range.getPeriod() == Period.TOUT) {
			return counter;
		}
		return getRangeCounter(range, dayCountersByCounter.get(counter));
	}

	public void collectLocalContextWithoutErrors() {
		// ici on n'inclue pas les informations de la bdd et des threads
		// car on n'en a pas besoin pour la collecte et cela économise des requêtes sql
		try {
			final JavaInformations javaInformations = new JavaInformations(
					Parameters.getServletContext(), false);

			collectWithoutErrors(Collections.singletonList(javaInformations));
		} catch (final Throwable t) { // NOPMD
			// include cause in message for debugging logs in the report
			LOG.warn("exception while collecting data: " + t.toString(), t);
		}
	}

	public void collectWithoutErrors(List<JavaInformations> javaInformationsList) {
		assert javaInformationsList != null;
		final long start = System.currentTimeMillis();
		try {
			estimatedMemorySize = collect(javaInformationsList);
		} catch (final Throwable t) { // NOPMD
			// include cause in message for debugging logs in the report
			LOG.warn("exception while collecting data: " + t.toString(), t);
		}
		// note : on n'inclue pas "new JavaInformations" de collectLocalContextWithoutErrors
		// dans la durée de la collecte mais il est inférieur à 1 ms (sans bdd)
		lastCollectDuration = Math.max(0, System.currentTimeMillis() - start);
	}

	private synchronized long collect(List<JavaInformations> javaInformationsList)
			throws IOException {
		long memorySize = 0;
		try {
			// si pas d'informations, on ne met pas 0 : on ne met rien
			if (!javaInformationsList.isEmpty()) {
				if (metricsPublishers == null) {
					metricsPublishers = MetricsPublisher.getMetricsPublishers(javaInformationsList);
				}
				collectJavaInformations(javaInformationsList);
				collectOtherJavaInformations(javaInformationsList);
				collectTomcatInformations(javaInformationsList);
			}
			for (final Counter counter : counters) {
				// counter.isDisplayed() peut changer pour spring, ejb, guice ou services selon l'utilisation
				dayCountersByCounter.get(counter).setDisplayed(counter.isDisplayed());
				// collecte pour chaque compteur (hits par minute, temps moyen, % d'erreurs système)
				// Rq : il serait possible d'ajouter le débit total en Ko / minute (pour http)
				// mais autant monitorer les vrais débits réseaux au niveau de l'OS
				if (counter.isDisplayed()) {
					// si le compteur n'est pas affiché (par ex ejb), pas de collecte
					// et pas de persistance de fichiers jrobin ou du compteur
					memorySize += collectCounterData(counter);
				}
			}
		} finally {
			if (metricsPublishers != null) {
				for (final MetricsPublisher metricsPublisher : metricsPublishers) {
					metricsPublisher.send();
				}
			}
		}

		final Calendar calendar = Calendar.getInstance();
		final int currentDayOfYear = calendar.get(Calendar.DAY_OF_YEAR);
		calendar.setTime(lastDateOfDeletedObsoleteFiles);
		if (calendar.get(Calendar.DAY_OF_YEAR) != currentDayOfYear) {
			// 1 fois par jour on supprime tous les fichiers .ser.gz obsolètes (modifiés il y a plus d'un an)
			// et tous les fichiers .rrd obsolètes (modifiés il y a plus de 3 mois)
			try {
				deleteObsoleteFiles();
			} finally {
				lastDateOfDeletedObsoleteFiles = new Date();
			}
		}

		if (!javaInformationsList.isEmpty()) {
			final String webappVersion = javaInformationsList.get(0).getWebappVersion();
			webappVersions.addVersionIfNeeded(webappVersion);
		}

		return memorySize;
	}

	private void collectJavaInformations(List<JavaInformations> javaInformationsList)
			throws IOException {
		long usedMemory = 0;
		long processesCpuTimeMillis = 0;
		int availableProcessors = 0;
		int sessionCount = 0;
		int activeThreadCount = 0;
		int activeConnectionCount = 0;
		int usedConnectionCount = 0;

		for (final JavaInformations javaInformations : javaInformationsList) {
			final MemoryInformations memoryInformations = javaInformations.getMemoryInformations();
			usedMemory = add(memoryInformations.getUsedMemory(), usedMemory);
			sessionCount = add(javaInformations.getSessionCount(), sessionCount);
			activeThreadCount = add(javaInformations.getActiveThreadCount(), activeThreadCount);
			activeConnectionCount = add(javaInformations.getActiveConnectionCount(),
					activeConnectionCount);
			usedConnectionCount = add(javaInformations.getUsedConnectionCount(),
					usedConnectionCount);

			// il y a au moins 1 coeur
			availableProcessors = add(Math.max(javaInformations.getAvailableProcessors(), 1),
					availableProcessors);
			// processesCpuTime n'est supporté que par le jdk sun
			processesCpuTimeMillis = add(javaInformations.getProcessCpuTimeMillis(),
					processesCpuTimeMillis);
		}
		collectJRobinValues(usedMemory, processesCpuTimeMillis, availableProcessors, sessionCount,
				activeThreadCount, activeConnectionCount, usedConnectionCount);
	}

	// CHECKSTYLE:OFF
	private void collectOtherJavaInformations(List<JavaInformations> javaInformationsList) // NOPMD
			throws IOException {
		// CHECKSTYLE:ON
		long usedNonHeapMemory = 0;
		long usedBufferedMemory = 0;
		int loadedClassesCount = 0;
		long garbageCollectionTimeMillis = 0;
		long usedPhysicalMemorySize = 0;
		long usedSwapSpaceSize = 0;
		int availableProcessors = 0;
		int sessionCount = 0;
		long sessionAgeSum = 0;
		int threadCount = 0;
		long databaseTransactionCount = 0;
		double systemLoadAverage = 0;
		long unixOpenFileDescriptorCount = 0;
		long freeDiskSpaceInTemp = Long.MAX_VALUE;
		double systemCpuLoad = 0;

		for (final JavaInformations javaInformations : javaInformationsList) {
			final MemoryInformations memoryInformations = javaInformations.getMemoryInformations();
			sessionCount = add(javaInformations.getSessionCount(), sessionCount);
			sessionAgeSum = add(javaInformations.getSessionAgeSum(), sessionAgeSum);
			threadCount = add(javaInformations.getThreadCount(), threadCount);
			databaseTransactionCount = add(javaInformations.getTransactionCount(),
					databaseTransactionCount);
			// il y a au moins 1 coeur
			availableProcessors = add(Math.max(javaInformations.getAvailableProcessors(), 1),
					availableProcessors);
			usedNonHeapMemory = add(memoryInformations.getUsedNonHeapMemory(), usedNonHeapMemory);
			usedBufferedMemory = add(memoryInformations.getUsedBufferedMemory(),
					usedBufferedMemory);
			loadedClassesCount = add(memoryInformations.getLoadedClassesCount(),
					loadedClassesCount);
			usedPhysicalMemorySize = add(memoryInformations.getUsedPhysicalMemorySize(),
					usedPhysicalMemorySize);
			usedSwapSpaceSize = add(memoryInformations.getUsedSwapSpaceSize(), usedSwapSpaceSize);
			garbageCollectionTimeMillis = add(memoryInformations.getGarbageCollectionTimeMillis(),
					garbageCollectionTimeMillis);
			// systemLoadAverage n'est supporté qu'à partir du jdk 1.6 sur linux ou unix
			systemLoadAverage = add(javaInformations.getSystemLoadAverage(), systemLoadAverage);
			// que sur linx ou unix
			unixOpenFileDescriptorCount = add(javaInformations.getUnixOpenFileDescriptorCount(),
					unixOpenFileDescriptorCount);
			if (javaInformations.getFreeDiskSpaceInTemp() >= 0) {
				// la valeur retenue est le minimum entre les serveurs
				freeDiskSpaceInTemp = Math.min(javaInformations.getFreeDiskSpaceInTemp(),
						freeDiskSpaceInTemp);
			}
			systemCpuLoad = add(javaInformations.getSystemCpuLoad(), systemCpuLoad);
		}

		// collecte du pourcentage de temps en ramasse-miette
		if (garbageCollectionTimeMillis >= 0) {
			if (this.gcTimeMillis != NOT_A_NUMBER) {
				// %gc = delta(somme(Temps GC)) / période / nb total de coeurs
				final int gcPercentage = Math
						.min((int) ((garbageCollectionTimeMillis - this.gcTimeMillis) * 100
								/ periodMillis / availableProcessors), 100);
				addJRobinValue(getOtherJRobin("gc"), gcPercentage);
			} else {
				addJRobinValue(getOtherJRobin("gc"), 0d);
			}
			this.gcTimeMillis = garbageCollectionTimeMillis;
		}

		final Map<String, Double> otherJRobinsValues = new LinkedHashMap<String, Double>();
		otherJRobinsValues.put("threadCount", (double) threadCount);
		otherJRobinsValues.put("loadedClassesCount", (double) loadedClassesCount);
		otherJRobinsValues.put("usedBufferedMemory", (double) usedBufferedMemory);
		otherJRobinsValues.put("usedNonHeapMemory", (double) usedNonHeapMemory);
		otherJRobinsValues.put("usedPhysicalMemorySize", (double) usedPhysicalMemorySize);
		otherJRobinsValues.put("usedSwapSpaceSize", (double) usedSwapSpaceSize);
		otherJRobinsValues.put("systemLoad", systemLoadAverage);
		otherJRobinsValues.put("systemCpuLoad", systemCpuLoad / javaInformationsList.size());
		otherJRobinsValues.put("fileDescriptors", (double) unixOpenFileDescriptorCount);
		for (final Map.Entry<String, Double> entry : otherJRobinsValues.entrySet()) {
			if (entry.getValue() >= 0) {
				addJRobinValue(getOtherJRobin(entry.getKey()), entry.getValue());
			}
		}

		collectSessionsMeanAge(sessionAgeSum, sessionCount);

		if (!noDatabase) {
			// collecte du nombre de transactions base de données par minute
			if (this.transactionCount != NOT_A_NUMBER) {
				final double periodMinutes = periodMillis / 60000d;
				addJRobinValue(getOtherJRobin("transactionsRate"),
						(databaseTransactionCount - this.transactionCount) / periodMinutes);
			} else {
				addJRobinValue(getOtherJRobin("transactionsRate"), 0d);
			}
			this.transactionCount = databaseTransactionCount;
		}

		if (freeDiskSpaceInTemp != Long.MAX_VALUE) {
			addJRobinValue(getOtherJRobin("Free_disk_space"), freeDiskSpaceInTemp);
		}

		// on pourrait collecter la valeur 100 dans jrobin pour qu'il fasse la moyenne
		// du pourcentage de disponibilité, mais cela n'aurait pas de sens sans
		// différenciation des indisponibilités prévues de celles non prévues
	}

	private void collectTomcatInformations(List<JavaInformations> javaInformationsList)
			throws IOException {
		int tomcatBusyThreads = 0;
		long bytesReceived = 0;
		long bytesSent = 0;
		boolean tomcatUsed = false;

		for (final JavaInformations javaInformations : javaInformationsList) {
			for (final TomcatInformations tomcatInformations : javaInformations
					.getTomcatInformationsList()) {
				tomcatBusyThreads = add(tomcatInformations.getCurrentThreadsBusy(),
						tomcatBusyThreads);
				bytesReceived = add(tomcatInformations.getBytesReceived(), bytesReceived);
				bytesSent = add(tomcatInformations.getBytesSent(), bytesSent);
				tomcatUsed = true;
			}
		}

		if (tomcatUsed) {
			// collecte des informations de Tomcat
			collectTomcatValues(tomcatBusyThreads, bytesReceived, bytesSent);
		}
	}

	private void collectJRobinValues(long usedMemory, long processesCpuTimeMillis,
			int availableProcessors, int sessionCount, int activeThreadCount,
			int activeConnectionCount, int usedConnectionCount) throws IOException {
		// collecte de la mémoire java
		addJRobinValue(getCounterJRobin("usedMemory"), usedMemory);

		// collecte du pourcentage d'utilisation cpu
		if (processesCpuTimeMillis >= 0) {
			// processesCpuTimeMillis est la somme pour tous les serveurs (et pour tous les coeurs)
			// donc ce temps peut être n fois supérieur à periodMillis
			// où n est le nombre total de coeurs sur tous les serveurs (si cluster);
			// et cpuPercentage s'approchera à pleine charge de 100
			// quel que soit le nombre de serveurs ou de coeurs;
			// cpuPercentage ne peut être supérieur à 100
			// car ce serait une valeur aberrante due aux imprécisions de mesure

			if (this.cpuTimeMillis != NOT_A_NUMBER) {
				// en gros, %cpu = delta(somme(Temps cpu)) / période / nb total de coeurs
				final int cpuPercentage = Math
						.min((int) ((processesCpuTimeMillis - this.cpuTimeMillis) * 100
								/ periodMillis / availableProcessors), 100);
				addJRobinValue(getCounterJRobin("cpu"), cpuPercentage);
			} else {
				addJRobinValue(getCounterJRobin("cpu"), 0d);
			}
			this.cpuTimeMillis = processesCpuTimeMillis;
		}

		// si ce collector est celui des nodes Jenkins, il n'y a pas de requêtes http
		// donc il ne peut pas y avoir de sessions http ou de threads actifs
		if (getCounterByName(Counter.HTTP_COUNTER_NAME) != null) {
			// collecte du nombre de sessions http
			if (sessionCount >= 0) {
				addJRobinValue(getCounterJRobin("httpSessions"), sessionCount);
			}
			// collecte du nombre de threads actifs (requêtes http en cours)
			addJRobinValue(getCounterJRobin("activeThreads"), activeThreadCount);
		}
		if (!noDatabase) {
			// collecte du nombre de connexions jdbc actives et du nombre de connexions jdbc ouvertes
			addJRobinValue(getCounterJRobin("activeConnections"), activeConnectionCount);
			addJRobinValue(getCounterJRobin("usedConnections"), usedConnectionCount);
		}

		// si ce collector est celui des nodes Jenkins, on collecte le nombre de builds en cours
		// pour le graphique
		if (getCounterByName(Counter.BUILDS_COUNTER_NAME) != null) {
			addJRobinValue(getCounterJRobin("runningBuilds"), JdbcWrapper.getRunningBuildCount());
			addJRobinValue(getCounterJRobin("buildQueueLength"), JdbcWrapper.getBuildQueueLength());
		}
	}

	private void collectTomcatValues(int tomcatBusyThreads, long bytesReceived, long bytesSent)
			throws IOException {
		addJRobinValue(getOtherJRobin("tomcatBusyThreads"), tomcatBusyThreads);
		if (this.tomcatBytesSent != NOT_A_NUMBER) {
			final double periodMinutes = periodMillis / 60000d;
			addJRobinValue(getOtherJRobin("tomcatBytesReceived"),
					(bytesReceived - this.tomcatBytesReceived) / periodMinutes);
			addJRobinValue(getOtherJRobin("tomcatBytesSent"),
					(bytesSent - this.tomcatBytesSent) / periodMinutes);
		} else {
			addJRobinValue(getOtherJRobin("tomcatBytesReceived"), 0d);
			addJRobinValue(getOtherJRobin("tomcatBytesSent"), 0d);
		}
		this.tomcatBytesReceived = bytesReceived;
		this.tomcatBytesSent = bytesSent;
	}

	private void collectSessionsMeanAge(long sessionAgeSum, int sessionCount) throws IOException {
		if (sessionCount >= 0 && getCounterByName(Counter.HTTP_COUNTER_NAME) != null) {
			final long sessionAgeMeanInMinutes;
			if (sessionCount > 0) {
				sessionAgeMeanInMinutes = sessionAgeSum / sessionCount / 60000;
			} else {
				sessionAgeMeanInMinutes = -1;
			}
			addJRobinValue(getOtherJRobin("httpSessionsMeanAge"), sessionAgeMeanInMinutes);
		}
	}

	private void addJRobinValue(JRobin jRobin, double value) throws IOException {
		jRobin.addValue(value);
		// if value == -1, then do not publish that metric
		if (value >= 0) {
			for (final MetricsPublisher metricsPublisher : metricsPublishers) {
				metricsPublisher.addValue(jRobin.getName(), value);
			}
		}
	}

	private static double add(double t1, double t2) {
		// avec des serveurs monitorés sur des OS/JVM multiples (windows, linux par exemple),
		// des valeurs peuvent être négatives ie non disponibles pour une JVM/OS
		// et positives ie disponibles pour une autre JVM/OS
		// (par exemple, systemLoadAverage est négatif sur windows et positif sur linux,
		// et dans ce cas, si windows et linux alors on ne somme pas et on garde linux,
		// si linux et windows alors on garde linux,
		// si linux et linux alors on somme linux+linux qui est positif
		// si windows et windows alors on somme windows+windows qui est négatif
		if (t1 < 0d && t2 > 0d) {
			return t2;
		} else if (t1 > 0d && t2 < 0d) {
			return t1;
		}
		return t1 + t2;
	}

	private static long add(long t1, long t2) {
		if (t1 < 0L && t2 > 0L) {
			return t2;
		} else if (t1 > 0L && t2 < 0L) {
			return t1;
		}
		return t1 + t2;
	}

	private static int add(int t1, int t2) {
		if (t1 < 0 && t2 > 0) {
			return t2;
		} else if (t1 > 0 && t2 < 0) {
			return t1;
		}
		return t1 + t2;
	}

	private long collectCounterData(Counter counter) throws IOException {
		// counterName vaut http, sql ou ws par exemple
		final String counterName = counter.getName();
		final List<CounterRequest> requests = counter.getRequests();
		if (!counter.isErrorCounter()) {
			// on calcule les totaux depuis le départ
			final CounterRequest newGlobalRequest = new CounterRequest(counterName + " global",
					counterName);
			for (final CounterRequest request : requests) {
				// ici, pas besoin de synchronized sur request puisque ce sont des clones indépendants
				newGlobalRequest.addHits(request);
			}

			// on récupère les instances de jrobin même s'il n'y a pas de hits ou pas de précédents totaux
			// pour être sûr qu'elles soient initialisées (si pas instanciée alors pas de courbe)
			final JRobin hitsJRobin;
			final JRobin meanTimesJRobin;
			final JRobin systemErrorsJRobin;
			if (!counter.isJspOrStrutsCounter()) {
				hitsJRobin = getCounterJRobin(counterName + "HitsRate");
				meanTimesJRobin = getCounterJRobin(counterName + "MeanTimes");
				systemErrorsJRobin = getCounterJRobin(counterName + "SystemErrors");
			} else {
				hitsJRobin = getOtherJRobin(counterName + "HitsRate");
				meanTimesJRobin = getOtherJRobin(counterName + "MeanTimes");
				systemErrorsJRobin = getOtherJRobin(counterName + "SystemErrors");
			}

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
				addJRobinValue(hitsJRobin, hitsParMinute);
				// s'il n'y a pas eu de hits, alors la moyenne vaut -1 : elle n'a pas de sens
				if (hits > 0) {
					addJRobinValue(meanTimesJRobin, lastPeriodGlobalRequest.getMean());
					addJRobinValue(systemErrorsJRobin,
							lastPeriodGlobalRequest.getSystemErrorPercentage());

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
		final long dayCounterEstimatedMemorySize = collectCounterRequestsAndErrorsData(counter,
				requests);
		return counter.getEstimatedMemorySize() + dayCounterEstimatedMemorySize;
	}

	private long collectCounterRequestsAndErrorsData(Counter counter, List<CounterRequest> requests)
			throws IOException {
		final Counter dayCounter = getCurrentDayCounter(counter);
		final boolean firstCollectDoneForCounter = Boolean.TRUE
				.equals(firstCollectDoneByCounter.get(counter));
		final List<CounterRequest> filteredRequests = filterRequestsIfOverflow(counter, requests);
		for (final CounterRequest newRequest : filteredRequests) {
			collectCounterRequestData(dayCounter, newRequest, firstCollectDoneForCounter);
		}
		if (dayCounter.getRequestsCount() > dayCounter.getMaxRequestsCount()) {
			// issue 339: ne pas laisser dans dayCounter trop de requêtes si elles sont à chaque fois différentes
			filterRequestsIfOverflow(dayCounter, dayCounter.getRequests());
		}
		if (dayCounter.isErrorCounter()) {
			dayCounter.addErrors(getDeltaOfErrors(counter, dayCounter));
		}
		dayCounter.writeToFile();
		if (!firstCollectDoneForCounter) {
			firstCollectDoneByCounter.put(counter, Boolean.TRUE);
		}
		return dayCounter.getEstimatedMemorySize();
	}

	private List<CounterRequest> filterRequestsIfOverflow(Counter counter,
			List<CounterRequest> requests) {
		final int maxRequestsCount = counter.getMaxRequestsCount();
		if (requests.size() <= maxRequestsCount) {
			return requests;
		}
		final List<CounterRequest> result = new ArrayList<CounterRequest>(requests);
		for (final CounterRequest request : requests) {
			if (result.size() > maxRequestsCount && request.getHits() < 10) {
				// Si le nombre de requêtes est supérieur à 10000
				// on suppose que l'application a des requêtes sql non bindées
				// (bien que cela ne soit en général pas conseillé).
				// En tout cas, on essaye ici d'éviter de saturer
				// la mémoire (et le disque dur) avec toutes ces requêtes
				// différentes en éliminant celles ayant moins de 10 hits.
				removeRequest(counter, request);
				result.remove(request);
				continue;
			}
		}
		while (result.size() > maxRequestsCount && !result.isEmpty()) {
			// cas extrême: si le nombre de requêtes est encore trop grand,
			// on enlève n'importe quelle requête
			final CounterRequest request = result.get(0);
			removeRequest(counter, request);
			result.remove(request);
		}
		return result;
	}

	private void collectCounterRequestData(Counter dayCounter, CounterRequest newRequest,
			boolean firstCollectDoneForCounter) throws IOException {
		final String requestStorageId = newRequest.getId();

		final CounterRequest request = requestsById.get(requestStorageId);
		if (request != null) {
			// idem : on clone et on soustrait les requêtes précédentes
			// sauf si c'est l'initialisation
			final CounterRequest lastPeriodRequest = newRequest.clone();
			lastPeriodRequest.removeHits(request);
			// avec la condition getHits() > 1 au lieu de getHits() > 0, on évite de créer des fichiers RRD
			// pour les toutes les requêtes appelées une seule fois sur la dernière période
			// et notamment pour les requêtes http "écrites au hasard" (par exemple, pour tester les failles d'un site web) ;
			// cela réduit la place occupée par de nombreux fichiers très peu utiles
			// (et s'il y a eu 0 hit, alors la moyenne vaut -1 : elle n'a pas de sens)
			if (lastPeriodRequest.getHits() > 1 && !dayCounter.isJspOrStrutsCounter()
					&& (!dayCounter.isErrorCounter() || dayCounter.isJobCounter())) {
				// on ne crée jamais de graphiques pour les "jsp", "error" et "job" car peu utiles
				// et potentiellement lourd en usage disque et en mémoire utilisée
				final JRobin requestJRobin = getRequestJRobin(requestStorageId,
						newRequest.getName());
				// plus nécessaire: if (dayCounter.isErrorCounter()) requestJRobin.addValue(lastPeriodRequest.getHits());

				// pas addJRobinValue ici, il y en aurait trop pour Graphite
				requestJRobin.addValue(lastPeriodRequest.getMean());
			}

			// agrégation de la requête sur le compteur pour le jour courant
			dayCounter.addHits(lastPeriodRequest);
		} else if (firstCollectDoneForCounter) {
			// si c'est la première collecte pour ce compteur (!firstCollectDoneForCounter), alors on n'ajoute pas
			// newRequest dans dayCounter car cela ajouterait la première fois tout le contenu
			// de la période "tout" dans le dayCounter du jour,
			// (attention : la collecte d'un compteur peut être plus tard, issue 242),
			// mais si c'est une collecte suivante (firstCollectDoneForCounter), alors on ajoute
			// newRequest dans dayCounter car il s'agit simplement d'une nouvelle requête
			// qui n'avait pas encore été rencontrée dans la période "tout"
			dayCounter.addHits(newRequest);
		}
		requestsById.put(requestStorageId, newRequest);
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

	void deleteObsoleteFiles() throws IOException {
		final long rrdDiskUsage = CounterStorage.deleteObsoleteCounterFiles(getApplication());
		final long serGzDiskUsage = JRobin.deleteObsoleteJRobinFiles(getApplication());
		diskUsage = rrdDiskUsage + serGzDiskUsage;
		// il manque la taille du fichier "last_shutdown.html", mais on n'est pas à ça près
		LOG.debug("Obsolete files deleted. JavaMelody disk usage: " + diskUsage / 1024 + " KB");
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

	private JRobin getOtherJRobin(String name) throws IOException {
		JRobin jrobin = otherJRobins.get(name);
		if (jrobin == null) {
			jrobin = JRobin.createInstance(getApplication(), name, null);
			otherJRobins.put(name, jrobin);
		}
		return jrobin;
	}

	public JRobin getJRobin(String graphName) throws IOException {
		JRobin jrobin = counterJRobins.get(graphName);
		if (jrobin == null) {
			jrobin = otherJRobins.get(graphName);
			if (jrobin == null) {
				jrobin = requestJRobinsById.get(graphName);
				if (jrobin == null) {
					// un graph n'est pas toujours de suite dans jrobin
					// Use a temporary JRobin instance to create the graph.
					// The real request name can not be got at this point, passing the graphName as the request name.
					jrobin = JRobin.createInstanceIfFileExists(getApplication(), graphName,
							graphName);
				}
			}
		}
		return jrobin;
	}

	public Collection<JRobin> getCounterJRobins() {
		return Collections.unmodifiableCollection(counterJRobins.values());
	}

	Collection<JRobin> getOtherJRobins() {
		return Collections.unmodifiableCollection(otherJRobins.values());
	}

	public Collection<JRobin> getDisplayedCounterJRobins() {
		return getDisplayedJRobins(counterJRobins.values());
	}

	public Collection<JRobin> getDisplayedOtherJRobins() {
		return getDisplayedJRobins(otherJRobins.values());
	}

	private Collection<JRobin> getDisplayedJRobins(Collection<JRobin> jrobins) {
		final List<JRobin> displayedJRobins = new ArrayList<JRobin>(jrobins.size());
		for (final JRobin jrobin : jrobins) {
			final String jrobinName = jrobin.getName();
			boolean displayed = true;
			// inutile car on ne génère pas les jrobin pour le counter de ce nom là
			//		if (jrobinName.startsWith(Counter.ERROR_COUNTER_NAME)) {
			//			displayed = false;
			//		} else {
			for (final Counter counter : getCounters()) {
				if (jrobinName.startsWith(counter.getName())) {
					displayed = counter.isDisplayed();
					break;
				}
			}
			if (displayed) {
				displayedJRobins.add(jrobin);
			}
		}
		return Collections.unmodifiableCollection(displayedJRobins);
	}

	/**
	 * Purge les données pour un compteur à partir de son nom.
	 * @param counterName Nom du compteur
	 */
	void clearCounter(String counterName) {
		final Counter counter = getCounterByName(counterName);
		if (counter != null) {
			final List<CounterRequest> requests = counter.getRequests();
			// on réinitialise le counter
			counter.clear();
			// et on purge les données correspondantes du collector utilisées pour les deltas
			globalRequestsByCounter.remove(counter);
			for (final CounterRequest request : requests) {
				requestsById.remove(request.getId());
				requestJRobinsById.remove(request.getId());
			}
		}
	}

	public boolean isStorageUsedByMultipleInstances() {
		return !storageLock.isAcquired();
	}

	public void stop() {
		try {
			try {
				// on persiste les compteurs pour les relire à l'initialisation et ne pas perdre les stats
				for (final Counter counter : counters) {
					counter.writeToFile();
				}
			} finally {
				storageLock.release();
			}
		} catch (final IOException e) {
			// persistance échouée, tant pis
			LOG.warn("exception while writing counters data to files", e);
		} finally {
			try {
				for (final Counter counter : counters) {
					counter.clear();
				}
			} finally {
				if (metricsPublishers != null) {
					for (final MetricsPublisher metricsPublisher : metricsPublishers) {
						metricsPublisher.stop();
					}
				}
			}
			stopped = true;
			// ici on ne fait pas de nettoyage de la liste counters car cette méthode
			// est appelée sur la webapp monitorée quand il y a un serveur de collecte
			// et que cette liste est envoyée au serveur de collecte,
			// et on ne fait pas de nettoyage des maps qui servent dans le cas
			// où le monitoring de la webapp monitorée est appelée par un navigateur
			// directement même si il y a par ailleurs un serveur de collecte
			// (dans ce dernier cas les données sont bien sûr partielles)
		}
	}

	public boolean isStopped() {
		return stopped;
	}

	public static void stopJRobin() {
		try {
			JRobin.stop();
		} catch (final Throwable t) { // NOPMD
			LOG.warn("stopping jrobin failed", t);
		}
	}

	public static void detachVirtualMachine() {
		try {
			VirtualMachine.detach();
		} catch (final Throwable t) { // NOPMD
			LOG.warn("exception while detaching virtual machine", t);
		}
	}

	/** {@inheritDoc} */
	@Override
	public String toString() {
		return getClass().getSimpleName() + "[application=" + getApplication() + ", periodMillis="
				+ periodMillis + ", counters=" + getCounters() + ']';
	}
}
