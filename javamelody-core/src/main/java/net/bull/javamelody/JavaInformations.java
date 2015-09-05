/*
 * Copyright 2008-2014 by Emeric Vernat
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
package net.bull.javamelody;

import java.io.Serializable;
import java.lang.management.ManagementFactory;
import java.lang.management.OperatingSystemMXBean;
import java.lang.management.ThreadMXBean;
import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import javax.servlet.ServletContext;
import javax.sql.DataSource;

/**
 * Informations systèmes sur le serveur, sans code html de présentation.
 * L'état d'une instance est initialisé à son instanciation et non mutable;
 * il est donc de fait thread-safe.
 * Cet état est celui d'une instance de JVM java, de ses threads et du système à un instant t.
 * Les instances sont sérialisables pour pouvoir être transmises au serveur de collecte.
 * @author Emeric Vernat
 */
class JavaInformations implements Serializable { // NOPMD
	static final double HIGH_USAGE_THRESHOLD_IN_PERCENTS = 95d;
	private static final long serialVersionUID = 3281861236369720876L;
	private static final Date START_DATE = new Date();
	private static boolean localWebXmlExists = true; // true par défaut
	private static boolean localPomXmlExists = true; // true par défaut
	private final MemoryInformations memoryInformations;
	@SuppressWarnings("all")
	private final List<TomcatInformations> tomcatInformationsList;
	private final int sessionCount;
	private final long sessionAgeSum;
	private final int activeThreadCount;
	private final int usedConnectionCount;
	private final int maxConnectionCount;
	private final int activeConnectionCount;
	private final long transactionCount;
	private final long processCpuTimeMillis;
	private final double systemLoadAverage;
	private final long unixOpenFileDescriptorCount;
	private final long unixMaxFileDescriptorCount;
	private final String host;
	private final String os;
	private final int availableProcessors;
	private final String javaVersion;
	private final String jvmVersion;
	private final String pid;
	private final String serverInfo;
	private final String contextPath;
	private final String contextDisplayName;
	private final Date startDate;
	private final String jvmArguments;
	private final long freeDiskSpaceInTemp;
	private final int threadCount;
	private final int peakThreadCount;
	private final long totalStartedThreadCount;
	private final String dataBaseVersion;
	private final String dataSourceDetails;
	@SuppressWarnings("all")
	private final List<ThreadInformations> threadInformationsList;
	@SuppressWarnings("all")
	private final List<CacheInformations> cacheInformationsList;
	@SuppressWarnings("all")
	private final List<JobInformations> jobInformationsList;
	@SuppressWarnings("all")
	private final List<String> dependenciesList;
	private final boolean webXmlExists = localWebXmlExists;
	private final boolean pomXmlExists = localPomXmlExists;

	static final class ThreadInformationsComparator implements Comparator<ThreadInformations>,
			Serializable {
		private static final long serialVersionUID = 1L;

		/** {@inheritDoc} */
		@Override
		public int compare(ThreadInformations thread1, ThreadInformations thread2) {
			return thread1.getName().compareToIgnoreCase(thread2.getName());
		}
	}

	static final class CacheInformationsComparator implements Comparator<CacheInformations>,
			Serializable {
		private static final long serialVersionUID = 1L;

		/** {@inheritDoc} */
		@Override
		public int compare(CacheInformations cache1, CacheInformations cache2) {
			return cache1.getName().compareToIgnoreCase(cache2.getName());
		}
	}

	static final class JobInformationsComparator implements Comparator<JobInformations>,
			Serializable {
		private static final long serialVersionUID = 1L;

		/** {@inheritDoc} */
		@Override
		public int compare(JobInformations job1, JobInformations job2) {
			return job1.getName().compareToIgnoreCase(job2.getName());
		}
	}

	// CHECKSTYLE:OFF
	JavaInformations(ServletContext servletContext, boolean includeDetails) {
		// CHECKSTYLE:ON
		super();
		memoryInformations = new MemoryInformations();
		tomcatInformationsList = TomcatInformations.buildTomcatInformationsList();
		sessionCount = SessionListener.getSessionCount();
		sessionAgeSum = SessionListener.getSessionAgeSum();
		activeThreadCount = JdbcWrapper.getActiveThreadCount();
		usedConnectionCount = JdbcWrapper.getUsedConnectionCount();
		activeConnectionCount = JdbcWrapper.getActiveConnectionCount();
		maxConnectionCount = JdbcWrapper.getMaxConnectionCount();
		transactionCount = JdbcWrapper.getTransactionCount();
		systemLoadAverage = buildSystemLoadAverage();
		processCpuTimeMillis = buildProcessCpuTimeMillis();
		unixOpenFileDescriptorCount = buildOpenFileDescriptorCount();
		unixMaxFileDescriptorCount = buildMaxFileDescriptorCount();
		host = Parameters.getHostName() + '@' + Parameters.getHostAddress();
		os = buildOS();
		availableProcessors = Runtime.getRuntime().availableProcessors();
		javaVersion = System.getProperty("java.runtime.name") + ", "
				+ System.getProperty("java.runtime.version");
		jvmVersion = System.getProperty("java.vm.name") + ", "
				+ System.getProperty("java.vm.version") + ", " + System.getProperty("java.vm.info");
		if (servletContext == null) {
			serverInfo = null;
			contextPath = null;
			contextDisplayName = null;
			dependenciesList = null;
		} else {
			serverInfo = servletContext.getServerInfo();
			contextPath = Parameters.getContextPath(servletContext);
			contextDisplayName = servletContext.getServletContextName();
			dependenciesList = buildDependenciesList(servletContext);
		}
		startDate = START_DATE;
		jvmArguments = buildJvmArguments();
		final ThreadMXBean threadBean = ManagementFactory.getThreadMXBean();
		threadCount = threadBean.getThreadCount();
		peakThreadCount = threadBean.getPeakThreadCount();
		totalStartedThreadCount = threadBean.getTotalStartedThreadCount();
		freeDiskSpaceInTemp = Parameters.TEMPORARY_DIRECTORY.getFreeSpace();

		if (includeDetails) {
			dataBaseVersion = buildDataBaseVersion();
			dataSourceDetails = buildDataSourceDetails();
			threadInformationsList = buildThreadInformationsList();
			cacheInformationsList = CacheInformations.buildCacheInformationsList();
			jobInformationsList = JobInformations.buildJobInformationsList();
			pid = PID.getPID();
		} else {
			dataBaseVersion = null;
			dataSourceDetails = null;
			threadInformationsList = null;
			cacheInformationsList = null;
			jobInformationsList = null;
			pid = null;
		}
	}

	static void setWebXmlExistsAndPomXmlExists(boolean webXmlExists, boolean pomXmlExists) {
		localWebXmlExists = webXmlExists;
		localPomXmlExists = pomXmlExists;
	}

	boolean doesWebXmlExists() {
		return webXmlExists;
	}

	boolean doesPomXmlExists() {
		return pomXmlExists;
	}

	private static String buildOS() {
		final String name = System.getProperty("os.name");
		final String version = System.getProperty("os.version");
		final String patchLevel = System.getProperty("sun.os.patch.level");
		final String arch = System.getProperty("os.arch");
		final String bits = System.getProperty("sun.arch.data.model");

		final StringBuilder sb = new StringBuilder();
		sb.append(name).append(", ");
		if (!name.toLowerCase(Locale.ENGLISH).contains("windows")) {
			// version is "6.1" and useless for os.name "Windows 7",
			// and can be "2.6.32-358.23.2.el6.x86_64" for os.name "Linux"
			sb.append(version).append(' ');
		}
		if (!"unknown".equals(patchLevel)) {
			// patchLevel is "unknown" and useless on Linux,
			// and can be "Service Pack 1" on Windows
			sb.append(patchLevel);
		}
		sb.append(", ").append(arch).append('/').append(bits);
		return sb.toString();
	}

	private static long buildProcessCpuTimeMillis() {
		final OperatingSystemMXBean operatingSystem = ManagementFactory.getOperatingSystemMXBean();
		if (isSunOsMBean(operatingSystem)) {
			// nano-secondes converties en milli-secondes
			return MemoryInformations.getLongFromOperatingSystem(operatingSystem,
					"getProcessCpuTime") / 1000000;
		}
		return -1;
	}

	private static long buildOpenFileDescriptorCount() {
		final OperatingSystemMXBean operatingSystem = ManagementFactory.getOperatingSystemMXBean();
		if (isSunOsMBean(operatingSystem) && isSunUnixMBean(operatingSystem)) {
			try {
				return MemoryInformations.getLongFromOperatingSystem(operatingSystem,
						"getOpenFileDescriptorCount");
			} catch (final Error e) {
				// pour issue 16 (using jsvc on ubuntu or debian)
				return -1;
			}
		}
		return -1;
	}

	private static long buildMaxFileDescriptorCount() {
		final OperatingSystemMXBean operatingSystem = ManagementFactory.getOperatingSystemMXBean();
		if (isSunOsMBean(operatingSystem) && isSunUnixMBean(operatingSystem)) {
			try {
				return MemoryInformations.getLongFromOperatingSystem(operatingSystem,
						"getMaxFileDescriptorCount");
			} catch (final Error e) {
				// pour issue 16 (using jsvc on ubuntu or debian)
				return -1;
			}
		}
		return -1;
	}

	private static double buildSystemLoadAverage() {
		// System load average for the last minute.
		// The system load average is the sum of
		// the number of runnable entities queued to the available processors
		// and the number of runnable entities running on the available processors
		// averaged over a period of time.
		final OperatingSystemMXBean operatingSystem = ManagementFactory.getOperatingSystemMXBean();
		if (operatingSystem.getSystemLoadAverage() >= 0) {
			// systemLoadAverage n'existe qu'à partir du jdk 1.6
			return operatingSystem.getSystemLoadAverage();
		}
		return -1;
	}

	private static String buildJvmArguments() {
		final StringBuilder jvmArgs = new StringBuilder();
		for (final String jvmArg : ManagementFactory.getRuntimeMXBean().getInputArguments()) {
			jvmArgs.append(jvmArg).append('\n');
		}
		if (jvmArgs.length() > 0) {
			jvmArgs.deleteCharAt(jvmArgs.length() - 1);
		}
		return jvmArgs.toString();
	}

	static List<ThreadInformations> buildThreadInformationsList() {
		final ThreadMXBean threadBean = ManagementFactory.getThreadMXBean();
		final Map<Thread, StackTraceElement[]> stackTraces = Thread.getAllStackTraces();
		final List<Thread> threads = new ArrayList<Thread>(stackTraces.keySet());

		// si "1.6.0_01".compareTo(Parameters.JAVA_VERSION) > 0;
		// on récupèrait les threads sans stack trace en contournant bug 6434648 avant 1.6.0_01
		// http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=6434648
		// hormis pour le thread courant qui obtient sa stack trace différemment sans le bug
		//		threads = getThreadsFromThreadGroups();
		//		final Thread currentThread = Thread.currentThread();
		//		stackTraces = Collections.singletonMap(currentThread, currentThread.getStackTrace());

		final boolean cpuTimeEnabled = threadBean.isThreadCpuTimeSupported()
				&& threadBean.isThreadCpuTimeEnabled();
		final long[] deadlockedThreads = getDeadlockedThreads(threadBean);
		final List<ThreadInformations> threadInfosList = new ArrayList<ThreadInformations>(
				threads.size());
		// hostAddress récupéré ici car il peut y avoir plus de 20000 threads
		final String hostAddress = Parameters.getHostAddress();
		for (final Thread thread : threads) {
			final StackTraceElement[] stackTraceElements = stackTraces.get(thread);
			final List<StackTraceElement> stackTraceElementList = stackTraceElements == null ? null
					: new ArrayList<StackTraceElement>(Arrays.asList(stackTraceElements));
			final long cpuTimeMillis;
			final long userTimeMillis;
			if (cpuTimeEnabled) {
				cpuTimeMillis = threadBean.getThreadCpuTime(thread.getId()) / 1000000;
				userTimeMillis = threadBean.getThreadUserTime(thread.getId()) / 1000000;
			} else {
				cpuTimeMillis = -1;
				userTimeMillis = -1;
			}
			final boolean deadlocked = deadlockedThreads != null
					&& Arrays.binarySearch(deadlockedThreads, thread.getId()) >= 0;
			// stackTraceElementList est une ArrayList et non unmodifiableList pour lisibilité xml
			threadInfosList.add(new ThreadInformations(thread, stackTraceElementList,
					cpuTimeMillis, userTimeMillis, deadlocked, hostAddress));
		}
		// on retourne ArrayList et non unmodifiableList pour lisibilité du xml par xstream
		return threadInfosList;
	}

	static List<Thread> getThreadsFromThreadGroups() {
		ThreadGroup group = Thread.currentThread().getThreadGroup(); // NOPMD
		while (group.getParent() != null) {
			group = group.getParent();
		}
		final Thread[] threadsArray = new Thread[group.activeCount()];
		group.enumerate(threadsArray, true);
		return Arrays.asList(threadsArray);
	}

	private static long[] getDeadlockedThreads(ThreadMXBean threadBean) {
		final long[] deadlockedThreads;
		if (threadBean.isSynchronizerUsageSupported()) {
			deadlockedThreads = threadBean.findDeadlockedThreads();
		} else {
			deadlockedThreads = threadBean.findMonitorDeadlockedThreads();
		}
		if (deadlockedThreads != null) {
			Arrays.sort(deadlockedThreads);
		}
		return deadlockedThreads;
	}

	private static String buildDataBaseVersion() {
		if (Parameters.isNoDatabase()) {
			return null;
		}
		final StringBuilder result = new StringBuilder();
		try {
			// on commence par voir si le driver jdbc a été utilisé
			// car s'il n'y a pas de datasource une exception est déclenchée
			if (Parameters.getLastConnectUrl() != null) {
				final Connection connection = DriverManager.getConnection(
						Parameters.getLastConnectUrl(), Parameters.getLastConnectInfo());
				connection.setAutoCommit(false);
				try {
					appendDataBaseVersion(result, connection);
				} finally {
					// rollback inutile ici car on ne fait que lire les meta-data (+ cf issue 38)
					connection.close();
				}
				return result.toString();
			}

			// on cherche une datasource avec InitialContext pour afficher nom et version bdd + nom et version driver jdbc
			// (le nom de la dataSource recherchée dans JNDI est du genre jdbc/Xxx qui est le nom standard d'une DataSource)
			final Map<String, DataSource> dataSources = JdbcWrapper.getJndiAndSpringDataSources();
			for (final Map.Entry<String, DataSource> entry : dataSources.entrySet()) {
				final String name = entry.getKey();
				final DataSource dataSource = entry.getValue();
				final Connection connection = dataSource.getConnection();
				// on ne doit pas changer autoCommit pour la connection d'une DataSource
				// (ou alors il faudrait remettre l'autoCommit après, issue 233)
				// connection.setAutoCommit(false);
				try {
					if (result.length() > 0) {
						result.append("\n\n");
					}
					result.append(name).append(":\n");
					appendDataBaseVersion(result, connection);
				} finally {
					// rollback inutile ici car on ne fait que lire les meta-data (+ cf issue 38)
					connection.close();
				}
			}
		} catch (final Exception e) {
			result.append(e.toString());
		}
		if (result.length() > 0) {
			return result.toString();
		}
		return null;
	}

	private static void appendDataBaseVersion(StringBuilder result, Connection connection)
			throws SQLException {
		final DatabaseMetaData metaData = connection.getMetaData();
		// Sécurité: pour l'instant on n'indique pas metaData.getUserName()
		result.append(metaData.getURL()).append('\n');
		result.append(metaData.getDatabaseProductName()).append(", ")
				.append(metaData.getDatabaseProductVersion()).append('\n');
		result.append("Driver JDBC:\n").append(metaData.getDriverName()).append(", ")
				.append(metaData.getDriverVersion());
	}

	private static String buildDataSourceDetails() {
		final Map<String, Map<String, Object>> dataSourcesProperties = JdbcWrapper
				.getBasicDataSourceProperties();
		final StringBuilder sb = new StringBuilder();
		for (final Map.Entry<String, Map<String, Object>> entry : dataSourcesProperties.entrySet()) {
			final Map<String, Object> dataSourceProperties = entry.getValue();
			if (dataSourceProperties.isEmpty()) {
				continue;
			}
			if (sb.length() > 0) {
				sb.append('\n');
			}
			final String name = entry.getKey();
			if (name != null) {
				sb.append(name).append(":\n");
			}
			for (final Map.Entry<String, Object> propertyEntry : dataSourceProperties.entrySet()) {
				sb.append(propertyEntry.getKey()).append(" = ").append(propertyEntry.getValue())
						.append('\n');
			}
		}
		if (sb.length() == 0) {
			return null;
		}
		return sb.toString();
	}

	private static List<String> buildDependenciesList(ServletContext servletContext) {
		final String directory = "/WEB-INF/lib/";

		final Set<String> dependencies;
		try {
			dependencies = servletContext.getResourcePaths(directory);
		} catch (final Exception e) {
			// Tomcat 8 can throw "IllegalStateException: The resources may not be accessed if they are not currently started"
			// for some ServletContext states (issue 415)
			return Collections.emptyList();
		}
		if (dependencies == null || dependencies.isEmpty()) {
			return Collections.emptyList();
		}
		final List<String> result = new ArrayList<String>(dependencies.size());
		for (final String dependency : dependencies) {
			result.add(dependency.substring(directory.length()));
		}
		Collections.sort(result);
		return result;
	}

	private static boolean isSunOsMBean(OperatingSystemMXBean operatingSystem) {
		// on ne teste pas operatingSystem instanceof com.sun.management.OperatingSystemMXBean
		// car le package com.sun n'existe à priori pas sur une jvm tierce
		final String className = operatingSystem.getClass().getName();
		return "com.sun.management.OperatingSystem".equals(className)
				|| "com.sun.management.UnixOperatingSystem".equals(className)
				// sun.management.OperatingSystemImpl pour java 8
				|| "sun.management.OperatingSystemImpl".equals(className);
	}

	private static boolean isSunUnixMBean(OperatingSystemMXBean operatingSystem) {
		for (final Class<?> inter : operatingSystem.getClass().getInterfaces()) {
			if ("com.sun.management.UnixOperatingSystemMXBean".equals(inter.getName())) {
				return true;
			}
		}
		return false;
	}

	MemoryInformations getMemoryInformations() {
		return memoryInformations;
	}

	List<TomcatInformations> getTomcatInformationsList() {
		return tomcatInformationsList;
	}

	int getSessionCount() {
		return sessionCount;
	}

	long getSessionAgeSum() {
		return sessionAgeSum;
	}

	long getSessionMeanAgeInMinutes() {
		if (sessionCount > 0) {
			return sessionAgeSum / sessionCount / 60000;
		}
		return -1;
	}

	int getActiveThreadCount() {
		return activeThreadCount;
	}

	int getUsedConnectionCount() {
		return usedConnectionCount;
	}

	int getActiveConnectionCount() {
		return activeConnectionCount;
	}

	int getMaxConnectionCount() {
		return maxConnectionCount;
	}

	long getTransactionCount() {
		return transactionCount;
	}

	double getUsedConnectionPercentage() {
		if (maxConnectionCount > 0) {
			return 100d * usedConnectionCount / maxConnectionCount;
		}
		return -1d;
	}

	long getProcessCpuTimeMillis() {
		return processCpuTimeMillis;
	}

	double getSystemLoadAverage() {
		return systemLoadAverage;
	}

	long getUnixOpenFileDescriptorCount() {
		return unixOpenFileDescriptorCount;
	}

	long getUnixMaxFileDescriptorCount() {
		return unixMaxFileDescriptorCount;
	}

	double getUnixOpenFileDescriptorPercentage() {
		if (unixOpenFileDescriptorCount >= 0) {
			return 100d * unixOpenFileDescriptorCount / unixMaxFileDescriptorCount;
		}
		return -1d;
	}

	String getHost() {
		return host;
	}

	String getOS() {
		return os;
	}

	int getAvailableProcessors() {
		return availableProcessors;
	}

	String getJavaVersion() {
		return javaVersion;
	}

	String getJvmVersion() {
		return jvmVersion;
	}

	String getPID() {
		return pid;
	}

	String getServerInfo() {
		return serverInfo;
	}

	String getContextPath() {
		return contextPath;
	}

	String getContextDisplayName() {
		return contextDisplayName;
	}

	Date getStartDate() {
		return startDate;
	}

	String getJvmArguments() {
		return jvmArguments;
	}

	long getFreeDiskSpaceInTemp() {
		return freeDiskSpaceInTemp;
	}

	int getThreadCount() {
		return threadCount;
	}

	int getPeakThreadCount() {
		return peakThreadCount;
	}

	long getTotalStartedThreadCount() {
		return totalStartedThreadCount;
	}

	String getDataBaseVersion() {
		return dataBaseVersion;
	}

	String getDataSourceDetails() {
		return dataSourceDetails;
	}

	List<ThreadInformations> getThreadInformationsList() {
		// on trie sur demande (si affichage)
		final List<ThreadInformations> result = new ArrayList<ThreadInformations>(
				threadInformationsList);
		Collections.sort(result, new ThreadInformationsComparator());
		return Collections.unmodifiableList(result);
	}

	List<CacheInformations> getCacheInformationsList() {
		// on trie sur demande (si affichage)
		final List<CacheInformations> result = new ArrayList<CacheInformations>(
				cacheInformationsList);
		Collections.sort(result, new CacheInformationsComparator());
		return Collections.unmodifiableList(result);
	}

	List<JobInformations> getJobInformationsList() {
		// on trie sur demande (si affichage)
		final List<JobInformations> result = new ArrayList<JobInformations>(jobInformationsList);
		Collections.sort(result, new JobInformationsComparator());
		return Collections.unmodifiableList(result);
	}

	int getCurrentlyExecutingJobCount() {
		int result = 0;
		for (final JobInformations jobInformations : jobInformationsList) {
			if (jobInformations.isCurrentlyExecuting()) {
				result++;
			}
		}
		return result;
	}

	boolean isDependenciesEnabled() {
		return dependenciesList != null && !dependenciesList.isEmpty();
	}

	List<String> getDependenciesList() {
		if (dependenciesList != null) {
			return Collections.unmodifiableList(dependenciesList);
		}
		return Collections.emptyList();
	}

	String getDependencies() {
		if (!isDependenciesEnabled()) {
			return null;
		}
		final StringBuilder sb = new StringBuilder();
		for (final String dependency : getDependenciesList()) {
			if (dependency.endsWith(".jar") || dependency.endsWith(".JAR")) {
				sb.append(dependency);
				sb.append(",\n");
			}
		}
		if (sb.length() >= 2) {
			sb.delete(sb.length() - 2, sb.length());
		}
		return sb.toString();
	}

	boolean isStackTraceEnabled() {
		for (final ThreadInformations threadInformations : threadInformationsList) {
			final List<StackTraceElement> stackTrace = threadInformations.getStackTrace();
			if (stackTrace != null && !stackTrace.isEmpty()) {
				return true;
			}
		}
		return false;
	}

	boolean isCacheEnabled() {
		return cacheInformationsList != null && !cacheInformationsList.isEmpty();
	}

	boolean isJobEnabled() {
		return jobInformationsList != null && !jobInformationsList.isEmpty();
	}

	/** {@inheritDoc} */
	@Override
	public String toString() {
		return getClass().getSimpleName() + "[pid=" + getPID() + ", host=" + getHost()
				+ ", javaVersion=" + getJavaVersion() + ", serverInfo=" + getServerInfo() + ']';
	}
}
