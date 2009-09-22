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
import java.util.Map;
import java.util.Set;

import javax.naming.NamingException;
import javax.servlet.ServletContext;
import javax.sql.DataSource;

import net.sf.ehcache.CacheManager;

/**
 * Informations systèmes sur le serveur, sans code html de présentation.
 * L'état d'une instance est initialisé à son instanciation et non mutable;
 * il est donc de fait thread-safe.
 * Cet état est celui d'une instance de JVM java, de ses threads et du système à un instant t.
 * Les instances sont sérialisables pour pouvoir être transmises au serveur de collecte.
 * @author Emeric Vernat
 */
class JavaInformations implements Serializable { // NOPMD
	// les stack traces des threads ne sont récupérées qu'à partir de java 1.6.0 update 1
	// pour éviter la fuite mémoire du bug http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=6434648
	private static final boolean STACK_TRACES_ENABLED = "1.6.0_01"
			.compareTo(Parameters.JAVA_VERSION) <= 0;
	private static final boolean SYSTEM_LOAD_AVERAGE_ENABLED = "1.6"
			.compareTo(Parameters.JAVA_VERSION) < 0;
	private static final boolean FREE_DISK_SPACE_ENABLED = "1.6".compareTo(Parameters.JAVA_VERSION) < 0;
	private static final long serialVersionUID = 3281861236369720876L;
	private static final Date START_DATE = new Date();
	private static final boolean EHCACHE_AVAILABLE = isEhcacheAvailable();
	private static boolean localWebXmlExists = true; // true par défaut
	private static boolean localPomXmlExists = true; // true par défaut
	private final MemoryInformations memoryInformations;
	private final int sessionCount;
	private final int activeThreadCount;
	private final int usedConnectionCount;
	private final int maxConnectionCount;
	private final int activeConnectionCount;
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
	private final String tomcatDataSourceDetails;
	@SuppressWarnings("all")
	private final List<ThreadInformations> threadInformationsList;
	@SuppressWarnings("all")
	private final List<CacheInformations> cacheInformationsList;
	@SuppressWarnings("all")
	private final List<String> dependenciesList;
	private final boolean webXmlExists = localWebXmlExists;
	private final boolean pomXmlExists = localPomXmlExists;

	static final class ThreadInformationsComparator implements Comparator<ThreadInformations>,
			Serializable {
		private static final long serialVersionUID = 1L;

		/** {@inheritDoc} */
		public int compare(ThreadInformations thread1, ThreadInformations thread2) {
			return thread1.getName().compareToIgnoreCase(thread2.getName());
		}
	}

	static final class CacheInformationsComparator implements Comparator<CacheInformations>,
			Serializable {
		private static final long serialVersionUID = 1L;

		/** {@inheritDoc} */
		public int compare(CacheInformations cache1, CacheInformations cache2) {
			return cache1.getName().compareToIgnoreCase(cache2.getName());
		}
	}

	// CHECKSTYLE:OFF
	JavaInformations(ServletContext servletContext, boolean includeDetails) {
		// CHECKSTYLE:ON
		super();
		memoryInformations = new MemoryInformations();
		sessionCount = SessionListener.getSessionCount();
		activeThreadCount = JdbcWrapper.getActiveThreadCount();
		usedConnectionCount = JdbcWrapper.getUsedConnectionCount();
		activeConnectionCount = JdbcWrapper.getActiveConnectionCount();
		maxConnectionCount = JdbcWrapper.getMaxConnectionCount();
		systemLoadAverage = buildSystemLoadAverage();
		processCpuTimeMillis = buildProcessCpuTimeMillis();
		final OperatingSystemMXBean operatingSystem = ManagementFactory.getOperatingSystemMXBean();
		if (isSunOsMBean(operatingSystem)
				&& "com.sun.management.UnixOperatingSystem".equals(operatingSystem.getClass()
						.getName())) {
			final com.sun.management.UnixOperatingSystemMXBean unixOsBean = (com.sun.management.UnixOperatingSystemMXBean) operatingSystem;
			unixOpenFileDescriptorCount = unixOsBean.getOpenFileDescriptorCount();
			unixMaxFileDescriptorCount = unixOsBean.getMaxFileDescriptorCount();
		} else {
			unixOpenFileDescriptorCount = -1;
			unixMaxFileDescriptorCount = -1;
		}
		host = Parameters.getHostName() + '@' + Parameters.getHostAddress();
		os = System.getProperty("os.name") + ' ' + System.getProperty("sun.os.patch.level") + ", "
				+ System.getProperty("os.arch") + '/' + System.getProperty("sun.arch.data.model");
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
			dependenciesList = buildDependenciesList();
		}
		startDate = START_DATE;
		jvmArguments = buildJvmArguments();
		final ThreadMXBean threadBean = ManagementFactory.getThreadMXBean();
		threadCount = threadBean.getThreadCount();
		peakThreadCount = threadBean.getPeakThreadCount();
		totalStartedThreadCount = threadBean.getTotalStartedThreadCount();
		if (FREE_DISK_SPACE_ENABLED) {
			freeDiskSpaceInTemp = Parameters.TEMPORARY_DIRECTORY.getFreeSpace();
		} else {
			freeDiskSpaceInTemp = -1;
		}

		if (includeDetails) {
			dataBaseVersion = buildDataBaseVersion();
			tomcatDataSourceDetails = buildTomcatDataSourceDetails();
			threadInformationsList = buildThreadInformationsList();
			cacheInformationsList = buildCacheInformationsList();
			pid = PID.getPID();
		} else {
			dataBaseVersion = null;
			tomcatDataSourceDetails = null;
			threadInformationsList = null;
			cacheInformationsList = null;
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

	private static long buildProcessCpuTimeMillis() {
		final OperatingSystemMXBean operatingSystem = ManagementFactory.getOperatingSystemMXBean();
		if (isSunOsMBean(operatingSystem)) {
			final com.sun.management.OperatingSystemMXBean osBean = (com.sun.management.OperatingSystemMXBean) operatingSystem;
			// nano-secondes converties en milli-secondes
			return osBean.getProcessCpuTime() / 1000000;
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
		if (SYSTEM_LOAD_AVERAGE_ENABLED && operatingSystem.getSystemLoadAverage() >= 0) {
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

	@SuppressWarnings("all")
	static List<ThreadInformations> buildThreadInformationsList() {
		final ThreadMXBean threadBean = ManagementFactory.getThreadMXBean();
		final List<Thread> threads;
		final Map<Thread, StackTraceElement[]> stackTraces;
		if (STACK_TRACES_ENABLED) {
			stackTraces = Thread.getAllStackTraces();
			threads = new ArrayList<Thread>(stackTraces.keySet());
		} else {
			// on récupère les threads sans stack trace en contournant bug 6434648 avant 1.6.0_01
			ThreadGroup group = Thread.currentThread().getThreadGroup();
			while (group.getParent() != null) {
				group = group.getParent();
			}
			final Thread[] threadsArray = new Thread[group.activeCount()];
			group.enumerate(threadsArray, true);

			stackTraces = Collections.emptyMap();
			threads = Arrays.asList(threadsArray);
		}

		final boolean cpuTimeEnabled = threadBean.isThreadCpuTimeSupported()
				&& threadBean.isThreadCpuTimeEnabled();
		final List<ThreadInformations> threadInfosList = new ArrayList<ThreadInformations>(threads
				.size());
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
			// stackTraceElementList est une ArrayList et non unmodifiableList pour lisiblité xml
			threadInfosList.add(new ThreadInformations(thread, stackTraceElementList,
					cpuTimeMillis, userTimeMillis));
		}
		// on retourne ArrayList et non unmodifiableList pour lisibilité du xml par xstream
		return threadInfosList;
	}

	private static boolean isEhcacheAvailable() {
		try {
			Class.forName("net.sf.ehcache.Cache");
			return true;
		} catch (final ClassNotFoundException e) {
			return false;
		}
	}

	private static List<CacheInformations> buildCacheInformationsList() {
		if (!EHCACHE_AVAILABLE) {
			return Collections.emptyList();
		}
		@SuppressWarnings("unchecked")
		final List<CacheManager> allCacheManagers = CacheManager.ALL_CACHE_MANAGERS;
		final List<CacheInformations> result = new ArrayList<CacheInformations>();
		for (final CacheManager cacheManager : allCacheManagers) {
			final String[] cacheNames = cacheManager.getCacheNames();
			for (final String cacheName : cacheNames) {
				result.add(new CacheInformations(cacheManager.getEhcache(cacheName)));
			}
		}
		return result;
	}

	private static String buildDataBaseVersion() {
		final StringBuilder result = new StringBuilder();
		try {
			// on commence par voir si le driver jdbc a été utilisé
			// car s'il n'y a pas de datasource une exception est déclenchée
			final JdbcDriver jdbcDriver = JdbcDriver.SINGLETON;
			if (jdbcDriver.getLastConnectUrl() != null) {
				final Connection connection = DriverManager.getConnection(jdbcDriver
						.getLastConnectUrl(), jdbcDriver.getLastConnectInfo());
				connection.setAutoCommit(false);
				try {
					appendDataBaseVersion(result, connection);
				} finally {
					connection.rollback();
					connection.close();
				}
				return result.toString();
			}

			// on cherche une datasource avec InitialContext pour afficher nom et version bdd + nom et version driver jdbc
			// (le nom de la dataSource recherchée dans JNDI est du genre jdbc/Xxx qui est le nom standard d'une DataSource)
			for (final Map.Entry<String, DataSource> entry : JdbcWrapperHelper.getDataSources()
					.entrySet()) {
				final String jndiName = entry.getKey();
				final DataSource dataSource = entry.getValue();
				final Connection connection = dataSource.getConnection();
				connection.setAutoCommit(false);
				try {
					if (result.length() > 0) {
						result.append('\n');
					}
					result.append(jndiName).append(":\n");
					appendDataBaseVersion(result, connection);
				} finally {
					connection.rollback();
					connection.close();
				}
			}
		} catch (final NamingException e) {
			result.append(e.toString());
		} catch (final ClassNotFoundException e) {
			result.append(e.toString());
		} catch (final SQLException e) {
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
		result.append(metaData.getDatabaseProductName()).append(", ").append(
				metaData.getDatabaseProductVersion()).append('\n');
		result.append("Driver JDBC:\n").append(metaData.getDriverName()).append(", ").append(
				metaData.getDriverVersion());
	}

	private static String buildTomcatDataSourceDetails() {
		final Map<String, Object> dataSourceProperties = JdbcWrapper
				.getTomcatBasicDataSourceProperties();
		if (dataSourceProperties.isEmpty()) {
			return null;
		}
		final StringBuilder sb = new StringBuilder();
		for (final Map.Entry<String, Object> entry : dataSourceProperties.entrySet()) {
			sb.append(entry.getKey()).append(" = ").append(entry.getValue()).append('\n');
		}
		return sb.toString();
	}

	private static List<String> buildDependenciesList() {
		final String directory = "/WEB-INF/lib/";
		@SuppressWarnings("unchecked")
		final Set<String> dependencies = Parameters.getServletContext().getResourcePaths(directory);
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
				|| "com.sun.management.UnixOperatingSystem".equals(className);
	}

	MemoryInformations getMemoryInformations() {
		return memoryInformations;
	}

	int getSessionCount() {
		return sessionCount;
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

	String getTomcatDataSourceDetails() {
		return tomcatDataSourceDetails;
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

	/** {@inheritDoc} */
	@Override
	public String toString() {
		return getClass().getSimpleName() + "[pid=" + getPID() + ", host=" + getHost()
				+ ", javaVersion=" + getJavaVersion() + ", serverInfo=" + getServerInfo() + ']';
	}
}
