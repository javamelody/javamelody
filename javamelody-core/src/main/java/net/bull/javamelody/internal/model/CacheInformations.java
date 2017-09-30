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
import java.io.InputStream;
import java.io.Serializable;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Properties;

import net.bull.javamelody.internal.common.LOG;
import net.sf.ehcache.CacheManager;
import net.sf.ehcache.Ehcache;
import net.sf.ehcache.Statistics;
import net.sf.ehcache.config.CacheConfiguration;
import net.sf.ehcache.management.CacheStatistics;

/**
 * Informations sur un cache de données.
 * L'état d'une instance est initialisé à son instanciation et non mutable;
 * il est donc de fait thread-safe.
 * Cet état est celui d'un cache à un instant t.
 * Les instances sont sérialisables pour pouvoir être transmises au serveur de collecte.
 * Pour l'instant seul ehcache est géré.
 * @author Emeric Vernat
 */
public class CacheInformations implements Serializable {
	private static final long serialVersionUID = -3025833425994923286L;
	private static final boolean EHCACHE_AVAILABLE = isEhcacheAvailable();
	private static final boolean EHCACHE_2_7 = isEhcache27();
	private static final boolean EHCACHE_1_6 = isEhcache16();
	private static final boolean EHCACHE_1_2 = isEhcache12();
	private static final boolean EHCACHE_1_2_X = isEhcache12x();

	private final String name;
	private final long inMemoryObjectCount;
	private final int inMemoryPercentUsed;
	private final long onDiskObjectCount;
	private final long inMemoryHits;
	private final long cacheHits;
	private final long cacheMisses;
	private final String configuration;
	private final List<?> cacheKeys;

	CacheInformations(Ehcache cache, boolean includeKeys) {
		super();
		assert cache != null;
		this.name = cache.getName();

		if (includeKeys) {
			this.cacheKeys = cache.getKeys();
		} else {
			this.cacheKeys = null;
		}

		if (EHCACHE_2_7) {
			// Depuis ehcache 2.7.0, cache.getStatistics() retourne "StatisticsGateway" qui est nouvelle et plus "Statistics".
			// CacheStatistics existe depuis ehcache 1.3.
			final CacheStatistics statistics = new CacheStatistics(cache);
			this.inMemoryObjectCount = statistics.getMemoryStoreObjectCount(); // ou cache.getStatistics().getLocalHeapSize() en v2.7.0
			this.onDiskObjectCount = statistics.getDiskStoreObjectCount(); // ou cache.getStatistics().getLocalDiskSize() en v2.7.0
			this.inMemoryHits = statistics.getInMemoryHits(); // ou cache.getStatistics().localHeapHitCount() en v2.7.0
			this.cacheHits = statistics.getCacheHits(); // ou cache.getStatistics().cacheHitCount() en v2.7.0
			this.cacheMisses = statistics.getCacheMisses(); // ou devrait être cache.getStatistics().cacheMissCount() en v2.7.0
			// en raison du bug https://jira.terracotta.org/jira/browse/EHC-1010
			// la valeur de l'efficacité du cache (hits/accesses) est fausse si ehcache 2.7.0
			this.inMemoryPercentUsed = computeMemoryPercentUsed(cache);
			this.configuration = buildConfiguration(cache);
			return;
		}

		final Statistics statistics = cache.getStatistics();
		assert statistics != null;
		if (EHCACHE_1_6) {
			// n'existent que depuis ehcache 1.6
			this.inMemoryObjectCount = statistics.getMemoryStoreObjectCount();
			this.onDiskObjectCount = statistics.getDiskStoreObjectCount();
			// NB: en ehcache 1.2, la valeur de STATISTICS_ACCURACY_BEST_EFFORT n'était pas la même
			assert statistics.getStatisticsAccuracy() == Statistics.STATISTICS_ACCURACY_BEST_EFFORT;
		} else {
			this.inMemoryObjectCount = cache.getMemoryStoreSize();
			this.onDiskObjectCount = cache.getDiskStoreSize();
		}
		// la taille du cache en mémoire par cache.calculateInMemorySize() est trop lente
		// pour être déterminée à chaque fois (1s pour 1Mo selon javadoc d'ehcache)
		if (EHCACHE_1_2_X) {
			// getInMemoryHits, getCacheHits et getCacheMisses n'existent pas en echache v1.2
			// mais existent en v1.2.1 et v1.2.3 (présent dans hibernate v?) mais avec int comme résultat
			// et existent depuis v1.2.4 mais avec long comme résultat
			this.inMemoryHits = invokeStatisticsMethod(statistics, "getInMemoryHits");
			this.cacheHits = invokeStatisticsMethod(statistics, "getCacheHits");
			this.cacheMisses = invokeStatisticsMethod(statistics, "getCacheMisses");
			// getCacheConfiguration et getMaxElementsOnDisk() n'existent pas en ehcache 1.2
			this.inMemoryPercentUsed = -1;
			this.configuration = null;
		} else if (EHCACHE_1_2) {
			this.inMemoryHits = -1;
			this.cacheHits = -1;
			this.cacheMisses = -1;
			this.inMemoryPercentUsed = -1;
			this.configuration = null;
		} else {
			this.inMemoryHits = statistics.getInMemoryHits();
			this.cacheHits = statistics.getCacheHits();
			this.cacheMisses = statistics.getCacheMisses();
			this.inMemoryPercentUsed = computeMemoryPercentUsed(cache);
			this.configuration = buildConfiguration(cache);
		}
	}

	// on ne doit pas référencer la classe Statistics dans les déclarations de méthodes (issue 335)
	private static long invokeStatisticsMethod(Object statistics, String methodName) {
		try {
			// getInMemoryHits, getCacheHits et getCacheMisses existent en v1.2.1 et v1.2.3
			// mais avec int comme résultat et existent depuis v1.2.4 avec long comme résultat
			// donc on cast en Number et non en Integer ou en Long
			final Number result = (Number) Statistics.class.getMethod(methodName, (Class<?>[]) null)
					.invoke(statistics, (Object[]) null);
			return result.longValue();
		} catch (final NoSuchMethodException e) {
			throw new IllegalArgumentException(e);
		} catch (final InvocationTargetException e) {
			throw new IllegalStateException(e.getCause());
		} catch (final IllegalAccessException e) {
			throw new IllegalStateException(e);
		}
	}

	private static boolean isEhcacheAvailable() {
		try {
			Class.forName("net.sf.ehcache.Cache");
			return true;
		} catch (final ClassNotFoundException e) {
			return false;
		} catch (final NoClassDefFoundError e) {
			// cf issue 67
			return false;
		}
	}

	static List<CacheInformations> buildCacheInformationsList() {
		if (!EHCACHE_AVAILABLE) {
			return Collections.emptyList();
		}
		final List<CacheManager> allCacheManagers;
		try {
			allCacheManagers = new ArrayList<CacheManager>(CacheManager.ALL_CACHE_MANAGERS);
		} catch (final NoSuchFieldError e) {
			// nécessaire pour ehcache 1.2 ou avant
			return Collections.emptyList();
		}
		final List<CacheInformations> result = new ArrayList<CacheInformations>();
		for (final CacheManager cacheManager : allCacheManagers) {
			final String[] cacheNames = cacheManager.getCacheNames();
			try {
				for (final String cacheName : cacheNames) {
					result.add(new CacheInformations(cacheManager.getEhcache(cacheName), false));
				}
			} catch (final Exception e) {
				// Avoid Exception throwing in cache information parsing
				// (for example with JGroups or TransactionException: transaction not started, issue 402)
				// and do not log an exception for each cache
				LOG.debug(e.toString(), e);
			}
		}
		return result;
	}

	public static CacheInformations buildCacheInformationsWithKeys(String cacheId) {
		assert EHCACHE_AVAILABLE;
		assert cacheId != null;
		final List<CacheManager> allCacheManagers = new ArrayList<CacheManager>(
				CacheManager.ALL_CACHE_MANAGERS);
		for (final CacheManager cacheManager : allCacheManagers) {
			final Ehcache ehcache = cacheManager.getEhcache(cacheId);
			if (ehcache != null) {
				return new CacheInformations(ehcache, true);
			}
		}
		return null;
	}

	private static boolean isEhcache27() {
		try {
			final InputStream input = Class.forName("net.sf.ehcache.Ehcache")
					.getResourceAsStream("/net/sf/ehcache/version.properties");
			if (input != null) {
				try {
					try {
						final Properties properties = new Properties();
						properties.load(input);
						final String version = properties.getProperty("version");
						return "2.7".compareTo(version) <= 0 || "2.10".compareTo(version) <= 0;
					} finally {
						input.close();
					}
				} catch (final IOException e) { // NOPMD
					// continue
				}
			}
			// ce Class.forName est nécessaire sur le serveur de collecte
			Class.forName("net.sf.ehcache.statistics.StatisticsGateway");
			return true;
		} catch (final ClassNotFoundException e) {
			return false;
		}
	}

	private static boolean isEhcache16() {
		try {
			// ce Class.forName est nécessaire sur le serveur de collecte
			Class.forName("net.sf.ehcache.Statistics");
			// getMemoryStoreObjectCount n'existe que depuis ehcache 1.6
			Statistics.class.getMethod("getMemoryStoreObjectCount");
			return true;
		} catch (final ClassNotFoundException e) {
			return false;
		} catch (final NoSuchMethodException e) {
			return false;
		}
	}

	private static boolean isEhcache12() {
		try {
			// ce Class.forName est nécessaire sur le serveur de collecte
			Class.forName("net.sf.ehcache.Ehcache");
			// getCacheConfiguration n'existe pas en ehcache 1.2
			Ehcache.class.getMethod("getCacheConfiguration");
			return false;
		} catch (final ClassNotFoundException e) {
			return false;
		} catch (final NoClassDefFoundError e) {
			// cf issue 67
			return false;
		} catch (final NoSuchMethodException e) {
			return true;
		}
	}

	private static boolean isEhcache12x() {
		try {
			// Statistics existe à partir d'ehcache 1.2.1
			Class.forName("net.sf.ehcache.Statistics");
			return isEhcache12();
		} catch (final ClassNotFoundException e) {
			return false;
		}
	}

	// cache must not be typed,
	// otherwise serialization would not work in the collector server or in jenkins scripts
	private int computeMemoryPercentUsed(Object cache) {
		final int maxElementsInMemory = ((Ehcache) cache).getCacheConfiguration()
				.getMaxElementsInMemory();
		if (maxElementsInMemory == 0) {
			// maxElementsInMemory peut être 0 (sans limite), cf issue 73
			return -1;
		}
		return (int) (100 * inMemoryObjectCount / maxElementsInMemory);
	}

	// cache must not be typed,
	// otherwise serialization would not work in the collector server or in jenkins scripts
	private String buildConfiguration(Object cache) {
		final StringBuilder sb = new StringBuilder();
		// getCacheConfiguration() et getMaxElementsOnDisk() n'existent pas en ehcache 1.2
		final CacheConfiguration config = ((Ehcache) cache).getCacheConfiguration();
		sb.append("ehcache [maxElementsInMemory = ").append(config.getMaxElementsInMemory());
		final boolean overflowToDisk = config.isOverflowToDisk();
		sb.append(", overflowToDisk = ").append(overflowToDisk);
		if (overflowToDisk) {
			sb.append(", maxElementsOnDisk = ").append(config.getMaxElementsOnDisk());
		}
		final boolean eternal = config.isEternal();
		sb.append(", eternal = ").append(eternal);
		if (!eternal) {
			sb.append(", timeToLiveSeconds = ").append(config.getTimeToLiveSeconds());
			sb.append(", timeToIdleSeconds = ").append(config.getTimeToIdleSeconds());
			sb.append(", memoryStoreEvictionPolicy = ")
					.append(config.getMemoryStoreEvictionPolicy());
		}
		sb.append(", diskPersistent = ").append(config.isDiskPersistent());
		sb.append(']');
		return sb.toString();
	}

	public String getName() {
		return name;
	}

	public long getInMemoryObjectCount() {
		return inMemoryObjectCount;
	}

	public long getInMemoryPercentUsed() {
		return inMemoryPercentUsed;
	}

	public long getOnDiskObjectCount() {
		return onDiskObjectCount;
	}

	public long getInMemoryHits() {
		return inMemoryHits;
	}

	public long getCacheHits() {
		return cacheHits;
	}

	public long getCacheMisses() {
		return cacheMisses;
	}

	// efficacité en pourcentage du cache mémoire par rapport au cache disque
	public int getInMemoryHitsRatio() {
		if (cacheHits == 0) {
			return -1;
		}
		return (int) (100 * inMemoryHits / cacheHits);
	}

	// efficacité en pourcentage du cache (mémoire + disque) par rapport au total des accès
	public int getHitsRatio() {
		final long accessCount = cacheHits + cacheMisses;
		if (accessCount == 0) {
			return -1;
		}
		return (int) (100 * cacheHits / accessCount);
	}

	public String getConfiguration() {
		return configuration;
	}

	public List<?> getCacheKeys() {
		return cacheKeys;
	}

	/** {@inheritDoc} */
	@Override
	public String toString() {
		return getClass().getSimpleName() + "[name=" + getName() + ", inMemoryObjectCount="
				+ getInMemoryObjectCount() + ", inMemoryPercentUsed=" + getInMemoryPercentUsed()
				+ ", onDiskObjectCount=" + getOnDiskObjectCount() + ", inMemoryHitsRatio="
				+ getInMemoryHitsRatio() + ", hitsRatio=" + getHitsRatio() + ']';
	}
}
