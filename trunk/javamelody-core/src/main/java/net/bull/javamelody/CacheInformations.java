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

import java.io.Serializable;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import net.sf.ehcache.CacheManager;
import net.sf.ehcache.Ehcache;
import net.sf.ehcache.Statistics;
import net.sf.ehcache.config.CacheConfiguration;

/**
 * Informations sur un cache de données.
 * L'état d'une instance est initialisé à son instanciation et non mutable;
 * il est donc de fait thread-safe.
 * Cet état est celui d'un cache à un instant t.
 * Les instances sont sérialisables pour pouvoir être transmises au serveur de collecte.
 * Pour l'instant seul ehcache est géré.
 * @author Emeric Vernat
 */
class CacheInformations implements Serializable {
	private static final long serialVersionUID = -3025833425994923286L;
	private static final boolean EHCACHE_AVAILABLE = isEhcacheAvailable();
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

	CacheInformations(Ehcache cache) {
		super();
		assert cache != null;
		final Statistics statistics = cache.getStatistics();
		assert statistics != null;
		this.name = cache.getName();
		long tmpInMemoryObjectCount;
		long tmpOnDiskObjectCount;
		if (EHCACHE_1_6) {
			// n'existent que depuis ehcache 1.6
			tmpInMemoryObjectCount = statistics.getMemoryStoreObjectCount();
			tmpOnDiskObjectCount = statistics.getDiskStoreObjectCount();
			// NB: en ehcache 1.2, la valeur de STATISTICS_ACCURACY_BEST_EFFORT n'était pas la même
			assert statistics.getStatisticsAccuracy() == Statistics.STATISTICS_ACCURACY_BEST_EFFORT;
		} else {
			tmpInMemoryObjectCount = cache.getMemoryStoreSize();
			tmpOnDiskObjectCount = cache.getDiskStoreSize();
		}
		this.inMemoryObjectCount = tmpInMemoryObjectCount;
		this.onDiskObjectCount = tmpOnDiskObjectCount;
		// la taille du cache en mémoire par cache.calculateInMemorySize() est trop lente
		// pour être déterminée à chaque fois (1s pour 1Mo selon javadoc d'ehcache)
		long tmpInMemoryHits;
		long tmpCacheHits;
		long tmpCacheMisses;
		int tmpInMemoryPercentUsed;
		String tmpConfiguration;
		if (EHCACHE_1_2_X) {
			// getInMemoryHits, getCacheHits et getCacheMisses n'existent pas en echache v1.2
			// mais existent en v1.2.1 et v1.2.3 (présent dans hibernate v?) mais avec int comme résultat
			// et existent depuis v1.2.4 mais avec long comme résultat
			tmpInMemoryHits = invokeStatisticsMethod(statistics, "getInMemoryHits");
			tmpCacheHits = invokeStatisticsMethod(statistics, "getCacheHits");
			tmpCacheMisses = invokeStatisticsMethod(statistics, "getCacheMisses");
			// getCacheConfiguration et getMaxElementsOnDisk() n'existent pas en ehcache 1.2
			tmpInMemoryPercentUsed = -1;
			tmpConfiguration = null;
		} else if (EHCACHE_1_2) {
			tmpInMemoryHits = -1;
			tmpCacheHits = -1;
			tmpCacheMisses = -1;
			tmpInMemoryPercentUsed = -1;
			tmpConfiguration = null;
		} else {
			tmpInMemoryHits = statistics.getInMemoryHits();
			tmpCacheHits = statistics.getCacheHits();
			tmpCacheMisses = statistics.getCacheMisses();
			final int maxElementsInMemory = cache.getCacheConfiguration().getMaxElementsInMemory();
			if (maxElementsInMemory == 0) {
				// maxElementsInMemory peut être 0 (sans limite), cf issue 73
				tmpInMemoryPercentUsed = -1;
			} else {
				tmpInMemoryPercentUsed = (int) (100 * inMemoryObjectCount / maxElementsInMemory);
			}
			tmpConfiguration = buildConfiguration(cache);
		}
		this.inMemoryHits = tmpInMemoryHits;
		this.cacheHits = tmpCacheHits;
		this.cacheMisses = tmpCacheMisses;
		this.inMemoryPercentUsed = tmpInMemoryPercentUsed;
		this.configuration = tmpConfiguration;
	}

	private static long invokeStatisticsMethod(Statistics statistics, String methodName) {
		try {
			// getInMemoryHits, getCacheHits et getCacheMisses existent en v1.2.1 et v1.2.3
			// mais avec int comme résultat et existent depuis v1.2.4 avec long comme résultat
			// donc on cast en Number et non en Integer ou en Long
			final Number result = (Number) Statistics.class
					.getMethod(methodName, (Class<?>[]) null).invoke(statistics, (Object[]) null);
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
		}
	}

	@SuppressWarnings("unchecked")
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
			for (final String cacheName : cacheNames) {
				try {
					result.add(new CacheInformations(cacheManager.getEhcache(cacheName)));
				} catch (final Exception e) {
					// Avoid Exception throwing in cache information parsing (for example with JGroups).
					LOG.debug(e.toString(), e);
				}
			}
		}
		return result;
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

	private static String buildConfiguration(Ehcache cache) {
		final StringBuilder sb = new StringBuilder();
		// getCacheConfiguration() et getMaxElementsOnDisk() n'existent pas en ehcache 1.2
		final CacheConfiguration configuration = cache.getCacheConfiguration();
		sb.append("ehcache [maxElementsInMemory = ").append(configuration.getMaxElementsInMemory());
		final boolean overflowToDisk = configuration.isOverflowToDisk();
		sb.append(", overflowToDisk = ").append(overflowToDisk);
		if (overflowToDisk) {
			sb.append(", maxElementsOnDisk = ").append(configuration.getMaxElementsOnDisk());
		}
		final boolean eternal = configuration.isEternal();
		sb.append(", eternal = ").append(eternal);
		if (!eternal) {
			sb.append(", timeToLiveSeconds = ").append(configuration.getTimeToLiveSeconds());
			sb.append(", timeToIdleSeconds = ").append(configuration.getTimeToIdleSeconds());
			sb.append(", memoryStoreEvictionPolicy = ").append(
					configuration.getMemoryStoreEvictionPolicy());
		}
		sb.append(", diskPersistent = ").append(configuration.isDiskPersistent());
		sb.append(']');
		return sb.toString();
	}

	String getName() {
		return name;
	}

	long getInMemoryObjectCount() {
		return inMemoryObjectCount;
	}

	long getInMemoryPercentUsed() {
		return inMemoryPercentUsed;
	}

	long getOnDiskObjectCount() {
		return onDiskObjectCount;
	}

	// efficacité en pourcentage du cache mémoire par rapport au cache disque
	int getInMemoryHitsRatio() {
		if (cacheHits == 0) {
			return -1;
		}
		return (int) (100 * inMemoryHits / cacheHits);
	}

	// efficacité en pourcentage du cache (mémoire + disque) par rapport au total des accès
	int getHitsRatio() {
		final long accessCount = cacheHits + cacheMisses;
		if (accessCount == 0) {
			return -1;
		}
		return (int) (100 * cacheHits / accessCount);
	}

	String getConfiguration() {
		return configuration;
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
