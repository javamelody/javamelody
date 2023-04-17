/*
 * Copyright 2008-2019 by Emeric Vernat
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

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import net.bull.javamelody.internal.common.LOG;
import net.sf.ehcache.CacheManager;
import net.sf.ehcache.Ehcache;
import net.sf.ehcache.config.CacheConfiguration;
import net.sf.ehcache.management.CacheStatistics;

/**
 * Informations sur un cache de données.
 * L'état d'une instance est initialisé à son instanciation et non mutable;
 * il est donc de fait thread-safe.
 * Cet état est celui d'un cache à un instant t.
 * Les instances sont sérialisables pour pouvoir être transmises au serveur de collecte.
 * Seul ehcache 2.7+ est géré ici. JCache est géré par {@link JCacheInformations}
 * @author Emeric Vernat
 */
public class CacheInformations implements Serializable {
	private static final long serialVersionUID = -3025833425994923286L;
	private static final boolean EHCACHE_AVAILABLE = isEhcacheAvailable();

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

		// Depuis ehcache 2.7.0, cache.getStatistics() retourne "StatisticsGateway" qui est nouvelle et plus "Statistics".
		// NB : cache.getStatistics() retourne une nouvelle instance à chaque fois et les perfs de cache.getStatistics() dépendent de cache.getStatisticsAccuracy()
		// CacheStatistics existe depuis ehcache 1.3.
		final CacheStatistics statistics = new CacheStatistics(cache);
		this.inMemoryObjectCount = statistics.getMemoryStoreObjectCount(); // ou cache.getStatistics().getLocalHeapSize() en v2.7.0
		this.onDiskObjectCount = statistics.getDiskStoreObjectCount(); // ou cache.getStatistics().getLocalDiskSize() en v2.7.0
		this.inMemoryHits = statistics.getInMemoryHits(); // ou cache.getStatistics().localHeapHitCount() en v2.7.0
		this.cacheHits = statistics.getCacheHits(); // ou cache.getStatistics().cacheHitCount() en v2.7.0
		this.cacheMisses = statistics.getCacheMisses(); // ou devrait être cache.getStatistics().cacheMissCount() en v2.7.0
		this.inMemoryPercentUsed = computeMemoryPercentUsed(cache);
		this.configuration = buildConfiguration(cache);
	}

	private static boolean isEhcacheAvailable() {
		try {
			Class.forName("net.sf.ehcache.Cache");
			// ehcache 2.7.0 existe depuis mars 2013, on ne gère plus ici les versions précédentes de ehcache
			Class.forName("net.sf.ehcache.statistics.StatisticsGateway");
			return true;
		} catch (final ClassNotFoundException | NoClassDefFoundError e) {
			// NoClassDefFoundError for issue 67
			return false;
		}
	}

	static List<CacheInformations> buildCacheInformationsList() {
		if (!EHCACHE_AVAILABLE) {
			return Collections.emptyList();
		}
		final List<CacheManager> allCacheManagers;
		try {
			allCacheManagers = new ArrayList<>(CacheManager.ALL_CACHE_MANAGERS);
		} catch (final NoSuchFieldError e) {
			// nécessaire pour ehcache 1.2 ou avant
			return Collections.emptyList();
		}
		final List<CacheInformations> result = new ArrayList<>();
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
		final List<CacheManager> allCacheManagers = new ArrayList<>(
				CacheManager.ALL_CACHE_MANAGERS);
		for (final CacheManager cacheManager : allCacheManagers) {
			final Ehcache ehcache = cacheManager.getEhcache(cacheId);
			if (ehcache != null) {
				return new CacheInformations(ehcache, true);
			}
		}
		throw new IllegalArgumentException("Cache not found");
	}

	// cache must not be typed,
	// otherwise serialization would not work in the collector server or in jenkins scripts
	private int computeMemoryPercentUsed(Object cache) {
		final long maxElementsInMemory = ((Ehcache) cache).getCacheConfiguration()
				.getMaxEntriesLocalHeap();
		if (maxElementsInMemory == 0) {
			// maxElementsInMemory peut être 0 (sans limite), cf issue 73
			return -1;
		}
		return (int) (100L * inMemoryObjectCount / maxElementsInMemory);
	}

	// cache must not be typed,
	// otherwise serialization would not work in the collector server or in jenkins scripts
	@SuppressWarnings("deprecation")
	private String buildConfiguration(Object cache) {
		final StringBuilder sb = new StringBuilder();
		final CacheConfiguration config = ((Ehcache) cache).getCacheConfiguration();
		sb.append("ehcache [maxEntriesLocalHeap = ").append(config.getMaxEntriesLocalHeap());
		final boolean overflowToDisk = config.isOverflowToDisk();
		sb.append(", overflowToDisk = ").append(overflowToDisk);
		if (overflowToDisk) {
			sb.append(", maxEntriesLocalDisk = ").append(config.getMaxEntriesLocalDisk());
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
