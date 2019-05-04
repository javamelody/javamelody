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

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import javax.cache.Cache;
import javax.cache.CacheManager;
import javax.cache.Caching;
import javax.cache.spi.CachingProvider;
import javax.management.JMException;
import javax.management.MBeanServer;
import javax.management.MalformedObjectNameException;
import javax.management.ObjectName;

/**
 * Informations sur un cache de données JCache (JSR107).
 * L'état d'une instance est initialisé à son instanciation et non mutable;
 * il est donc de fait thread-safe.
 * Cet état est celui d'un cache à un instant t.
 * Les instances sont sérialisables pour pouvoir être transmises au serveur de collecte.
 * @author James Pether Sörling
 * @author Emeric Vernat
 */
public class JCacheInformations implements Serializable {
	private static final long serialVersionUID = -3025833425994923286L;
	private static final MBeanServer MBEAN_SERVER = MBeans.getPlatformMBeanServer();
	private static final boolean JCACHE_AVAILABLE = isJCacheAvailable();

	private final String name;
	private final long cacheHits;
	private final long cacheMisses;
	private List<?> cacheKeys;

	JCacheInformations(ObjectName cache) {
		super();
		assert cache != null;
		this.name = cache.getKeyProperty("Cache");
		this.cacheHits = getValue(cache, "CacheHits");
		this.cacheMisses = getValue(cache, "CacheMisses");
		// an element can be put several times in a cache before being removed or evicted
		// so objectCount != CachePuts - CacheRemovals - CacheEvictions
		// size and keys can only be read by javax.cache.Cache.iterator()
	}

	JCacheInformations(String cacheName) {
		super();
		assert cacheName != null;
		this.name = cacheName;
		this.cacheHits = -1;
		this.cacheMisses = 1;
	}

	private static Long getValue(ObjectName cache, String attribute) {
		try {
			return (Long) MBEAN_SERVER.getAttribute(cache, attribute);
		} catch (JMException e) {
			return -1L;
		}
	}

	static List<JCacheInformations> buildJCacheInformationsList() {
		if (!JCACHE_AVAILABLE) {
			return Collections.emptyList();
		}

		final List<JCacheInformations> result = new ArrayList<JCacheInformations>();
		final Set<String> cacheNames = new HashSet<String>();
		final Set<ObjectName> cacheStatistics = getJsr107CacheStatistics();
		for (final ObjectName cache : cacheStatistics) {
			final JCacheInformations jcacheInformations = new JCacheInformations(cache);
			result.add(jcacheInformations);
			cacheNames.add(jcacheInformations.getName());
		}
		for (final CachingProvider cachingProvider : Caching.getCachingProviders()) {
			final CacheManager cacheManager = cachingProvider.getCacheManager();
			for (final String cacheName : cacheManager.getCacheNames()) {
				if (!cacheNames.contains(cacheName)) {
					final JCacheInformations jcacheInformations = new JCacheInformations(cacheName);
					result.add(jcacheInformations);
					cacheNames.add(jcacheInformations.getName());
				}
			}
		}
		return result;
	}

	public static JCacheInformations buildJCacheInformationsWithKeys(String cacheId) {
		assert JCACHE_AVAILABLE;
		assert cacheId != null;
		for (final CachingProvider cachingProvider : Caching.getCachingProviders()) {
			final CacheManager cacheManager = cachingProvider.getCacheManager();
			for (final String cacheName : cacheManager.getCacheNames()) {
				if (cacheName.equals(cacheId)) {
					// getCache may never return null
					final Cache<Object, Object> cache = cacheManager.getCache(cacheId);
					final List<Object> cacheKeys = new ArrayList<Object>();
					for (final Iterator<Cache.Entry<Object, Object>> it = cache.iterator(); it
							.hasNext();) {
						cacheKeys.add(it.next().getKey());
					}
					for (final JCacheInformations cacheInformations : buildJCacheInformationsList()) {
						if (cacheInformations.getName().equals(cacheId)) {
							cacheInformations.cacheKeys = cacheKeys;
							return cacheInformations; // NOPMD
						}
					}
				}
			}
		}
		return null;
	}

	private static Set<ObjectName> getJsr107CacheStatistics() {
		try {
			final ObjectName objectName = new ObjectName("javax.cache:type=CacheStatistics,*");
			return MBEAN_SERVER.queryNames(objectName, null);
		} catch (MalformedObjectNameException e) {
			throw new IllegalStateException(e);
		}
	}

	private static boolean isJCacheAvailable() {
		try {
			Class.forName("javax.cache.Cache");
			return true;
		} catch (final ClassNotFoundException e) {
			return false;
		}
	}

	public String getName() {
		return name;
	}

	public long getCacheHits() {
		return cacheHits;
	}

	public long getCacheMisses() {
		return cacheMisses;
	}

	// efficacité en pourcentage du cache par rapport au total des accès
	public int getHitsRatio() {
		final long accessCount = cacheHits + cacheMisses;
		if (accessCount == 0) {
			return -1;
		}
		return (int) (100 * cacheHits / accessCount);
	}

	public List<?> getCacheKeys() {
		return cacheKeys;
	}

	/** {@inheritDoc} */
	@Override
	public String toString() {
		return getClass().getSimpleName() + "[name=" + getName() + ", hitsRatio=" + getHitsRatio()
				+ ']';
	}
}
