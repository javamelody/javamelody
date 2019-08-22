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
import java.lang.management.GarbageCollectorMXBean;
import java.lang.management.ManagementFactory;
import java.lang.management.MemoryPoolMXBean;
import java.lang.management.MemoryUsage;
import java.text.DecimalFormat;

import net.bull.javamelody.internal.common.I18N;

/**
 * Informations systèmes sur la mémoire du serveur, sans code html de présentation.
 * L'état d'une instance est initialisé à son instanciation et non mutable;
 * il est donc de fait thread-safe.
 * Cet état est celui d'une instance de JVM java.
 * Les instances sont sérialisables pour pouvoir être transmises au serveur de collecte.
 * @author Emeric Vernat
 */
public class MemoryInformations implements Serializable {
	private static final long serialVersionUID = 3281861236369720876L;
	private static final String NEXT = ",\n";
	private static final String MO = " Mo";

	// usedMemory est la mémoire utilisée du heap (voir aussi non heap dans gestion mémoire)
	private final long usedMemory;
	// maxMemory est la mémoire maximum pour le heap (paramètre -Xmx1024m par exemple)
	private final long maxMemory;
	// usedPermGen est la mémoire utilisée de "Perm Gen" (classes et les instances de String "interned")
	private final long usedPermGen;
	// maxPermGen est la mémoire maximum pour "Perm Gen" (paramètre -XX:MaxPermSize=128m par exemple)
	private final long maxPermGen;
	private final long usedNonHeapMemory;
	private final long usedBufferedMemory;
	private final int loadedClassesCount;
	private final long garbageCollectionTimeMillis;
	private final long usedPhysicalMemorySize;
	private final long usedSwapSpaceSize;
	private final String memoryDetails;

	MemoryInformations() {
		super();
		usedMemory = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory();
		maxMemory = Runtime.getRuntime().maxMemory();
		final MemoryPoolMXBean permGenMemoryPool = getPermGenMemoryPool();
		if (permGenMemoryPool != null) {
			final MemoryUsage usage = permGenMemoryPool.getUsage();
			usedPermGen = usage.getUsed();
			maxPermGen = usage.getMax();
		} else {
			usedPermGen = -1;
			maxPermGen = -1;
		}
		usedNonHeapMemory = ManagementFactory.getMemoryMXBean().getNonHeapMemoryUsage().getUsed();
		usedBufferedMemory = MBeansAccessor.getUsedBufferMemory();
		loadedClassesCount = ManagementFactory.getClassLoadingMXBean().getLoadedClassCount();
		garbageCollectionTimeMillis = buildGarbageCollectionTimeMillis();

		usedPhysicalMemorySize = MBeansAccessor
				.getLongFromOperatingSystem("TotalPhysicalMemorySize")
				- MBeansAccessor.getLongFromOperatingSystem("FreePhysicalMemorySize");
		usedSwapSpaceSize = buildUsedSwapSpaceSize();

		memoryDetails = buildMemoryDetails();
	}

	private long buildUsedSwapSpaceSize() {
		try {
			return MBeansAccessor.getLongFromOperatingSystem("TotalSwapSpaceSize")
					- MBeansAccessor.getLongFromOperatingSystem("FreeSwapSpaceSize");
		} catch (final Throwable e) { // NOPMD
			// pour issues 794, dans google appengine
			// javax.management.RuntimeErrorException: java.lang.InternalError: errno: 38 error: sysinfo failed to get swap size
			return -1;
		}
	}

	private static MemoryPoolMXBean getPermGenMemoryPool() {
		for (final MemoryPoolMXBean memoryPool : ManagementFactory.getMemoryPoolMXBeans()) {
			// name est "Perm Gen" ou "PS Perm Gen" (32 vs 64 bits ?)
			if (memoryPool.getName().endsWith("Perm Gen")) {
				return memoryPool;
			}
		}
		return null;
	}

	private static long buildGarbageCollectionTimeMillis() {
		long garbageCollectionTime = 0;
		for (final GarbageCollectorMXBean garbageCollector : ManagementFactory
				.getGarbageCollectorMXBeans()) {
			garbageCollectionTime += garbageCollector.getCollectionTime();
		}
		return garbageCollectionTime;
	}

	private String buildMemoryDetails() {
		final DecimalFormat integerFormat = I18N.createIntegerFormat();
		final String nonHeapMemory = "Non heap memory = "
				+ integerFormat.format(usedNonHeapMemory / 1024 / 1024) + MO
				+ " (Perm Gen, Code Cache)";
		// classes actuellement chargées
		final String classLoading = "Loaded classes = " + integerFormat.format(loadedClassesCount);
		final String gc = "Garbage collection time = "
				+ integerFormat.format(garbageCollectionTimeMillis) + " ms";
		String osInfo = "";
		String osInfo2 = "";
		final long totalPhysicalMemorySize = MBeansAccessor
				.getLongFromOperatingSystem("TotalPhysicalMemorySize");
		if (totalPhysicalMemorySize >= 0) {
			osInfo = "Process cpu time = "
					+ integerFormat.format(
							MBeansAccessor.getLongFromOperatingSystem("ProcessCpuTime") / 1000000)
					+ " ms,\nCommitted virtual memory = "
					+ integerFormat.format(
							MBeansAccessor.getLongFromOperatingSystem("CommittedVirtualMemorySize")
									/ 1024 / 1024)
					+ MO + ",\nFree physical memory = "
					+ integerFormat.format(
							MBeansAccessor.getLongFromOperatingSystem("FreePhysicalMemorySize")
									/ 1024 / 1024)
					+ MO + ",\nTotal physical memory = "
					+ integerFormat.format(totalPhysicalMemorySize / 1024 / 1024) + MO;
			try {
				osInfo2 = ",\nFree swap space = "
						+ integerFormat.format(
								MBeansAccessor.getLongFromOperatingSystem("FreeSwapSpaceSize")
										/ 1024 / 1024)
						+ MO + ",\nTotal swap space = "
						+ integerFormat.format(
								MBeansAccessor.getLongFromOperatingSystem("TotalSwapSpaceSize")
										/ 1024 / 1024)
						+ MO;
			} catch (final Throwable e) { // NOPMD
				// pour issues 794, dans google appengine
				// javax.management.RuntimeErrorException: java.lang.InternalError: errno: 38 error: sysinfo failed to get swap size
			}
		}
		if (usedBufferedMemory < 0) {
			return nonHeapMemory + NEXT + classLoading + NEXT + gc + NEXT + osInfo + osInfo2;
		}
		final String bufferedMemory = "Buffered memory = "
				+ integerFormat.format(usedBufferedMemory / 1024 / 1024) + MO;
		return nonHeapMemory + NEXT + bufferedMemory + NEXT + classLoading + NEXT + gc + NEXT
				+ osInfo;
	}

	public long getUsedMemory() {
		return usedMemory;
	}

	public long getMaxMemory() {
		return maxMemory;
	}

	public double getUsedMemoryPercentage() {
		return 100d * usedMemory / maxMemory;
	}

	public long getUsedPermGen() {
		return usedPermGen;
	}

	public long getMaxPermGen() {
		return maxPermGen;
	}

	public double getUsedPermGenPercentage() {
		if (usedPermGen > 0 && maxPermGen > 0) {
			return 100d * usedPermGen / maxPermGen;
		}
		return -1d;
	}

	public long getUsedNonHeapMemory() {
		return usedNonHeapMemory;
	}

	public long getUsedBufferedMemory() {
		return usedBufferedMemory;
	}

	public int getLoadedClassesCount() {
		return loadedClassesCount;
	}

	public long getGarbageCollectionTimeMillis() {
		return garbageCollectionTimeMillis;
	}

	public long getUsedPhysicalMemorySize() {
		return usedPhysicalMemorySize;
	}

	public long getUsedSwapSpaceSize() {
		return usedSwapSpaceSize;
	}

	public String getMemoryDetails() {
		return memoryDetails;
	}

	/** {@inheritDoc} */
	@Override
	public String toString() {
		return getClass().getSimpleName() + "[usedMemory=" + getUsedMemory() + ", maxMemory="
				+ getMaxMemory() + ']';
	}
}
