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
import java.lang.management.GarbageCollectorMXBean;
import java.lang.management.ManagementFactory;
import java.lang.management.MemoryPoolMXBean;
import java.lang.management.MemoryUsage;
import java.lang.management.OperatingSystemMXBean;
import java.text.DecimalFormat;

/**
 * Informations systèmes sur la mémoire du serveur, sans code html de présentation.
 * L'état d'une instance est initialisé à son instanciation et non mutable;
 * il est donc de fait thread-safe.
 * Cet état est celui d'une instance de JVM java.
 * Les instances sont sérialisables pour pouvoir être transmises au serveur de collecte.
 * @author Emeric Vernat
 */
class MemoryInformations implements Serializable {
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
		loadedClassesCount = ManagementFactory.getClassLoadingMXBean().getLoadedClassCount();
		garbageCollectionTimeMillis = buildGarbageCollectionTimeMillis();

		final OperatingSystemMXBean operatingSystem = ManagementFactory.getOperatingSystemMXBean();
		if (isSunOsMBean(operatingSystem)) {
			final com.sun.management.OperatingSystemMXBean osBean = (com.sun.management.OperatingSystemMXBean) operatingSystem;
			usedPhysicalMemorySize = osBean.getTotalPhysicalMemorySize()
					- osBean.getFreePhysicalMemorySize();
			usedSwapSpaceSize = osBean.getTotalSwapSpaceSize() - osBean.getFreeSwapSpaceSize();
		} else {
			usedPhysicalMemorySize = -1;
			usedSwapSpaceSize = -1;
		}

		memoryDetails = buildMemoryDetails();
	}

	private static MemoryPoolMXBean getPermGenMemoryPool() {
		for (final MemoryPoolMXBean memoryPool : ManagementFactory.getMemoryPoolMXBeans()) {
			if ("Perm Gen".equals(memoryPool.getName())) {
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
				+ integerFormat.format(usedNonHeapMemory / 1024 / 1024) + MO;
		// classes actuellement chargées
		final String classLoading = "Loaded classes = " + integerFormat.format(loadedClassesCount);
		final String gc = "Garbage collection time = "
				+ integerFormat.format(garbageCollectionTimeMillis) + " ms";
		final OperatingSystemMXBean operatingSystem = ManagementFactory.getOperatingSystemMXBean();
		String osInfo = "";
		if (isSunOsMBean(operatingSystem)) {
			final com.sun.management.OperatingSystemMXBean osBean = (com.sun.management.OperatingSystemMXBean) operatingSystem;
			osInfo = "Process cpu time = "
					+ integerFormat.format(osBean.getProcessCpuTime() / 1000000)
					+ " ms,\nCommitted virtual memory = "
					+ integerFormat.format(osBean.getCommittedVirtualMemorySize() / 1024 / 1024)
					+ MO + ",\nFree physical memory = "
					+ integerFormat.format(osBean.getFreePhysicalMemorySize() / 1024 / 1024) + MO
					+ ",\nTotal physical memory = "
					+ integerFormat.format(osBean.getTotalPhysicalMemorySize() / 1024 / 1024) + MO
					+ ",\nFree swap space = "
					+ integerFormat.format(osBean.getFreeSwapSpaceSize() / 1024 / 1024) + MO
					+ ",\nTotal swap space = "
					+ integerFormat.format(osBean.getTotalSwapSpaceSize() / 1024 / 1024) + MO;
		}

		return nonHeapMemory + NEXT + classLoading + NEXT + gc + NEXT + osInfo;
	}

	private static boolean isSunOsMBean(OperatingSystemMXBean operatingSystem) {
		// on ne teste pas operatingSystem instanceof com.sun.management.OperatingSystemMXBean
		// car le package com.sun n'existe à priori pas sur une jvm tierce
		final String className = operatingSystem.getClass().getName();
		return "com.sun.management.OperatingSystem".equals(className)
				|| "com.sun.management.UnixOperatingSystem".equals(className);
	}

	long getUsedMemory() {
		return usedMemory;
	}

	long getMaxMemory() {
		return maxMemory;
	}

	long getUsedPermGen() {
		return usedPermGen;
	}

	long getMaxPermGen() {
		return maxPermGen;
	}

	long getUsedNonHeapMemory() {
		return usedNonHeapMemory;
	}

	int getLoadedClassesCount() {
		return loadedClassesCount;
	}

	long getGarbageCollectionTimeMillis() {
		return garbageCollectionTimeMillis;
	}

	long getUsedPhysicalMemorySize() {
		return usedPhysicalMemorySize;
	}

	long getUsedSwapSpaceSize() {
		return usedSwapSpaceSize;
	}

	String getMemoryDetails() {
		return memoryDetails;
	}

	/** {@inheritDoc} */
	@Override
	public String toString() {
		return getClass().getSimpleName() + "[usedMemory=" + getUsedMemory() + ", maxMemroy="
				+ getMaxMemory() + ']';
	}
}
