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
import java.lang.management.GarbageCollectorMXBean;
import java.lang.management.ManagementFactory;
import java.lang.management.MemoryPoolMXBean;
import java.lang.management.MemoryUsage;
import java.lang.management.OperatingSystemMXBean;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.text.DecimalFormat;
import java.util.HashSet;
import java.util.Set;

import javax.management.JMException;
import javax.management.MalformedObjectNameException;
import javax.management.ObjectName;

import net.bull.javamelody.internal.common.I18N;
import net.bull.javamelody.internal.common.LOG;

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
	private static final Set<ObjectName> NIO_BUFFER_POOLS = new HashSet<ObjectName>();

	static {
		try {
			NIO_BUFFER_POOLS.addAll(new MBeans().getNioBufferPools());
		} catch (final MalformedObjectNameException e) {
			LOG.debug(e.toString());
		}
	}

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
		usedBufferedMemory = getUsedBufferMemory();
		loadedClassesCount = ManagementFactory.getClassLoadingMXBean().getLoadedClassCount();
		garbageCollectionTimeMillis = buildGarbageCollectionTimeMillis();

		final OperatingSystemMXBean operatingSystem = ManagementFactory.getOperatingSystemMXBean();
		if (isSunOsMBean(operatingSystem)) {
			usedPhysicalMemorySize = getLongFromOperatingSystem(operatingSystem,
					"getTotalPhysicalMemorySize")
					- getLongFromOperatingSystem(operatingSystem, "getFreePhysicalMemorySize");
			usedSwapSpaceSize = getLongFromOperatingSystem(operatingSystem, "getTotalSwapSpaceSize")
					- getLongFromOperatingSystem(operatingSystem, "getFreeSwapSpaceSize");
		} else {
			usedPhysicalMemorySize = -1;
			usedSwapSpaceSize = -1;
		}

		memoryDetails = buildMemoryDetails();
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

	private static long getUsedBufferMemory() {
		if (NIO_BUFFER_POOLS.isEmpty()) {
			return -1;
		}
		long result = 0;
		final MBeans mBeans = new MBeans();
		try {
			for (final ObjectName objectName : NIO_BUFFER_POOLS) {
				// adds direct and mapped buffers
				result += (Long) mBeans.getAttribute(objectName, "MemoryUsed");
			}
		} catch (final JMException e) {
			// n'est pas censé arriver
			throw new IllegalStateException(e);
		}
		return result;
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
		final OperatingSystemMXBean operatingSystem = ManagementFactory.getOperatingSystemMXBean();
		String osInfo = "";
		if (isSunOsMBean(operatingSystem)) {
			osInfo = "Process cpu time = " + integerFormat.format(
					getLongFromOperatingSystem(operatingSystem, "getProcessCpuTime") / 1000000)
					+ " ms,\nCommitted virtual memory = "
					+ integerFormat.format(getLongFromOperatingSystem(operatingSystem,
							"getCommittedVirtualMemorySize") / 1024 / 1024)
					+ MO + ",\nFree physical memory = "
					+ integerFormat.format(getLongFromOperatingSystem(operatingSystem,
							"getFreePhysicalMemorySize") / 1024 / 1024)
					+ MO + ",\nTotal physical memory = "
					+ integerFormat.format(getLongFromOperatingSystem(operatingSystem,
							"getTotalPhysicalMemorySize") / 1024 / 1024)
					+ MO + ",\nFree swap space = "
					+ integerFormat.format(
							getLongFromOperatingSystem(operatingSystem, "getFreeSwapSpaceSize")
									/ 1024 / 1024)
					+ MO + ",\nTotal swap space = "
					+ integerFormat.format(
							getLongFromOperatingSystem(operatingSystem, "getTotalSwapSpaceSize")
									/ 1024 / 1024)
					+ MO;
		}
		if (usedBufferedMemory < 0) {
			return nonHeapMemory + NEXT + classLoading + NEXT + gc + NEXT + osInfo;
		}
		final String bufferedMemory = "Buffered memory = "
				+ integerFormat.format(usedBufferedMemory / 1024 / 1024) + MO;
		return nonHeapMemory + NEXT + bufferedMemory + NEXT + classLoading + NEXT + gc + NEXT
				+ osInfo;
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

	static long getLongFromOperatingSystem(OperatingSystemMXBean operatingSystem,
			String methodName) {
		return (Long) getFromOperatingSystem(operatingSystem, methodName);
	}

	static double getDoubleFromOperatingSystem(OperatingSystemMXBean operatingSystem,
			String methodName) {
		return (Double) getFromOperatingSystem(operatingSystem, methodName);
	}

	static Object getFromOperatingSystem(OperatingSystemMXBean operatingSystem, String methodName) {
		try {
			final Method method = operatingSystem.getClass().getMethod(methodName,
					(Class<?>[]) null);
			method.setAccessible(true);
			return method.invoke(operatingSystem, (Object[]) null);
		} catch (final InvocationTargetException e) {
			if (e.getCause() instanceof Error) {
				throw (Error) e.getCause();
			} else if (e.getCause() instanceof RuntimeException) {
				throw (RuntimeException) e.getCause();
			}
			throw new IllegalStateException(e.getCause());
		} catch (final NoSuchMethodException e) {
			throw new IllegalArgumentException(e);
		} catch (final IllegalAccessException e) {
			throw new IllegalStateException(e);
		}
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

	long getUsedNonHeapMemory() {
		return usedNonHeapMemory;
	}

	long getUsedBufferedMemory() {
		return usedBufferedMemory;
	}

	int getLoadedClassesCount() {
		return loadedClassesCount;
	}

	public long getGarbageCollectionTimeMillis() {
		return garbageCollectionTimeMillis;
	}

	long getUsedPhysicalMemorySize() {
		return usedPhysicalMemorySize;
	}

	long getUsedSwapSpaceSize() {
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
