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
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

/**
 * Detect CPU hotspots CPU by periodic sampling of the stack-traces of the threads.
 * @author Emeric Vernat with some ideas from C&eacute;drik Lime
 */
public class SamplingProfiler {
	/**
	 * Excluded packages by default : those of the jvm, of tomcat...
	 */
	private static final String[] DEFAULT_EXCLUDED_PACKAGES = { "java.", "sun.", "com.sun.",
			"javax.", "org.apache.", "org.hibernate.", "oracle.", "org.postgresql.",
			"org.eclipse.", };

	/**
	 * Maximum number of methods to hold into memory
	 */
	private static final int MAX_DATA_SIZE = 10000;

	private final String[] excludedPackages;

	private final String[] includedPackages;

	private final Map<SampledMethod, SampledMethod> data = new HashMap<SampledMethod, SampledMethod>();

	/**
	 * Sampled method.
	 * @author Emeric Vernat
	 */
	public static class SampledMethod implements Comparable<SampledMethod>, Serializable {
		private static final long serialVersionUID = 1L;

		private long count;

		private final String className;

		private final String methodName;

		private transient int hash;

		SampledMethod(String className, String methodName) {
			super();
			assert className != null;
			assert methodName != null;
			this.className = className;
			this.methodName = methodName;
			this.hash = className.hashCode() * 31 + methodName.hashCode();
		}

		// hash is transient
		private Object readResolve() {
			this.hash = className.hashCode() * 31 + methodName.hashCode();
			return this;
		}

		void incrementCount() {
			count++;
		}

		public long getCount() {
			return count;
		}

		void setCount(long count) {
			this.count = count;
		}

		public String getClassName() {
			return this.className;
		}

		public String getMethodName() {
			return this.methodName;
		}

		@Override
		public int compareTo(SampledMethod method) {
			return count < method.count ? 1 : count == method.count ? 0 : -1;
		}

		@Override
		public int hashCode() {
			return hash;
		}

		@Override
		public boolean equals(Object obj) {
			if (this == obj) {
				return true;
			}
			if (obj == null) {
				return false;
			}
			if (getClass() != obj.getClass()) {
				return false;
			}
			final SampledMethod other = (SampledMethod) obj;
			return methodName.equals(other.methodName) && className.equals(other.className);
		}

		@Override
		public String toString() {
			return className + '.' + methodName;
		}
	}

	/**
	 * Constructor.
	 * Excluded packages by default "java,sun,com.sun,javax,org.apache,org.hibernate,oracle,org.postgresql,org.eclipse"
	 */
	public SamplingProfiler() {
		super();
		this.excludedPackages = DEFAULT_EXCLUDED_PACKAGES;
		this.includedPackages = null;
	}

	/**
	 * Constructor.
	 * @param excludedPackages List of excluded packages (can be null)
	 * @param includedPackages List of included packages (can be null)
	 */
	public SamplingProfiler(List<String> excludedPackages, List<String> includedPackages) {
		super();
		assert excludedPackages != null || includedPackages != null;
		// In general, there are either excluded packages or included packages.
		// (If both, excluded result has priority over included result: it excludes some included.)
		this.excludedPackages = verifyPackageNames(excludedPackages);
		this.includedPackages = verifyPackageNames(includedPackages);
	}

	/**
	 * Constructor.
	 * @param excludedPackages List of excluded packages separated by comma (can be null)
	 * @param includedPackages List of included packages separated by comma (can be null)
	 */
	public SamplingProfiler(String excludedPackages, String includedPackages) {
		this(splitPackageNames(excludedPackages), splitPackageNames(includedPackages));
		// In general, there are either excluded packages or included packages.
		// (If both, excluded result has priority over included result: it excludes some included.)
	}

	private static List<String> splitPackageNames(String packageNames) {
		if (packageNames == null) {
			return null;
		}
		return Arrays.asList(packageNames.split(","));
	}

	private String[] verifyPackageNames(List<String> packageNames) {
		if (packageNames == null) {
			return null;
		}
		final String[] packages = packageNames.toArray(new String[0]);
		for (int i = 0; i < packages.length; i++) {
			packages[i] = packages[i].trim(); // NOPMD
			if (packages[i].isEmpty()) {
				throw new IllegalArgumentException(
						"A package can not be empty, item " + i + " in " + packageNames);
			}
			if (!packages[i].endsWith(".")) {
				packages[i] = packages[i] + '.'; // NOPMD
			}
		}
		return packages;
	}

	public synchronized void update() {
		final Map<Thread, StackTraceElement[]> stackTraces = Thread.getAllStackTraces();
		try {
			final Thread currentThread = Thread.currentThread();
			for (final Map.Entry<Thread, StackTraceElement[]> entry : stackTraces.entrySet()) {
				final Thread thread = entry.getKey();
				final StackTraceElement[] stackTrace = entry.getValue();
				if (stackTrace.length > 0 && thread.getState() == Thread.State.RUNNABLE
						&& thread != currentThread) {
					for (final StackTraceElement element : stackTrace) {
						if (!isPackageExcluded(element)) {
							addSample(element);
							break;
						}
					}
				}
			}
		} finally {
			limitDataSize();
		}
	}

	private void addSample(StackTraceElement element) {
		final SampledMethod key = new SampledMethod(element.getClassName(),
				element.getMethodName());
		// or final String key = element.getClassName() + '.' + element.getMethodName();
		SampledMethod method = this.data.get(key);
		if (method == null) {
			method = key;
			// or method = new SampledMethod(element.getClassName(), element.getMethodName());
			this.data.put(key, method);
		}
		// on pourrait incrémenter la valeur selon l'augmentation de cpuTime pour ce thread,
		// mais l'intervalle entre deux samples est probablement trop grand
		// pour que le cpu du thread entre les deux intervalles ait un rapport avec cette méthode
		method.incrementCount();
	}

	private void limitDataSize() {
		long minCount = 1;
		int size = data.size();
		while (size > MAX_DATA_SIZE) {
			final Iterator<SampledMethod> iterator = data.keySet().iterator();
			while (iterator.hasNext() && size > MAX_DATA_SIZE) {
				final SampledMethod method = iterator.next();
				if (method.getCount() <= minCount) {
					iterator.remove();
					size--;
				}
			}
			minCount++;
		}
	}

	private boolean isPackageExcluded(StackTraceElement element) {
		return excludedPackages != null && isPackageMatching(element, excludedPackages)
				|| includedPackages != null && !isPackageMatching(element, includedPackages);
	}

	private boolean isPackageMatching(StackTraceElement element, String[] packageNames) {
		final String className = element.getClassName();
		for (final String packageName : packageNames) {
			if (className.startsWith(packageName)) {
				return true;
			}
		}
		return false;
	}

	public synchronized List<SampledMethod> getHotspots(int rows) {
		final List<SampledMethod> methods = new ArrayList<SampledMethod>(data.values());
		Collections.sort(methods);
		return methods.subList(0, Math.min(rows, methods.size()));
	}

	public synchronized void clear() {
		data.clear();
	}
}
