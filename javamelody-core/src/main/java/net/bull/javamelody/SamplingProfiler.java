/*
 * Copyright 2008-2012 by Emeric Vernat
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
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

/**
 * Detect CPU hotspots CPU by periodic sampling of the stack-traces of the threads.
 * @author Emeric Vernat with some ideas from C&eacute;drik Lime
 */
class SamplingProfiler {
	/**
	 * Excluded packages by default : those of the jvm, of tomcat...
	 */
	private static final String[] DEFAULT_EXCLUDED_PACKAGES = new String[] { "java.", "sun.",
			"com.sun.", "javax.", "org.apache.", "org.hibernate.", "oracle.", "org.postgresql.",
			"org.eclipse.", };

	/**
	 * Maximum number of methods to hold into memory
	 */
	private final int maxDataSize = 10000;

	private final String[] excludedPackages;

	private final Map<SampledMethod, SampledMethod> data = new HashMap<SampledMethod, SampledMethod>();

	static class SampledMethod implements Comparable<SampledMethod>, Serializable {
		private static final long serialVersionUID = 1L;

		private long count;

		private final String className;

		private final String methodName;

		private transient int hashCode;

		SampledMethod(String className, String methodName) {
			super();
			assert className != null;
			assert methodName != null;
			this.className = className;
			this.methodName = methodName;
			this.hashCode = className.hashCode() * 31 + methodName.hashCode();
		}

		// hashCode is transient
		private Object readResolve() {
			this.hashCode = className.hashCode() * 31 + methodName.hashCode();
			return this;
		}

		void incrementCount() {
			count++;
		}

		long getCount() {
			return count;
		}

		void setCount(long count) {
			this.count = count;
		}

		String getClassName() {
			return this.className;
		}

		String getMethodName() {
			return this.methodName;
		}

		@Override
		public int compareTo(SampledMethod method) {
			return count < method.count ? 1 : count == method.count ? 0 : -1;
		}

		@Override
		public int hashCode() {
			return hashCode;
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
			if (!methodName.equals(other.methodName) || !className.equals(other.className)) {
				return false;
			}
			return true;
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
	SamplingProfiler() {
		super();
		this.excludedPackages = DEFAULT_EXCLUDED_PACKAGES;
	}

	/**
	 * Constructor.
	 * @param excludedPackages List of excluded packages
	 */
	SamplingProfiler(List<String> excludedPackages) {
		super();
		assert excludedPackages != null;
		final String[] packages = excludedPackages.toArray(new String[excludedPackages.size()]);
		for (int i = 0; i < packages.length; i++) {
			packages[i] = packages[i].trim();
			if (packages[i].length() == 0) {
				throw new IllegalArgumentException("An excluded package can not be empty: "
						+ excludedPackages);
			}
			if (!packages[i].endsWith(".")) {
				packages[i] = packages[i] + '.';
			}
		}
		this.excludedPackages = packages;
	}

	synchronized void update() {
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
		final SampledMethod key = new SampledMethod(element.getClassName(), element.getMethodName());
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
		while (size > maxDataSize) {
			final Iterator<SampledMethod> iterator = data.keySet().iterator();
			while (iterator.hasNext() && size > maxDataSize) {
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
		final String className = element.getClassName();
		for (final String excludedPackage : excludedPackages) {
			if (className.startsWith(excludedPackage)) {
				return true;
			}
		}
		return false;
	}

	synchronized List<SampledMethod> getHotspots(int rows) {
		final List<SampledMethod> methods = new ArrayList<SampledMethod>(data.values());
		Collections.sort(methods);
		return methods.subList(0, Math.min(rows, methods.size()));
	}

	synchronized void clear() {
		data.clear();
	}
}
