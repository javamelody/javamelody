/*
 * Copyright 2008-2014 by Emeric Vernat
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
package net.bull.javamelody;

import java.io.InputStream;
import java.io.Serializable;
import java.security.CodeSource;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Scanner;

/**
 * Histogramme mémoire.
 * @author Emeric Vernat
 */
class HeapHistogram implements Serializable {
	private static final long serialVersionUID = 2163916067335213382L;

	@SuppressWarnings("all")
	private final List<ClassInfo> classes;
	@SuppressWarnings("all")
	private final List<ClassInfo> permGenClasses;
	private final Date time;
	private long totalHeapBytes;
	private long totalHeapInstances;
	private long totalPermGenBytes;
	private long totalPermgenInstances;
	private boolean sourceDisplayed;
	private boolean deltaDisplayed; // deltaDisplayed kept for backward compatibility with all collect servers

	HeapHistogram(InputStream in, boolean jrockit) {
		time = new Date();
		final Scanner sc = new Scanner(in, "UTF-8");
		final List<ClassInfo> classInfos = scan(sc, jrockit);

		classes = new ArrayList<ClassInfo>();
		permGenClasses = new ArrayList<ClassInfo>();

		for (final ClassInfo classInfo : classInfos) {
			if (classInfo.isPermGen()) {
				permGenClasses.add(classInfo);
				totalPermGenBytes += classInfo.getBytes();
				totalPermgenInstances += classInfo.getInstancesCount();
			} else {
				classes.add(classInfo);
				totalHeapBytes += classInfo.getBytes();
				totalHeapInstances += classInfo.getInstancesCount();
			}
			if (!sourceDisplayed && classInfo.getSource() != null) {
				sourceDisplayed = true;
			}
		}
		if (!jrockit) {
			sc.next("Total");
			final long totalInstances = sc.nextLong();
			final long totalBytes = sc.nextLong();
			assert totalInstances == totalPermgenInstances + totalHeapInstances;
			assert totalBytes == totalPermGenBytes + totalHeapBytes;
		}
		sort();
	}

	void add(HeapHistogram second) {
		final Map<String, ClassInfo> classesMap = new HashMap<String, ClassInfo>(1024);
		final Map<String, ClassInfo> permGenMap = new HashMap<String, ClassInfo>(1024);
		for (final ClassInfo classInfo : classes) {
			addClassInfo(classInfo, classesMap);
		}
		for (final ClassInfo classInfo : permGenClasses) {
			addClassInfo(classInfo, permGenMap);
		}
		for (final ClassInfo classInfo : second.getHeapHistogram()) {
			addClassInfo(classInfo, classesMap);
		}
		for (final ClassInfo classInfo : second.getPermGenHistogram()) {
			addClassInfo(classInfo, permGenMap);
		}
		totalHeapBytes += second.getTotalHeapBytes();
		totalHeapInstances += second.getTotalHeapInstances();
		totalPermGenBytes += second.getTotalPermGenBytes();
		totalPermgenInstances += second.getTotalPermGenInstances();
		classes.clear();
		classes.addAll(classesMap.values());
		permGenClasses.clear();
		permGenClasses.addAll(permGenMap.values());
		sort();
		sourceDisplayed = sourceDisplayed || second.isSourceDisplayed();
	}

	private void addClassInfo(ClassInfo newClInfo, Map<String, ClassInfo> map) {
		final ClassInfo oldClInfo = map.get(newClInfo.getName());
		if (oldClInfo == null) {
			map.put(newClInfo.getName(), newClInfo);
		} else {
			oldClInfo.add(newClInfo);
		}
	}

	Date getTime() {
		return time;
	}

	List<ClassInfo> getHeapHistogram() {
		return Collections.unmodifiableList(classes);
	}

	long getTotalHeapInstances() {
		return totalHeapInstances;
	}

	long getTotalHeapBytes() {
		return totalHeapBytes;
	}

	List<ClassInfo> getPermGenHistogram() {
		return Collections.unmodifiableList(permGenClasses);
	}

	long getTotalPermGenInstances() {
		return totalPermgenInstances;
	}

	long getTotalPermGenBytes() {
		return totalPermGenBytes;
	}

	boolean isSourceDisplayed() {
		return sourceDisplayed;
	}

	@Deprecated
	// deltaDisplayed kept for backward compatibility with all collect servers
	boolean isDeltaDisplayed() {
		return deltaDisplayed;
	}

	private void sort() {
		final Comparator<ClassInfo> classInfoReversedComparator = Collections
				.reverseOrder(new ClassInfoComparator());
		Collections.sort(permGenClasses, classInfoReversedComparator);
		Collections.sort(classes, classInfoReversedComparator);
	}

	static final class ClassInfoComparator implements Comparator<ClassInfo>, Serializable {
		private static final long serialVersionUID = 1L;

		/** {@inheritDoc} */
		@Override
		public int compare(ClassInfo classInfo1, ClassInfo classInfo2) {
			if (classInfo1.getBytes() > classInfo2.getBytes()) {
				return 1;
			} else if (classInfo1.getBytes() < classInfo2.getBytes()) {
				return -1;
			} else {
				return 0;
			}
		}
	}

	private void skipHeader(Scanner sc, boolean jrockit) {
		sc.nextLine();
		sc.nextLine();
		if (!jrockit) {
			sc.skip("-+");
			sc.nextLine();
		}
	}

	private List<ClassInfo> scan(Scanner sc, boolean jrockit) {
		final Map<String, ClassInfo> classInfoMap = new HashMap<String, ClassInfo>(1024);
		sc.useRadix(10);
		skipHeader(sc, jrockit);

		final String nextLine;
		if (jrockit) {
			nextLine = "[0-9.]+%";
		} else {
			nextLine = "[0-9]+:";
		}
		while (sc.hasNext(nextLine)) {
			final ClassInfo newClInfo = new ClassInfo(sc, jrockit);
			addClassInfo(newClInfo, classInfoMap);
		}
		return new ArrayList<ClassInfo>(classInfoMap.values());
	}

	static class ClassInfo implements Serializable {
		private static final long serialVersionUID = 6283636454450216347L;
		private static Map<Character, String> arrayTypes = new HashMap<Character, String>();
		static {
			arrayTypes.put('Z', "boolean");
			arrayTypes.put('C', "char");
			arrayTypes.put('B', "byte");
			arrayTypes.put('S', "short");
			arrayTypes.put('I', "int");
			arrayTypes.put('J', "long");
			arrayTypes.put('F', "float");
			arrayTypes.put('D', "double");
			arrayTypes.put('L', "object");
		}

		private long instances;
		private long bytes;
		private final String jvmName;
		private final String name;
		private final boolean permGen;
		private final String source;

		ClassInfo(Scanner sc, boolean jrockit) {
			super();

			sc.next();
			if (jrockit) {
				bytes = parseLongWithK(sc.next());
				instances = sc.nextLong();
			} else {
				instances = sc.nextLong();
				bytes = sc.nextLong();
			}
			jvmName = sc.next();
			permGen = jvmName.charAt(0) == '<';
			name = convertJVMName();
			source = findSource();
		}

		void add(ClassInfo classInfo) {
			assert getName().equals(classInfo.getName());
			this.bytes += classInfo.getBytes();
			this.instances += classInfo.getInstancesCount();
		}

		String getName() {
			return name;
		}

		long getInstancesCount() {
			return instances;
		}

		long getBytes() {
			return bytes;
		}

		boolean isPermGen() {
			return permGen;
		}

		String getSource() {
			return source;
		}

		private String findSource() {
			// on exclue les classes de PermGen et les classes générées dynamiquement
			if (jvmName.endsWith("Klass>") || jvmName.startsWith("sun.reflect.")) {
				return null;
			}
			try {
				final Class<?> clazz = Class.forName(jvmName);
				return findSource(clazz);
			} catch (final LinkageError e) {
				// dans jonas en OSGI, par exemple avec des classes Quartz, il peut survenir
				// des LinkageError (rq: NoClassDefFoundError hérite également de LinkageError)
				return null;
			} catch (final ClassNotFoundException e) {
				// on suppose qu'il y a une seule webapp et que la plupart des classes peuvent être chargées
				// sinon il y a une exception et on retourne null
				return null;
			}
		}

		private static String findSource(Class<?> clazz) {
			final CodeSource codeSource = clazz.getProtectionDomain().getCodeSource();
			if (codeSource != null && codeSource.getLocation() != null) {
				String src = codeSource.getLocation().toString();
				if (src.startsWith("file:/")) {
					src = src.substring("file:/".length());
				} else if (src.startsWith("vfs:/")) {
					// "vfs:/" pour jboss 6.0
					src = src.substring("vfs:/".length());
				} else if (src.startsWith("reference:file:/")) {
					// "reference:file:/" pour les bundles jonas
					src = src.substring("reference:file:/".length());
				}
				if (src.endsWith(".jar") || src.endsWith(".war")) {
					src = src.intern();
				}
				return src;
			}
			return null;
		}

		private String convertJVMName() {
			String result;
			final int index = jvmName.lastIndexOf('[');

			if (index != -1) {
				final char code = jvmName.charAt(index + 1);
				if (code == 'L') {
					result = jvmName.substring(index + 2, jvmName.length() - 1);
				} else {
					result = arrayTypes.get(code);
					if (result == null) {
						result = jvmName;
					}
				}
				final StringBuilder sb = new StringBuilder(result);
				for (int i = 0; i <= index; i++) {
					sb.append("[]");
				}
				result = sb.toString();
			} else {
				result = jvmName;
			}
			return result.intern();
		}

		static long parseLongWithK(String text) {
			assert text.length() > 0;
			if (text.charAt(text.length() - 1) == 'k') {
				String t = text.substring(0, text.length() - 1);
				if (t.charAt(0) == '+') {
					t = t.substring(1);
				}
				return 1024 * Long.parseLong(t);
			}
			// inutile car le total n'est pas lu
			//			else if (text.endsWith("kB")) {
			//				return 1024 * Long.parseLong(text.substring(0, text.length() - 2));
			//			}
			return Long.parseLong(text);
		}
	}
}
