/*
 * Copyright 2007-2008 Sun Microsystems, Inc.  All Rights Reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.  Sun designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Sun in the LICENSE file that accompanied this code.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Sun Microsystems, Inc., 4150 Network Circle, Santa Clara,
 * CA 95054 USA or visit www.sun.com if you need additional information or
 * have any questions.
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
 * Classe HeapHistogramImpl de VisualVM et issue de netbeans.org, (c) Sun Microsystems.
 * Adaptée uniquement pour le style de code, le comparateur, la méthode add et la source.
 * @author Tomas Hurka
 */
class HeapHistogram implements Serializable {
	private static final long serialVersionUID = 2163916067335213382L;
	private static final String BOOLEAN_TEXT = "boolean"; // NOI18N
	private static final String CHAR_TEXT = "char"; // NOI18N
	private static final String BYTE_TEXT = "byte"; // NOI18N
	private static final String SHORT_TEXT = "short"; // NOI18N
	private static final String INT_TEXT = "int"; // NOI18N
	private static final String LONG_TEXT = "long"; // NOI18N
	private static final String FLOAT_TEXT = "float"; // NOI18N
	private static final String DOUBLE_TEXT = "double"; // NOI18N
	//	private static final String VOID_TEXT = "void"; // NOI18N
	private static final char BOOLEAN_CODE = 'Z'; // NOI18N
	private static final char CHAR_CODE = 'C'; // NOI18N
	private static final char BYTE_CODE = 'B'; // NOI18N
	private static final char SHORT_CODE = 'S'; // NOI18N
	private static final char INT_CODE = 'I'; // NOI18N
	private static final char LONG_CODE = 'J'; // NOI18N
	private static final char FLOAT_CODE = 'F'; // NOI18N
	private static final char DOUBLE_CODE = 'D'; // NOI18N
	private static final char OBJECT_CODE = 'L'; // NOI18N
	@SuppressWarnings("all")
	private final List<ClassInfo> classes;
	@SuppressWarnings("all")
	private final List<ClassInfo> permGenClasses;
	private final Date time;
	//	private final long totalBytes;
	//	private final long totalInstances;
	private long totalHeapBytes;
	private long totalHeapInstances;
	private long totalPermGenBytes;
	private long totalPermgenInstances;
	private boolean sourceDisplayed;
	private boolean deltaDisplayed;

	HeapHistogram(InputStream in, boolean jrockit) {
		final Map<String, ClassInfo> classesMap = new HashMap<String, ClassInfo>(1024);
		final Map<String, ClassInfo> permGenMap = new HashMap<String, ClassInfo>(1024);
		time = new Date();
		final Scanner sc = new Scanner(in, "UTF-8"); // NOI18N
		sc.useRadix(10);
		skipHeader(sc, jrockit);

		final String nextLine;
		if (jrockit) {
			nextLine = "[0-9.]+%";
		} else {
			nextLine = "[0-9]+:";
		}
		while (sc.hasNext(nextLine)) { // NOI18N
			final ClassInfo newClInfo = new ClassInfo(sc, jrockit);
			if (newClInfo.isPermGen()) {
				storeClassInfo(newClInfo, permGenMap);
				totalPermGenBytes += newClInfo.getBytes();
				totalPermgenInstances += newClInfo.getInstancesCount();
			} else {
				storeClassInfo(newClInfo, classesMap);
				totalHeapBytes += newClInfo.getBytes();
				totalHeapInstances += newClInfo.getInstancesCount();
			}
			if (!sourceDisplayed && newClInfo.getSource() != null) {
				sourceDisplayed = true;
			}
			if (!deltaDisplayed && newClInfo.getBytesDelta() != 0) {
				deltaDisplayed = true;
			}
		}
		if (!jrockit) {
			sc.next("Total"); // NOI18N
			final long totalInstances = sc.nextLong();
			final long totalBytes = sc.nextLong();
			assert totalInstances == totalPermgenInstances + totalHeapInstances;
			assert totalBytes == totalPermGenBytes + totalHeapBytes;
		}
		classes = new ArrayList<ClassInfo>(classesMap.values());
		permGenClasses = new ArrayList<ClassInfo>(permGenMap.values());
		sort();
	}

	private void skipHeader(Scanner sc, boolean jrockit) {
		sc.nextLine();
		sc.nextLine();
		if (!jrockit) {
			sc.skip("-+");
			sc.nextLine();
		}
	}

	void add(HeapHistogram second) {
		final Map<String, ClassInfo> classesMap = new HashMap<String, ClassInfo>(1024);
		final Map<String, ClassInfo> permGenMap = new HashMap<String, ClassInfo>(1024);
		for (final ClassInfo classInfo : classes) {
			storeClassInfo(classInfo, classesMap);
		}
		for (final ClassInfo classInfo : permGenClasses) {
			storeClassInfo(classInfo, permGenMap);
		}
		for (final ClassInfo classInfo : second.getHeapHistogram()) {
			storeClassInfo(classInfo, classesMap);
		}
		for (final ClassInfo classInfo : second.getPermGenHistogram()) {
			storeClassInfo(classInfo, permGenMap);
		}
		totalHeapBytes += second.getTotalHeapBytes();
		totalHeapInstances += second.getTotalHeapInstances();
		totalPermGenBytes += second.getTotalPermGenBytes();
		totalPermgenInstances += second.getTotalPermGenInstances();
		classes.clear();
		classes.addAll(new ArrayList<ClassInfo>(classesMap.values()));
		permGenClasses.clear();
		permGenClasses.addAll(new ArrayList<ClassInfo>(permGenMap.values()));
		sort();
		sourceDisplayed = sourceDisplayed || second.isSourceDisplayed();
		deltaDisplayed = deltaDisplayed || second.isDeltaDisplayed();
	}

	private void sort() {
		final Comparator<ClassInfo> classInfoReversedComparator = Collections
				.reverseOrder(new ClassInfoComparator());
		Collections.sort(permGenClasses, classInfoReversedComparator);
		Collections.sort(classes, classInfoReversedComparator);
	}

	private void storeClassInfo(ClassInfo newClInfo, Map<String, ClassInfo> map) {
		final ClassInfo oldClInfo = map.get(newClInfo.getName());
		if (oldClInfo == null) {
			map.put(newClInfo.getName(), newClInfo);
		} else {
			oldClInfo.setBytes(oldClInfo.getBytes() + newClInfo.getBytes());
			oldClInfo.setBytesDelta(oldClInfo.getBytesDelta() + newClInfo.getBytesDelta());
			oldClInfo.setInstancesCount(oldClInfo.getInstancesCount()
					+ newClInfo.getInstancesCount());
		}
	}

	Date getTime() {
		return time;
	}

	List<ClassInfo> getHeapHistogram() {
		return Collections.unmodifiableList(classes);
	}

	//	long getTotalInstances() {
	//		return totalInstances;
	//	}
	//
	//	long getTotalBytes() {
	//		return totalBytes;
	//	}

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

	boolean isDeltaDisplayed() {
		return deltaDisplayed;
	}

	static final class ClassInfoComparator implements Comparator<ClassInfo>, Serializable {
		private static final long serialVersionUID = 1L;

		/** {@inheritDoc} */
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

	static class ClassInfo implements Serializable { // NOPMD
		private static final long serialVersionUID = 6283636454450216347L;
		private long instances;
		private long bytes;
		private long bytesDelta;
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
				bytesDelta = parseLongWithK(sc.next());
			} else {
				instances = sc.nextLong();
				bytes = sc.nextLong();
			}
			jvmName = sc.next();
			permGen = jvmName.charAt(0) == '<';
			name = convertJVMName();
			source = findSource();
		}

		String getName() {
			return name;
		}

		long getInstancesCount() {
			return instances;
		}

		void setInstancesCount(long instancesCount) {
			this.instances = instancesCount;
		}

		long getBytes() {
			return bytes;
		}

		void setBytes(long bytes) {
			this.bytes = bytes;
		}

		long getBytesDelta() {
			return bytesDelta;
		}

		void setBytesDelta(long bytesDelta) {
			this.bytesDelta = bytesDelta;
		}

		/** {@inheritDoc} */
		@Override
		public int hashCode() {
			return getName().hashCode();
		}

		/** {@inheritDoc} */
		@Override
		public boolean equals(Object obj) {
			if (obj instanceof ClassInfo) {
				return getName().equals(((ClassInfo) obj).getName());
			}
			return false;
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
				final CodeSource codeSource = clazz.getProtectionDomain().getCodeSource();
				if (codeSource != null && codeSource.getLocation() != null) {
					String src = codeSource.getLocation().toString();
					if (src.startsWith("file:/")) {
						src = src.substring("file:/".length());
					}
					if (src.endsWith(".jar") || src.endsWith(".war")) {
						src = src.intern();
					}
					return src;
				}
				return null;
			} catch (final ClassNotFoundException e) {
				// on suppose qu'il y a une seule webapp et que la plupart des classes peuvent être chargées
				// sinon il y a une exception et on retourne null
				return null;
			}
		}

		// CHECKSTYLE:OFF
		private String convertJVMName() { // NOPMD
			// CHECKSTYLE:ON
			String result;
			final int index = jvmName.lastIndexOf('[');

			if (index != -1) {
				switch (jvmName.charAt(index + 1)) {
				case BOOLEAN_CODE:
					result = BOOLEAN_TEXT;
					break;
				case CHAR_CODE:
					result = CHAR_TEXT;
					break;
				case BYTE_CODE:
					result = BYTE_TEXT;
					break;
				case SHORT_CODE:
					result = SHORT_TEXT;
					break;
				case INT_CODE:
					result = INT_TEXT;
					break;
				case LONG_CODE:
					result = LONG_TEXT;
					break;
				case FLOAT_CODE:
					result = FLOAT_TEXT;
					break;
				case DOUBLE_CODE:
					result = DOUBLE_TEXT;
					break;
				case OBJECT_CODE:
					result = jvmName.substring(index + 2, jvmName.length() - 1);
					break;
				default:
					//System.out.println("Uknown name " + jvmName);
					result = jvmName;
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
