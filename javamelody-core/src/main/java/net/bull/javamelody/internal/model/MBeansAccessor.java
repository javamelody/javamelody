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

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import javax.management.JMException;
import javax.management.MBeanAttributeInfo;
import javax.management.MBeanInfo;
import javax.management.MBeanServer;
import javax.management.MalformedObjectNameException;
import javax.management.ObjectName;

/**
 * Accesseurs pour des MBeans particuliers.
 * @author Emeric Vernat
 */
final class MBeansAccessor {
	private static final MBeanServer MBEAN_SERVER = MBeans.getPlatformMBeanServer();
	private static final Set<ObjectName> NIO_BUFFER_POOLS = getNioBufferPools();
	private static final ObjectName OPERATING_SYSTEM = createObjectName(
			"java.lang:type=OperatingSystem");
	private static final Set<String> OPERATING_SYSTEM_ATTRIBUTES = getAttributesNames(
			OPERATING_SYSTEM);
	private static final ObjectName THREADING = createObjectName("java.lang:type=Threading");
	private static final boolean MBEAN_ALLOCATED_BYTES_ENABLED = getAttributesNames(THREADING)
			.contains("ThreadAllocatedMemoryEnabled");
	private static final String[] THREAD_ALLOCATED_BYTES_SIGNATURE = { long.class.getName(), };

	private MBeansAccessor() {
		super();
	}

	static Set<ObjectName> getTomcatThreadPools() {
		final Set<ObjectName> result = new HashSet<ObjectName>(
				MBEAN_SERVER.queryNames(createObjectName("*:type=ThreadPool,*"), null));
		// #843 Tomcat info is not available anymore
		result.removeAll(MBEAN_SERVER.queryNames(
				createObjectName("*:type=ThreadPool,*,subType=SocketProperties"), null));
		return result;
	}

	static Set<ObjectName> getTomcatGlobalRequestProcessors() {
		return MBEAN_SERVER.queryNames(createObjectName("*:type=GlobalRequestProcessor,*"), null);
	}

	private static Set<ObjectName> getNioBufferPools() {
		return MBEAN_SERVER.queryNames(createObjectName("java.nio:type=BufferPool,*"), null);
	}

	static long getUsedBufferMemory() {
		if (NIO_BUFFER_POOLS.isEmpty()) {
			return -1;
		}
		long result = 0;
		try {
			for (final ObjectName objectName : NIO_BUFFER_POOLS) {
				// adds direct and mapped buffers
				result += (Long) getAttribute(objectName, "MemoryUsed");
			}
		} catch (final JMException e) {
			throw new IllegalStateException(e);
		}
		return result;
	}

	// depuis jdk 9 et le module jdk.management/com.sun.management.internal,
	// on ne peut plus appeler par réflexion des getters d'OperatingSystemMXBean "Sun" :
	// https://docs.oracle.com/javase/9/docs/api/com/sun/management/OperatingSystemMXBean.html
	// (ManagementFactory.getOperatingSystemMXBean() instanceof com.sun.management.internal.OperatingSystemImpl en jdk 9)
	// car java.lang.reflect.InaccessibleObjectException:
	// Unable to make public long com.sun.management.internal.OperatingSystemImpl...... accessible:
	//	module jdk.management does not "opens com.sun.management.internal" to unnamed module ....
	// sauf si par chance, --add-opens=jdk.management/com.sun.management.internal=ALL-UNNAMED en ligne de commande
	// donc on appelle les attributs des MBeans équivalents qui sont visibles dans tous les cas
	@SuppressWarnings("unchecked")
	static <T> T getAttribute(ObjectName name, String attribute) throws JMException {
		return (T) MBEAN_SERVER.getAttribute(name, attribute);
	}

	static long getLongFromOperatingSystem(String attribute) {
		if (!OPERATING_SYSTEM_ATTRIBUTES.contains(attribute)) {
			// si on demande un attribut qui n'existe pas dans cette JVM ou dans cet OS, alors -1
			return -1L;
		}
		try {
			return getAttribute(OPERATING_SYSTEM, attribute);
		} catch (final JMException e) {
			throw new IllegalStateException(e);
		}
	}

	static double getDoubleFromOperatingSystem(String attribute) {
		if (!OPERATING_SYSTEM_ATTRIBUTES.contains(attribute)) {
			// si on demande un attribut qui n'existe pas dans cette JVM ou dans cet OS, alors -1
			return -1D;
		}
		try {
			return getAttribute(OPERATING_SYSTEM, attribute);
		} catch (final JMException e) {
			throw new IllegalStateException(e);
		}
	}

	static long getThreadAllocatedBytes(long threadId) {
		if (!MBEAN_ALLOCATED_BYTES_ENABLED) {
			return -1L;
		}
		try {
			return (Long) MBEAN_SERVER.invoke(THREADING, "getThreadAllocatedBytes",
					new Object[] { threadId }, THREAD_ALLOCATED_BYTES_SIGNATURE);
		} catch (final JMException e) {
			throw new IllegalStateException(e);
		}
	}

	static Object invoke(ObjectName name, String operationName, Object[] params, Class<?>[] classes)
			throws JMException {
		assert name != null;
		assert operationName != null;
		assert params != null;
		assert classes != null;
		final String[] signature = new String[classes.length];
		for (int i = 0; i < signature.length; i++) {
			signature[i] = classes[i].getName();
		}
		return MBEAN_SERVER.invoke(name, operationName, params, signature);
	}

	private static ObjectName createObjectName(String name) {
		try {
			return new ObjectName(name);
		} catch (final MalformedObjectNameException e) {
			// ne peut pas arriver ici vu les valeurs utilisées pour name
			throw new IllegalStateException(e);
		}
	}

	private static Set<String> getAttributesNames(ObjectName name) {
		try {
			final Set<String> result = new HashSet<String>();
			final MBeanInfo mBeanInfo = MBEAN_SERVER.getMBeanInfo(name);
			final MBeanAttributeInfo[] attributes = mBeanInfo.getAttributes();
			for (final MBeanAttributeInfo attribute : attributes) {
				if (attribute.isReadable()) {
					result.add(attribute.getName());
				}
			}
			return result;
		} catch (final JMException e) {
			return Collections.emptySet();
		}
	}
}
