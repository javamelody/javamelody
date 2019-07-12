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
import java.lang.management.ManagementFactory;
import java.lang.management.ThreadMXBean;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Collections;
import java.util.List;

/**
 * Informations sur un thread java, sans code html de présentation.
 * L'état d'une instance est initialisé à son instanciation et non mutable;
 * il est donc de fait thread-safe.
 * Cet état est celui d'un thread java à un instant t.
 * Les instances sont sérialisables pour pouvoir être transmises au serveur de collecte.
 * @author Emeric Vernat
 */
public class ThreadInformations implements Serializable {
	private static final long serialVersionUID = 3604281253550723654L;
	@SuppressWarnings("all")
	private static final ThreadMXBean THREAD_BEAN = ManagementFactory.getThreadMXBean();
	private static final boolean CPU_TIME_ENABLED = THREAD_BEAN.isThreadCpuTimeSupported()
			&& THREAD_BEAN.isThreadCpuTimeEnabled();
	private static final Method THREAD_ALLOCATED_BYTES_METHOD = getThreadAllocatedBytesMethod();
	private final String name;
	private final long id;
	private final int priority;
	private final boolean daemon;
	private final Thread.State state;
	private final long cpuTimeMillis;
	private final long userTimeMillis;
	private final boolean deadlocked;
	private final String globalThreadId;
	@SuppressWarnings("all")
	private final List<StackTraceElement> stackTrace;

	@SuppressWarnings("all")
	public ThreadInformations(Thread thread, List<StackTraceElement> stackTrace, long cpuTimeMillis,
			long userTimeMillis, boolean deadlocked, String hostAddress) {
		super();
		assert thread != null;
		assert stackTrace == null || stackTrace instanceof Serializable;

		this.name = thread.getName();
		this.id = thread.getId();
		this.priority = thread.getPriority();
		this.daemon = thread.isDaemon();
		this.state = thread.getState();
		this.stackTrace = stackTrace;
		this.cpuTimeMillis = cpuTimeMillis;
		this.userTimeMillis = userTimeMillis;
		this.deadlocked = deadlocked;
		this.globalThreadId = buildGlobalThreadId(thread, hostAddress);
	}

	public static long getCurrentThreadCpuTime() {
		return getThreadCpuTime(Thread.currentThread().getId());
	}

	static long getThreadCpuTime(long threadId) {
		if (CPU_TIME_ENABLED) {
			// le coût de cette méthode se mesure à environ 0,6 microseconde
			return THREAD_BEAN.getThreadCpuTime(threadId);
		}
		return 0;
	}

	public static long getCurrentThreadAllocatedBytes() {
		return getThreadAllocatedBytes(Thread.currentThread().getId());
	}

	static long getThreadAllocatedBytes(long threadId) {
		// quand disponible (pas en jdk 9+), l'appel par réflexion est de l'ordre de 0,10 microseconde
		// au lieu de 0,45 microseconde pour l'appel par MBeans
		if (THREAD_ALLOCATED_BYTES_METHOD != null) {
			try {
				return (Long) THREAD_ALLOCATED_BYTES_METHOD.invoke(THREAD_BEAN, threadId);
			} catch (final IllegalAccessException e) {
				throw new IllegalArgumentException(e);
			} catch (final InvocationTargetException e) {
				throw new IllegalArgumentException(e);
			}
		}
		return MBeansAccessor.getThreadAllocatedBytes(threadId);
	}

	private static Method getThreadAllocatedBytesMethod() {
		// en général, THREAD_BEAN instanceof com.sun.management.ThreadMXBean, sauf sur JVM tierces
		try {
			final Class<? extends ThreadMXBean> clazz = THREAD_BEAN.getClass();
			final Method method = clazz.getMethod("getThreadAllocatedBytes", long.class);
			if (method != null) {
				method.setAccessible(true);
				// on teste pour vérifier que la fonction est supportée et activée
				final Long bytes = (Long) method.invoke(THREAD_BEAN,
						Thread.currentThread().getId());
				if (bytes.longValue() != -1) {
					return method;
				}
			}
			return null;
		} catch (final IllegalAccessException e) {
			return null;
		} catch (final InvocationTargetException e) {
			return null;
		} catch (final NoSuchMethodException e) {
			return null;
		} catch (final SecurityException e) {
			return null;
		} catch (final Exception e) {
			// pour java 9 car java.lang.reflect.InaccessibleObjectException:
			// Unable to make public long com.sun.management.internal.HotSpotThreadImpl.getThreadAllocatedBytes(long) accessible:
			// module jdk.management does not "exports com.sun.management.internal" to unnamed module
			return null;
		}
	}

	public String getName() {
		return name;
	}

	public long getId() {
		return id;
	}

	public int getPriority() {
		return priority;
	}

	public boolean isDaemon() {
		return daemon;
	}

	public Thread.State getState() {
		return state;
	}

	public List<StackTraceElement> getStackTrace() {
		if (stackTrace != null) {
			return Collections.unmodifiableList(stackTrace);
		}
		return stackTrace;
	}

	public String getExecutedMethod() {
		final List<StackTraceElement> trace = stackTrace;
		if (trace != null && !trace.isEmpty()) {
			return trace.get(0).toString();
		}
		return "";
	}

	public long getCpuTimeMillis() {
		return cpuTimeMillis;
	}

	public long getUserTimeMillis() {
		return userTimeMillis;
	}

	public boolean isDeadlocked() {
		return deadlocked;
	}

	public String getGlobalThreadId() {
		return globalThreadId;
	}

	private static String buildGlobalThreadId(Thread thread, String hostAddress) {
		return PID.getPID() + '_' + hostAddress + '_' + thread.getId();
	}

	/** {@inheritDoc} */
	@Override
	public String toString() {
		return getClass().getSimpleName() + "[id=" + getId() + ", name=" + getName() + ", daemon="
				+ isDaemon() + ", priority=" + getPriority() + ", deadlocked=" + isDeadlocked()
				+ ", state=" + getState() + ']';
	}
}
