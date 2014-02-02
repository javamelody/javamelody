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

import java.io.Serializable;
import java.lang.management.ManagementFactory;
import java.lang.management.ThreadMXBean;
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
class ThreadInformations implements Serializable {
	private static final long serialVersionUID = 3604281253550723654L;
	@SuppressWarnings("all")
	private static final ThreadMXBean THREAD_BEAN = ManagementFactory.getThreadMXBean();
	private static final boolean CPU_TIME_ENABLED = THREAD_BEAN.isThreadCpuTimeSupported()
			&& THREAD_BEAN.isThreadCpuTimeEnabled();
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
	ThreadInformations(Thread thread, List<StackTraceElement> stackTrace, long cpuTimeMillis,
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

	static long getCurrentThreadCpuTime() {
		return getThreadCpuTime(Thread.currentThread().getId());
	}

	static long getThreadCpuTime(long threadId) {
		if (CPU_TIME_ENABLED) {
			// le coût de cette méthode se mesure à environ 0,6 microseconde
			return THREAD_BEAN.getThreadCpuTime(threadId);
		}
		return 0;
	}

	String getName() {
		return name;
	}

	long getId() {
		return id;
	}

	int getPriority() {
		return priority;
	}

	boolean isDaemon() {
		return daemon;
	}

	Thread.State getState() {
		return state;
	}

	List<StackTraceElement> getStackTrace() {
		if (stackTrace != null) {
			return Collections.unmodifiableList(stackTrace);
		}
		return stackTrace;
	}

	String getExecutedMethod() {
		final List<StackTraceElement> trace = stackTrace;
		if (trace != null && !trace.isEmpty()) {
			return trace.get(0).toString();
		}
		return "";
	}

	long getCpuTimeMillis() {
		return cpuTimeMillis;
	}

	long getUserTimeMillis() {
		return userTimeMillis;
	}

	boolean isDeadlocked() {
		return deadlocked;
	}

	String getGlobalThreadId() {
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
