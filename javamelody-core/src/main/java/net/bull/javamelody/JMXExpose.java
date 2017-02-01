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
package net.bull.javamelody;

import java.io.File;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.management.JMException;
import javax.management.MBeanServer;
import javax.management.ObjectName;

/**
 * Registers CounterRequestMXBean beans for each of the enabled counters.
 * The beans are registered under "net.bull.javamelody:type=CounterRequest,context=<webapp>,name=<counter name>" names.
 *
 * @author Alexey Pushkin
 */
class JMXExpose {
	private static Set<ObjectName> names = new HashSet<ObjectName>();

	public static void start(Collector collector) {
		try {
			final String webapp = new File(Parameters.getServletContext().getRealPath("/"))
					.getName();
			final MBeanServer platformMBeanServer = MBeans.getPlatformMBeanServer();
			final List<Counter> counters = collector.getCounters();
			for (final Counter counter : counters) {
				final CounterRequestMXBean mxBean = new CounterRequestMXBeanImpl(counter);
				final ObjectName name = new ObjectName(
						String.format("net.bull.javamelody:type=CounterRequest,context=%s,name=%s",
								webapp, counter.getName()));
				platformMBeanServer.registerMBean(mxBean, name);
				names.add(name);
			}
		} catch (final JMException e) {
			LOG.warn("failed to register JMX beans", e);
		}
	}

	/**
	 * Unregisters CounterRequestMXBean beans
	 *
	 */
	public static void stop() {
		try {
			final MBeanServer platformMBeanServer = MBeans.getPlatformMBeanServer();
			for (final ObjectName name : names) {
				platformMBeanServer.unregisterMBean(name);
			}
		} catch (final JMException e) {
			LOG.warn("failed to unregister JMX beans", e);
		}
	}
}
