package net.bull.javamelody;

import java.io.File;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import javax.management.JMException;
import javax.management.MBeanServer;
import javax.management.ObjectName;
import javax.servlet.ServletContext;

/**
 * Registers CounterRequestMXBean beans for each of the enabled counters
 * The beans are registered under "net.bull.javamelody:type=CounterRequest,context=<webapp>,name=<counter name>" names.
 *
 * @author Alexey Pushkin
 */
public class JMXExpose {
	private static Set<ObjectName> names = new HashSet<ObjectName>();

	/**
	 * Registers CounterRequestMXBean beans
	 *
	 * @param collector
	 * @param servletContext
	 */
	public static void start(Collector collector, ServletContext servletContext) {
		try {
			String webapp = new File(servletContext.getRealPath("/")).getName();
			MBeanServer platformMBeanServer = MBeans.getPlatformMBeanServer();
			List<Counter> counters = collector.getCounters();
			for (Counter counter : counters) {
				CounterRequestMXBean mxBean = new CounterRequestMXBeanImpl(counter);
				ObjectName name = new ObjectName(
						String.format("net.bull.javamelody:type=CounterRequest,context=%s,name=%s",
								webapp, counter.getName()));
				platformMBeanServer.registerMBean(mxBean, name);
				names.add(name);
			}
		} catch (JMException e) {
			LOG.warn("failed to register JMX beans", e);
		}
	}

	/**
	 * Unregisters CounterRequestMXBean beans
	 *
	 */
	public static void stop() {
		try {
			MBeanServer platformMBeanServer = MBeans.getPlatformMBeanServer();
			Iterator<ObjectName> it = names.iterator();
			while (it.hasNext()) {
				ObjectName name = it.next();
				platformMBeanServer.unregisterMBean(name);
				it.remove();
			}
		} catch (JMException e) {
			LOG.warn("failed to unregister JMX beans", e);
		}
	}
}
