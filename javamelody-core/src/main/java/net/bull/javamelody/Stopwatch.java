package net.bull.javamelody;

import net.bull.javamelody.internal.model.Counter;

/**
 * Stopwatch to record execution times of a piece of code in the services statistics.
 * (To monitor whole methods, see MonitoringSpringInterceptor, MonitoringInterceptor or MonitoringProxy.)
 * @author Emeric Vernat
 */
public class Stopwatch implements AutoCloseable {
	private static final Counter SERVICES_COUNTER = MonitoringProxy.getServicesCounter();

	/**
	 * Starts a stopwatch (must always be used in try-with-resource):
	 * <pre>
	 * try (Stopwatch stopwatch = new Stopwatch("nameyouwant")) {
	 *     // your code
	 * }
	 * </pre>
	 * @param stopwatchName Whatever name you want to display in the statistics
	 */
	public Stopwatch(String stopwatchName) {
		super();
		SERVICES_COUNTER.bindContextIncludingCpu(stopwatchName);
	}

	/**
	 * Stops the stopwatch.
	 */
	@Override
	public void close() throws Exception {
		SERVICES_COUNTER.addRequestForCurrentContext(false);
	}
}
