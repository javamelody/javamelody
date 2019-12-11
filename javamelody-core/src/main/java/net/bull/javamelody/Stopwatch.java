package net.bull.javamelody;

import net.bull.javamelody.internal.common.Parameters;
import net.bull.javamelody.internal.model.Counter;

/**
 * Stopwatch to record execution times of a piece of code in the services statistics.
 * (To monitor whole methods, see MonitoringSpringInterceptor, MonitoringInterceptor or MonitoringProxy.)
 * @author Emeric Vernat
 */
public class Stopwatch implements AutoCloseable {
	private static final Counter SERVICES_COUNTER = MonitoringProxy.getServicesCounter();
	private static final boolean COUNTER_HIDDEN = Parameters
			.isCounterHidden(SERVICES_COUNTER.getName());

	private final String name;
	private final long startTime;
	private long duration;
	private boolean closed;

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
		SERVICES_COUNTER.setDisplayed(!COUNTER_HIDDEN);
		SERVICES_COUNTER.setUsed(true);
		SERVICES_COUNTER.bindContextIncludingCpu(stopwatchName);
		this.startTime = System.currentTimeMillis();
		this.name = stopwatchName;
	}

	/**
	 * @return name of the stopwatch.
	 */
	public String getName() {
		return name;
	}

	/**
	 * @return duration until now or until stop of the stopwatch.
	 */
	public long getDuration() {
		if (closed) {
			return duration;
		}
		return System.currentTimeMillis() - startTime;
	}

	/**
	 * @return already stopped ?
	 */
	public boolean isClosed() {
		return closed;
	}

	/**
	 * Stops the stopwatch.
	 */
	@Override
	public void close() {
		if (closed) {
			throw new IllegalStateException("Stopwatch already closed");
		}
		SERVICES_COUNTER.addRequestForCurrentContext(false);
		duration = getDuration();
		closed = true;
	}
}
