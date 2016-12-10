package net.bull.javamelody;

/**
 * Used to expose CounterRequest data via JMX
 * @see CounterRequest
 *
 * @author Alexey Pushkin
 */
public class CounterRequestData {
	private CounterRequest request;

	public CounterRequestData(CounterRequest request) {
		this.request = request;
	}

	public String getName() {
		return request.getName();
	}

	public long getHits() {
		return request.getHits();
	}

	public long getDurationsSum() {
		return request.getDurationsSum();
	}

	public int getMean() {
		return request.getMean();
	}

	public int getStandardDeviation() {
		return request.getStandardDeviation();
	}

	public long getMaximum() {
		return request.getMaximum();
	}

	public long getCpuTimeSum() {
		return request.getCpuTimeSum();
	}

	public int getCpuTimeMean() {
		return request.getCpuTimeMean();
	}

	public float getSystemErrorPercentage() {
		return request.getSystemErrorPercentage();
	}

	public int getResponseSizeMean() {
		return request.getResponseSizeMean();
	}

}
