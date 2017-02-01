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

/**
 * Used to expose CounterRequest data via JMX.
 * @see CounterRequest
 *
 * @author Alexey Pushkin
 */
public class CounterRequestData {
	private final CounterRequest request;

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
