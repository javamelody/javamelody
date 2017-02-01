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

import java.util.List;
import java.util.SortedMap;
import java.util.TreeMap;

/**
 * Used to expose CounterRequestAggregationData via JMX.
 *
 * @see CounterRequestAggregationData
 *
 * @author Alexey Pushkin
 */
public class CounterRequestAggregationData {
	private final CounterRequestData globalRequest;
	private final CounterRequestData warningRequest;
	private final CounterRequestData severeRequest;
	private final int warningThreshold;
	private final int severeThreshold;
	private final SortedMap<String, CounterRequestData> requests;

	public CounterRequestAggregationData(CounterRequestAggregation aggregation) {
		this.globalRequest = new CounterRequestData(aggregation.getGlobalRequest());
		this.warningRequest = new CounterRequestData(aggregation.getWarningRequest());
		this.severeRequest = new CounterRequestData(aggregation.getSevereRequest());
		this.warningThreshold = aggregation.getWarningThreshold();
		this.severeThreshold = aggregation.getSevereThreshold();

		this.requests = new TreeMap<String, CounterRequestData>();
		final List<CounterRequest> requestList = aggregation.getRequests();
		for (final CounterRequest request : requestList) {
			requests.put(request.getName(), new CounterRequestData(request));
		}
	}

	public CounterRequestData getGlobalRequest() {
		return globalRequest;
	}

	public CounterRequestData getWarningRequest() {
		return warningRequest;
	}

	public CounterRequestData getSevereRequest() {
		return severeRequest;
	}

	public int getWarningThreshold() {
		return warningThreshold;
	}

	public int getSevereThreshold() {
		return severeThreshold;
	}

	public SortedMap<String, CounterRequestData> getRequests() {
		return requests;
	}
}
