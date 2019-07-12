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

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import net.bull.javamelody.internal.model.Counter.CounterRequestContextComparator;

/**
 * Données pour le tableau des requêtes courantes.
 * @author Emeric Vernat
 */
public class CounterRequestContextData {
	private final List<Counter> counters;

	private final JavaInformations javaInformations;

	private final Map<Long, ThreadInformations> threadInformationsById;

	private final List<CounterRequestContext> contexts;

	private final List<CounterRequestContext> allContexts;

	private final List<CounterRequest> allRequests;

	private final Map<Counter, CounterRequestAggregation> aggregationsByCounter = new HashMap<>();

	private final boolean stackTraceEnabled;

	private final boolean childHitsDisplayed;

	private final boolean remoteUserDisplayed;

	public CounterRequestContextData(List<Counter> counters,
			List<CounterRequestContext> currentRequests, JavaInformations javaInformations) {
		super();
		this.counters = counters;
		this.javaInformations = javaInformations;
		this.contexts = currentRequests;
		Collections.sort(contexts, Collections
				.reverseOrder(new CounterRequestContextComparator(System.currentTimeMillis())));

		this.threadInformationsById = new HashMap<>();
		this.allContexts = new ArrayList<>();
		this.allRequests = new ArrayList<>();

		for (final ThreadInformations threadInformations : javaInformations
				.getThreadInformationsList()) {
			threadInformationsById.put(threadInformations.getId(), threadInformations);
		}
		for (final CounterRequestContext context : contexts) {
			allContexts.add(context);
			for (final CounterRequestContext childContext : context.getChildContexts()) {
				allContexts.add(childContext);
			}
		}
		final Map<String, CounterRequest> requestsById = mapAllRequestsById();
		for (final CounterRequestContext context : allContexts) {
			final String requestId = new CounterRequest(context.getRequestName(),
					context.getParentCounter().getName()).getId();
			final CounterRequest request = requestsById.get(requestId);
			allRequests.add(request);
		}

		this.stackTraceEnabled = javaInformations.isStackTraceEnabled();

		boolean myChildHitsDisplayed = false;
		for (final CounterRequestContext rootCurrentContext : getRootContexts()) {
			if (rootCurrentContext.hasChildHits()) {
				// one root has child
				myChildHitsDisplayed = true;
				break;
			}
		}
		this.childHitsDisplayed = myChildHitsDisplayed;

		boolean myRemoteUserDisplayed = false;
		for (final CounterRequestContext context : getRootContexts()) {
			if (context.getRemoteUser() != null) {
				myRemoteUserDisplayed = true;
				break;
			}
		}
		this.remoteUserDisplayed = myRemoteUserDisplayed;
	}

	private Map<String, CounterRequest> mapAllRequestsById() {
		final Map<String, CounterRequest> result = new HashMap<>();
		for (final Counter counter : counters) {
			for (final CounterRequest counterRequest : counter.getRequests()) {
				result.put(counterRequest.getId(), counterRequest);
			}
		}
		return result;
	}

	public CounterRequestAggregation getAggregationForCounter(Counter counter) {
		CounterRequestAggregation aggregation = aggregationsByCounter.get(counter);
		if (aggregation == null) {
			aggregation = new CounterRequestAggregation(counter);
			aggregationsByCounter.put(counter, aggregation);
		}
		return aggregation;
	}

	public ThreadInformations getThreadInformationsByCounterRequestContext(
			CounterRequestContext counterRequestContext) {
		if (counterRequestContext.getParentContext() == null) {
			// on affiche le thread que pour le contexte parent
			return threadInformationsById.get(counterRequestContext.getThreadId());
		}
		return null;
	}

	public boolean isStackTraceEnabled() {
		return stackTraceEnabled;
	}

	public boolean isChildHitsDisplayed() {
		return childHitsDisplayed;
	}

	public boolean isRemoteUserDisplayed() {
		return remoteUserDisplayed;
	}

	public final List<CounterRequestContext> getRootContexts() {
		return contexts;
	}

	public List<CounterRequestContext> getAllContexts() {
		return allContexts;
	}

	public List<CounterRequest> getAllRequests() {
		return allRequests;
	}

	JavaInformations getJavaInformations() {
		return javaInformations;
	}
}
