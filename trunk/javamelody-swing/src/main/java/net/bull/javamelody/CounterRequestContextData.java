/*
 * Copyright 2008-2012 by Emeric Vernat
 *
 *     This file is part of Java Melody.
 *
 * Java Melody is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Java Melody is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Java Melody.  If not, see <http://www.gnu.org/licenses/>.
 */
package net.bull.javamelody;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import net.bull.javamelody.Counter.CounterRequestContextComparator;

/**
 * Données pour le tableau des requêtes courantes.
 * @author Emeric Vernat
 */
class CounterRequestContextData {
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

	CounterRequestContextData(List<Counter> counters, List<CounterRequestContext> currentRequests,
			JavaInformations javaInformations) {
		super();
		this.counters = counters;
		this.javaInformations = javaInformations;
		this.contexts = currentRequests;
		Collections.sort(contexts, Collections.reverseOrder(new CounterRequestContextComparator(
				System.currentTimeMillis())));

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
			final String requestId = new CounterRequest(context.getRequestName(), context
					.getParentCounter().getName()).getId();
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

	CounterRequestAggregation getAggregationForCounter(Counter counter) {
		CounterRequestAggregation aggregation = aggregationsByCounter.get(counter);
		if (aggregation == null) {
			aggregation = new CounterRequestAggregation(counter);
			aggregationsByCounter.put(counter, aggregation);
		}
		return aggregation;
	}

	ThreadInformations getThreadInformationsByCounterRequestContext(
			CounterRequestContext counterRequestContext) {
		if (counterRequestContext.getParentContext() == null) {
			// on affiche le thread que pour le contexte parent
			return threadInformationsById.get(counterRequestContext.getThreadId());
		}
		return null;
	}

	boolean isStackTraceEnabled() {
		return stackTraceEnabled;
	}

	boolean isChildHitsDisplayed() {
		return childHitsDisplayed;
	}

	boolean isRemoteUserDisplayed() {
		return remoteUserDisplayed;
	}

	List<CounterRequestContext> getRootContexts() {
		return contexts;
	}

	List<CounterRequestContext> getAllContexts() {
		return allContexts;
	}

	List<CounterRequest> getAllRequests() {
		return allRequests;
	}

	JavaInformations getJavaInformations() {
		return javaInformations;
	}
}
