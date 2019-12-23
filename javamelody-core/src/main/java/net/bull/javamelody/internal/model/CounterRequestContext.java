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

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import net.bull.javamelody.internal.model.CounterRequest.ICounterRequestContext;

/**
 * Contexte d'une requête pour un compteur (non synchronisé).
 * Le contexte sera initialisé dans un ThreadLocal puis sera utilisé à l'enregistrement de la requête parente.
 * Par exemple, le contexte d'une requête http a zéro ou plusieurs requêtes sql.
 * @author Emeric Vernat
 */
public class CounterRequestContext implements ICounterRequestContext, Cloneable, Serializable {
	private static final long serialVersionUID = 1L;
	private static final Long ONE = 1L;
	private static final String SPRING_BEST_MATCHING_PATTERN_ATTRIBUTE = "org.springframework.web.servlet.HandlerMapping.bestMatchingPattern";
	// attention de ne pas sérialiser le counter d'origine vers le serveur de collecte, le vrai ayant été cloné
	private Counter parentCounter;
	private final CounterRequestContext parentContext;
	private CounterRequestContext currentChildContext;
	private final String requestName;
	private final String completeRequestName;
	private final transient HttpServletRequest httpRequest;
	private final String remoteUser;
	private final long threadId;
	// attention, si sérialisation vers serveur de collecte, la durée peut être impactée s'il y a désynchronisation d'horloge
	private final long startTime;
	private final long startCpuTime;
	private final long startAllocatedBytes;
	private final String sessionId;
	// ces 2 champs sont initialisés à 0
	private int childHits;
	private int childDurationsSum;
	@SuppressWarnings("all")
	private Map<String, Long> childRequestsExecutionsByRequestId;

	// CHECKSTYLE:OFF
	public CounterRequestContext(Counter parentCounter, CounterRequestContext parentContext,
			String requestName, String completeRequestName, HttpServletRequest httpRequest,
			String remoteUser, long startCpuTime, long startAllocatedBytes, String sessionId) {
		// CHECKSTYLE:ON
		this(parentCounter, parentContext, requestName, completeRequestName, httpRequest,
				remoteUser, Thread.currentThread().getId(), System.currentTimeMillis(),
				startCpuTime, startAllocatedBytes, sessionId);
		if (parentContext != null) {
			parentContext.setCurrentChildContext(this);
		}
	}

	// constructeur privé pour la méthode clone
	// CHECKSTYLE:OFF
	private CounterRequestContext(Counter parentCounter, CounterRequestContext parentContext, // NOPMD
			String requestName, String completeRequestName, HttpServletRequest httpRequest,
			String remoteUser, long threadId, long startTime, long startCpuTime,
			long startAllocatedBytes, String sessionId) {
		// CHECKSTYLE:ON
		super();
		assert parentCounter != null;
		assert requestName != null;
		assert completeRequestName != null;
		this.parentCounter = parentCounter;
		// parentContext est non null si on a ejb dans http
		// et il est null pour http ou pour ejb sans http
		this.parentContext = parentContext;
		this.requestName = requestName;
		this.completeRequestName = completeRequestName;
		this.httpRequest = httpRequest;
		this.remoteUser = remoteUser;
		this.threadId = threadId;
		this.startTime = startTime;
		this.startCpuTime = startCpuTime;
		this.startAllocatedBytes = startAllocatedBytes;
		this.sessionId = sessionId;
	}

	public Counter getParentCounter() {
		return parentCounter;
	}

	void setParentCounter(Counter parentCounter) {
		assert parentCounter != null
				&& this.parentCounter.getName().equals(parentCounter.getName());
		this.parentCounter = parentCounter;
	}

	public static void replaceParentCounters(List<CounterRequestContext> rootCurrentContexts,
			List<Counter> newParentCounters) {
		final Map<String, Counter> newParentCountersByName = new HashMap<String, Counter>(
				newParentCounters.size());
		for (final Counter counter : newParentCounters) {
			newParentCountersByName.put(counter.getName(), counter);
		}
		replaceParentCounters(rootCurrentContexts, newParentCountersByName);
	}

	private static void replaceParentCounters(List<CounterRequestContext> rootCurrentContexts,
			Map<String, Counter> newParentCountersByName) {
		for (final CounterRequestContext context : rootCurrentContexts) {
			final Counter newParentCounter = newParentCountersByName
					.get(context.getParentCounter().getName());
			if (newParentCounter != null) {
				// si le counter n'est pas/plus affiché, newParentCounter peut être null
				context.setParentCounter(newParentCounter);
			}
			final List<CounterRequestContext> childContexts = context.getChildContexts();
			if (!childContexts.isEmpty()) {
				replaceParentCounters(childContexts, newParentCountersByName);
			}
		}
	}

	public CounterRequestContext getParentContext() {
		return parentContext;
	}

	public static String getHttpRequestName(HttpServletRequest httpRequest, String requestName) {
		if (httpRequest == null) {
			return requestName;
		}
		final String bestMatchingPattern = (String) httpRequest
				.getAttribute(SPRING_BEST_MATCHING_PATTERN_ATTRIBUTE);
		if (bestMatchingPattern != null) {
			final int indexOfSpace = requestName.indexOf(' ');
			if (indexOfSpace != -1) {
				// ajoute GET ou POST ou POST ajax ou autre après bestMatchingPattern
				return bestMatchingPattern + requestName.substring(indexOfSpace);
			}
			return bestMatchingPattern;
		}
		return requestName;
	}

	public String getRequestName() {
		return getHttpRequestName(httpRequest, requestName);
	}

	public String getCompleteRequestName() {
		return completeRequestName;
	}

	public String getRemoteUser() {
		return remoteUser;
	}

	public long getThreadId() {
		return threadId;
	}

	public int getDuration(long timeOfSnapshot) {
		// durée écoulée (non négative même si resynchro d'horloge)
		return (int) Math.max(timeOfSnapshot - startTime, 0);
	}

	public int getCpuTime() {
		if (startCpuTime < 0) {
			return -1;
		}
		final int cpuTime = (int) ((ThreadInformations.getThreadCpuTime(getThreadId())
				- startCpuTime) / 1000000L);
		// pas de négatif ici sinon on peut avoir une assertion si elles sont activées
		return Math.max(cpuTime, 0);
	}

	public int getAllocatedKBytes() {
		if (startAllocatedBytes < 0) {
			return -1;
		}
		final int allocatedKBytes = (int) ((ThreadInformations
				.getThreadAllocatedBytes(getThreadId()) - startAllocatedBytes) / 1024L);
		return Math.max(allocatedKBytes, 0);
	}

	/** {@inheritDoc} */
	@Override
	public int getChildHits() {
		return childHits;
	}

	/** {@inheritDoc} */
	@Override
	public int getChildDurationsSum() {
		return childDurationsSum;
	}

	public Map<String, Long> getChildRequestsExecutionsByRequestId() {
		if (childRequestsExecutionsByRequestId == null) {
			return Collections.emptyMap();
		}
		// pas de nouvelle instance de map ici pour raison de perf
		// (la méthode est utilisée sur un seul thread)
		return childRequestsExecutionsByRequestId;
	}

	public int getTotalChildHits() {
		// childHits de ce contexte plus tous ceux des contextes fils,
		// il vaut mieux appeler cette méthode sur un clone du contexte pour avoir un résultat stable
		// puisque les contextes fils des requêtes en cours peuvent changer à tout moment
		int result = getChildHits();
		CounterRequestContext childContext = getCurrentChildContext();
		while (childContext != null) {
			result += childContext.getChildHits();
			childContext = childContext.getCurrentChildContext();
		}
		return result;
	}

	public int getTotalChildDurationsSum() {
		// childDurationsSum de ce contexte plus tous ceux des contextes fils,
		// il vaut mieux appeler cette méthode sur un clone du contexte pour avoir un résultat stable
		// puisque les contextes fils des requêtes en cours peuvent changer à tout moment
		int result = getChildDurationsSum();
		CounterRequestContext childContext = getCurrentChildContext();
		while (childContext != null) {
			result += childContext.getChildDurationsSum();
			childContext = childContext.getCurrentChildContext();
		}
		return result;
	}

	public boolean hasChildHits() {
		return parentCounter.getChildCounterName() != null
				&& (getTotalChildHits() > 0 || parentCounter.hasChildHits());
	}

	public List<CounterRequestContext> getChildContexts() {
		// il vaut mieux appeler cette méthode sur un clone du contexte pour avoir un résultat stable
		// puisque les contextes fils des requêtes en cours peuvent changer à tout moment
		final List<CounterRequestContext> childContexts;
		CounterRequestContext childContext = getCurrentChildContext();
		if (childContext == null) {
			childContexts = Collections.emptyList();
		} else {
			childContexts = new ArrayList<CounterRequestContext>(2);
		}
		while (childContext != null) {
			childContexts.add(childContext);
			childContext = childContext.getCurrentChildContext();
		}
		return Collections.unmodifiableList(childContexts);
	}

	private CounterRequestContext getCurrentChildContext() {
		return currentChildContext;
	}

	private void setCurrentChildContext(CounterRequestContext currentChildContext) {
		this.currentChildContext = currentChildContext;
	}

	@SuppressWarnings("unused")
	void addChildRequest(Counter childCounter, String request, String requestId, long duration,
			boolean systemError, long responseSize) {
		// si je suis le counter fils du counter du contexte parent
		// comme sql pour http alors on ajoute la requête fille
		if (parentContext != null && parentCounter.getName()
				.equals(parentContext.getParentCounter().getChildCounterName())) {
			childHits++;
			childDurationsSum += (int) duration;
		}

		// pour drill-down on conserve pour chaque requête mère, les requêtes filles appelées et le
		// nombre d'exécutions pour chacune
		if (parentContext == null) {
			addChildRequestForDrillDown(requestId);
		} else {
			parentContext.addChildRequestForDrillDown(requestId);
		}
	}

	private void addChildRequestForDrillDown(String requestId) {
		if (childRequestsExecutionsByRequestId == null) {
			childRequestsExecutionsByRequestId = new LinkedHashMap<String, Long>();
		}
		Long nbExecutions = childRequestsExecutionsByRequestId.get(requestId);
		if (nbExecutions == null) {
			nbExecutions = ONE;
		} else {
			nbExecutions += 1;
		}
		childRequestsExecutionsByRequestId.put(requestId, nbExecutions);
	}

	void closeChildContext() {
		final CounterRequestContext childContext = getCurrentChildContext();
		childHits += childContext.getChildHits();
		childDurationsSum += childContext.getChildDurationsSum();
		// ce contexte fils est terminé
		setCurrentChildContext(null);
	}

	/** {@inheritDoc} */
	@Override
	//CHECKSTYLE:OFF
	public CounterRequestContext clone() { // NOPMD
		//CHECKSTYLE:ON
		// ce clone n'est valide que pour un contexte root sans parent
		assert getParentContext() == null;
		return clone(null);
	}

	private CounterRequestContext clone(CounterRequestContext parentContextClone) {
		final Counter counter = getParentCounter();
		// s'il fallait un clone du parentCounter pour sérialiser, on pourrait faire seulement ça:
		//		final Counter parentCounterClone = new Counter(counter.getName(), counter.getStorageName(),
		//				counter.getIconName(), counter.getChildCounterName(), null);
		final CounterRequestContext clone = new CounterRequestContext(counter, parentContextClone,
				getRequestName(), getCompleteRequestName(), httpRequest, getRemoteUser(),
				getThreadId(), startTime, startCpuTime, startAllocatedBytes, sessionId);
		clone.childHits = getChildHits();
		clone.childDurationsSum = getChildDurationsSum();
		final CounterRequestContext childContext = getCurrentChildContext();
		if (childContext != null) {
			clone.currentChildContext = childContext.clone(clone);
		}
		if (childRequestsExecutionsByRequestId != null) {
			clone.childRequestsExecutionsByRequestId = new LinkedHashMap<String, Long>(
					childRequestsExecutionsByRequestId);
		}
		return clone;
	}

	/** {@inheritDoc} */
	@Override
	public String toString() {
		return getClass().getSimpleName() + "[parentCounter=" + getParentCounter().getName()
				+ ", completeRequestName=" + getCompleteRequestName() + ", threadId="
				+ getThreadId() + ", startTime=" + startTime + ", childHits=" + getChildHits()
				+ ", childDurationsSum=" + getChildDurationsSum() + ", childContexts="
				+ getChildContexts() + ']';
	}
}
