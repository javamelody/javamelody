/*
 * Copyright 2008-2009 by Emeric Vernat, Bull
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
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import net.bull.javamelody.CounterRequest.ICounterRequestContext;


/**
 * Contexte d'une requête pour un compteur (non synchronisé).
 * Le contexte sera initialisé dans un ThreadLocal puis sera utilisé à l'enregistrement de la requête parente.
 * Par exemple, le contexte d'une requête http a zéro ou plusieurs requêtes sql.
 * @author Emeric Vernat
 */
class CounterRequestContext implements ICounterRequestContext, Cloneable {
	private static final Long ONE = 1L;
	// attention de ne pas sérialiser ce counter vers le serveur de collecte, le vrai ayant été cloné
	private final transient Counter parentCounter;
	private final CounterRequestContext parentContext;
	private CounterRequestContext currentChildContext;
	private final String requestName;
	private final String completeRequestName;
	private final String remoteUser;
	private final long threadId;
	// attention, si sérialisation vers serveur de collecte, la durée peut être impactée s'il y a désynchronisation d'horloge
	private final long startTime;
	private final long startCpuTime;
	// ces 2 champs sont initialisés à 0
	private int childHits;
	private int childDurationsSum;
	@SuppressWarnings("all")
	private Map<String, Long> childRequestsExecutionsByRequestId;

	CounterRequestContext(Counter parentCounter, CounterRequestContext parentContext,
			String requestName, String completeRequestName, String remoteUser, long startCpuTime) {
		this(parentCounter, parentContext, requestName, completeRequestName, remoteUser, Thread
				.currentThread().getId(), System.currentTimeMillis(), startCpuTime);
		if (parentContext != null) {
			parentContext.setCurrentChildContext(this);
		}
	}

	// constructeur privé pour la méthode clone
	// CHECKSTYLE:OFF
	private CounterRequestContext(Counter parentCounter, CounterRequestContext parentContext,
			String requestName, String completeRequestName, String remoteUser, long threadId,
			long startTime, long startCpuTime) {
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
		this.remoteUser = remoteUser;
		this.threadId = threadId;
		this.startTime = startTime;
		this.startCpuTime = startCpuTime;
	}

	Counter getParentCounter() {
		return parentCounter;
	}

	CounterRequestContext getParentContext() {
		return parentContext;
	}

	String getRequestName() {
		return requestName;
	}

	String getCompleteRequestName() {
		return completeRequestName;
	}

	String getRemoteUser() {
		return remoteUser;
	}

	long getThreadId() {
		return threadId;
	}

	int getDuration(long timeOfSnapshot) {
		// durée écoulée (non négative même si resynchro d'horloge)
		return (int) Math.max(timeOfSnapshot - startTime, 0);
	}

	int getCpuTime() {
		return (int) (ThreadInformations.getThreadCpuTime(getThreadId()) - startCpuTime) / 1000000;
	}

	/** {@inheritDoc} */
	public int getChildHits() {
		return childHits;
	}

	/** {@inheritDoc} */
	public int getChildDurationsSum() {
		return childDurationsSum;
	}

	/** {@inheritDoc} */
	public Map<String, Long> getChildRequestsExecutionsByRequestId() {
		if (childRequestsExecutionsByRequestId == null) {
			return Collections.emptyMap();
		}
		// pas de nouvelle instance de map ici pour raison de perf
		// (la méthode est utilisée sur un seul thread)
		return childRequestsExecutionsByRequestId;
	}

	int getTotalChildHits() {
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

	int getTotalChildDurationsSum() {
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

	List<CounterRequestContext> getChildContexts() {
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
			boolean systemError, int responseSize) {
		// si je suis le counter fils du counter du contexte parent
		// comme sql pour http alors on ajoute la requête fille
		if (parentContext != null
				&& parentCounter.getName().equals(
						parentContext.getParentCounter().getChildCounterName())) {
			childHits++;
			childDurationsSum += duration;
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
				getRequestName(), getCompleteRequestName(), getRemoteUser(), getThreadId(),
				startTime, startCpuTime);
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
