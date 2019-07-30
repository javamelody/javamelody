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

import net.bull.javamelody.Parameter;
import net.bull.javamelody.internal.model.Counter.CounterRequestComparator;

/**
 * Agrégation des requêtes d'un compteur pour l'affichage d'une synthèse.
 * @author Emeric Vernat
 */
public class CounterRequestAggregation {
	private final Counter counter;
	private final List<CounterRequest> requests;
	private final CounterRequest globalRequest;
	private final int warningThreshold;
	private final int severeThreshold;
	private final boolean responseSizeDisplayed;
	private final boolean childHitsDisplayed;
	private final boolean timesDisplayed;
	private final boolean cpuTimesDisplayed;
	private final boolean allocatedKBytesDisplayed;
	private final CounterRequest warningRequest;
	private final CounterRequest severeRequest;

	public CounterRequestAggregation(Counter counter) {
		super();
		assert counter != null;
		this.counter = counter;
		if (counter.isErrorCounter()) {
			this.requests = counter.getOrderedByHitsRequests();
		} else {
			this.requests = counter.getOrderedRequests();
		}
		assert requests != null;

		final String counterName = counter.getName();
		this.globalRequest = new CounterRequest(counterName + " global", counterName);
		for (final CounterRequest request : requests) {
			// ici, pas besoin de synchronized sur request puisque ce sont des clones indépendants
			globalRequest.addHits(request);
		}

		// on n'affiche pas la colonne "Taille de réponse" si elle est négative car non défini
		// (pour les requêtes sql par exemple)
		this.responseSizeDisplayed = globalRequest.getResponseSizeMean() >= 0L;
		this.childHitsDisplayed = globalRequest.hasChildHits();
		this.timesDisplayed = globalRequest.getMean() >= 0;
		this.cpuTimesDisplayed = globalRequest.getCpuTimeMean() >= 0;
		this.allocatedKBytesDisplayed = globalRequest.getAllocatedKBytesMean() >= 0;

		// globalMean et globalStandardDeviation sont utilisées pour déterminer
		// les seuils des couleurs des moyennes dans le tableau quand les paramètres
		// warning-threshold-millis et severe-threshold-millis ne sont pas définis
		final int globalMean = globalRequest.getMean();
		final int globalStandardDeviation = globalRequest.getStandardDeviation();
		this.warningThreshold = getThreshold(Parameter.WARNING_THRESHOLD_MILLIS,
				globalMean + globalStandardDeviation);
		this.severeThreshold = getThreshold(Parameter.SEVERE_THRESHOLD_MILLIS,
				globalMean + 2 * globalStandardDeviation);

		// synthèse globale avec requêtes global, warning et severe
		// on calcule les pourcentages de requêtes dont les temps (moyens!) dépassent les 2 seuils
		this.warningRequest = new CounterRequest(counterName + " warning", counterName);
		this.severeRequest = new CounterRequest(counterName + " severe", counterName);
		for (final CounterRequest request : requests) {
			// ici, pas besoin de synchronized sur request puisque ce sont des clones indépendants
			final int mean = request.getMean();
			if (mean > severeThreshold) {
				severeRequest.addHits(request);
			} else if (mean > warningThreshold) {
				warningRequest.addHits(request);
			}
			// les requêtes sous warning ne sont pas décomptées dans la synthèse autrement que dans global
		}
	}

	private static int getThreshold(Parameter parameter, int defaultValue) {
		final String param = parameter.getValue();
		if (param == null) {
			return defaultValue;
		}
		final int threshold = Integer.parseInt(param);
		if (threshold <= 0) {
			throw new IllegalStateException(
					"Le paramètre " + parameter.getCode() + " doit être > 0");
		}
		return threshold;
	}

	public List<CounterRequest> getRequests() {
		return requests;
	}

	public CounterRequest getGlobalRequest() {
		return globalRequest;
	}

	public CounterRequest getWarningRequest() {
		return warningRequest;
	}

	public CounterRequest getSevereRequest() {
		return severeRequest;
	}

	public int getWarningThreshold() {
		return warningThreshold;
	}

	public int getSevereThreshold() {
		return severeThreshold;
	}

	public boolean isResponseSizeDisplayed() {
		return responseSizeDisplayed;
	}

	public boolean isChildHitsDisplayed() {
		return childHitsDisplayed;
	}

	public boolean isTimesDisplayed() {
		return timesDisplayed;
	}

	public boolean isCpuTimesDisplayed() {
		return cpuTimesDisplayed;
	}

	public boolean isAllocatedKBytesDisplayed() {
		return allocatedKBytesDisplayed;
	}

	public List<CounterRequest> getRequestsAggregatedOrFilteredByClassName(String requestId) {
		final List<CounterRequest> requestsAggregatedByClassName = getRequestsAggregatedByClassName();
		final List<CounterRequest> requestList;
		if (requestId == null) {
			// on va afficher la liste des requêtes aggrégées par classe
			requestList = requestsAggregatedByClassName;
		} else {
			// on a un paramètre requestId, ie que l'utilisateur a cliqué sur un lien de détail
			// des requêtes pour une classe, et on va afficher la liste des requêtes non aggrégées
			// mais filtrées pour cette classe
			requestList = new ArrayList<CounterRequest>();
			// on recherche d'abord le nom de la classe à partir de requestId
			for (final CounterRequest requestAggregated : requestsAggregatedByClassName) {
				if (requestId.equals(requestAggregated.getId())) {
					final String className = requestAggregated.getName();
					// et on filtre les requêtes pour cette classe
					requestList.addAll(getRequestsFilteredByClassName(className));
					break;
				}
			}
		}
		return Collections.unmodifiableList(requestList);
	}

	private List<CounterRequest> getRequestsAggregatedByClassName() {
		assert counter.isBusinessFacadeCounter();
		final Map<String, CounterRequest> requestMap = new HashMap<String, CounterRequest>();
		final String counterName = counter.getName();
		for (final CounterRequest request : getRequests()) {
			final String className = getClassNameFromRequest(request);
			CounterRequest global = requestMap.get(className);
			if (global == null) {
				global = new CounterRequest(className, counterName);
				requestMap.put(className, global);
			}
			global.addHits(request);
		}
		// on trie par la somme des durées
		final List<CounterRequest> requestList = new ArrayList<CounterRequest>(requestMap.values());
		if (requestList.size() > 1) {
			Collections.sort(requestList, Collections.reverseOrder(new CounterRequestComparator()));
		}
		return requestList;
	}

	private List<CounterRequest> getRequestsFilteredByClassName(String className) {
		assert counter.isBusinessFacadeCounter();
		assert className != null;
		final List<CounterRequest> requestList = new ArrayList<CounterRequest>();
		for (final CounterRequest request : getRequests()) {
			final String requestClassName = getClassNameFromRequest(request);
			if (className.equals(requestClassName)) {
				requestList.add(request);
			}
		}
		if (requestList.size() > 1) {
			Collections.sort(requestList, Collections.reverseOrder(new CounterRequestComparator()));
		}
		return requestList;
	}

	private static String getClassNameFromRequest(CounterRequest request) {
		final int lastIndexOf = request.getName().lastIndexOf('.');
		if (lastIndexOf != -1) {
			return request.getName().substring(0, lastIndexOf);
		}
		return request.getName();
	}
}
