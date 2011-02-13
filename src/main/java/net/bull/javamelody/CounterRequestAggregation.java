/*
 * Copyright 2008-2010 by Emeric Vernat
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

import net.bull.javamelody.Counter.CounterRequestComparator;

/**
 * Agrégation des requêtes d'un compteur pour l'affichage d'une synthèse.
 * @author Emeric Vernat
 */
class CounterRequestAggregation {
	private final Counter counter;
	private final List<CounterRequest> requests;
	private final CounterRequest globalRequest;
	private final int warningThreshold;
	private final int severeThreshold;
	private final boolean responseSizeDisplayed;
	private final boolean childHitsDisplayed;
	private final boolean timesDisplayed;
	private final boolean cpuTimesDisplayed;
	private final CounterRequest warningRequest;
	private final CounterRequest severeRequest;

	CounterRequestAggregation(Counter counter) {
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
		this.responseSizeDisplayed = globalRequest.getResponseSizeMean() >= 0;
		this.childHitsDisplayed = globalRequest.hasChildHits();
		this.timesDisplayed = globalRequest.getMean() >= 0;
		this.cpuTimesDisplayed = globalRequest.getCpuTimeMean() >= 0;

		// globalMean et globalStandardDeviation sont utilisées pour déterminer
		// les seuils des couleurs des moyennes dans le tableau quand les paramètres
		// warning-threshold-millis et severe-threshold-millis ne sont pas définis
		final int globalMean = globalRequest.getMean();
		final int globalStandardDeviation = globalRequest.getStandardDeviation();
		this.warningThreshold = getThreshold(Parameter.WARNING_THRESHOLD_MILLIS, globalMean
				+ globalStandardDeviation);
		this.severeThreshold = getThreshold(Parameter.SEVERE_THRESHOLD_MILLIS, globalMean + 2
				* globalStandardDeviation);

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
		final String param = Parameters.getParameter(parameter);
		if (param == null) {
			return defaultValue;
		}
		final int threshold = Integer.parseInt(param);
		if (threshold <= 0) {
			throw new IllegalStateException("Le paramètre " + parameter.getCode()
					+ " doit être > 0");
		}
		return threshold;
	}

	List<CounterRequest> getRequests() {
		return requests;
	}

	CounterRequest getGlobalRequest() {
		return globalRequest;
	}

	CounterRequest getWarningRequest() {
		return warningRequest;
	}

	CounterRequest getSevereRequest() {
		return severeRequest;
	}

	int getWarningThreshold() {
		return warningThreshold;
	}

	int getSevereThreshold() {
		return severeThreshold;
	}

	boolean isResponseSizeDisplayed() {
		return responseSizeDisplayed;
	}

	boolean isChildHitsDisplayed() {
		return childHitsDisplayed;
	}

	boolean isTimesDisplayed() {
		return timesDisplayed;
	}

	boolean isCpuTimesDisplayed() {
		return cpuTimesDisplayed;
	}

	List<CounterRequest> getRequestsAggregatedByClassName() {
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

	List<CounterRequest> getRequestsFilteredByClassName(String className) {
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

	private String getClassNameFromRequest(CounterRequest request) {
		final int lastIndexOf = request.getName().lastIndexOf('.');
		if (lastIndexOf != -1) {
			return request.getName().substring(0, lastIndexOf);
		}
		return request.getName();
	}
}
