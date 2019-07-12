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

/**
 * Données Real User Monitoring (<a href='https://en.wikipedia.org/wiki/Real_user_monitoring'>RUM</a>) d'une requête http.
 * @author Emeric Vernat
 */
public class CounterRequestRumData implements Serializable, Cloneable {
	private static final long serialVersionUID = 745110095604593659L;

	// au-delà de 5 minutes, on considère une valeur RUM comme aberrante et à ignorer
	private static final long ABERRANT_VALUE = 5 * 60 * 1000;

	private long hits;
	private long networkTimeSum;
	private long domProcessingSum;
	private long pageRenderingSum;

	public long getHits() {
		return hits;
	}

	public int getNetworkTimeMean() {
		if (hits > 0) {
			return (int) (networkTimeSum / hits);
		}
		return -1;
	}

	public int getDomProcessingMean() {
		if (hits > 0) {
			return (int) (domProcessingSum / hits);
		}
		return -1;
	}

	public int getPageRenderingMean() {
		if (hits > 0) {
			return (int) (pageRenderingSum / hits);
		}
		return -1;
	}

	void addHit(long networkTime, long domProcessing, long pageRendering) {
		if (networkTime < 0 || networkTime > ABERRANT_VALUE || domProcessing < 0
				|| domProcessing > ABERRANT_VALUE || pageRendering < 0
				|| pageRendering > ABERRANT_VALUE) {
			// aberrant value, we ignore it
			return;
		}

		networkTimeSum += networkTime;
		domProcessingSum += domProcessing;
		pageRenderingSum += pageRendering;
		hits++;
	}

	void addHits(CounterRequestRumData rumData) {
		if (rumData.hits != 0) {
			hits += rumData.hits;
			networkTimeSum += rumData.networkTimeSum;
			domProcessingSum += rumData.domProcessingSum;
			pageRenderingSum += rumData.pageRenderingSum;
		}
	}

	void removeHits(CounterRequestRumData rumData) {
		if (rumData.hits != 0) {
			hits -= rumData.hits;
			networkTimeSum -= rumData.networkTimeSum;
			domProcessingSum -= rumData.domProcessingSum;
			pageRenderingSum -= rumData.pageRenderingSum;
		}
	}

	/** {@inheritDoc} */
	@Override
	public CounterRequestRumData clone() { // NOPMD
		try {
			return (CounterRequestRumData) super.clone();
		} catch (final CloneNotSupportedException e) {
			// ne peut arriver puisque CounterRequest implémente Cloneable
			throw new IllegalStateException(e);
		}
	}

	/** {@inheritDoc} */
	@Override
	public String toString() {
		return getClass().getSimpleName() + "[hits=" + hits + ']';
	}
}
