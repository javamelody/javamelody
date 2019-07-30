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
package net.bull.javamelody.internal.web;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.Collection;

import net.bull.javamelody.PrometheusController;
import net.bull.javamelody.internal.model.Collector;
import net.bull.javamelody.internal.model.JRobin;

/**
 *  This controller provides the `lastValue` metrics which also be exported by adding the http parameter includeLastValue=true.
 *  Note: the `lastValue` metrics are already aggregated over time, where Prometheus prefers the raw counters and gauges.
 *  Also, obtaining the `lastValue` metrics appears to have a 5-10ms overhead.
 *
 *  The `lastValue` metrics are DISABLED by default.
 *  
 *  See {@link PrometheusController} for more details.
 *
 * @author https://github.com/slynn1324, Stefan Penndorf, Emeric Vernat
 */
class LastValuePrometheusController extends PrometheusController {

	private static final String METRIC_PREFIX = "javamelody_last_value_";

	private final Collector collector;

	LastValuePrometheusController(Collector collector, PrintWriter out) {
		super(out, METRIC_PREFIX);
		assert collector != null;
		assert out != null;
		this.collector = collector;
	}

	/**
	 * Includes the traditional 'graph' fields from the 'lastValue' API.
	 *
	 * These fields are summary fields aggregated over `javamelody.resolutions-seconds` (default 60), which
	 * is normally an odd thing to pass to Prometheus.  Most (all?) of these can be calculated inside
	 * Prometheus from the Collector stats.
	 *
	 * Note: This lookup seems to take the longest execution time -- 5-10ms per request due to JRobin reads.
	 *
	 * Disabled by default.  To enable set the 'prometheus-include-last-value' property to 'true'.
	 *
	 * @throws IOException e
	 */
	@Override
	public void report() throws IOException {
		Collection<JRobin> jrobins = collector.getDisplayedCounterJRobins();
		for (final JRobin jrobin : jrobins) {
			printDouble(MetricType.GAUGE, camelToSnake(jrobin.getName()),
					"javamelody value per minute", jrobin.getLastValue());
		}

		jrobins = collector.getDisplayedOtherJRobins();
		for (final JRobin jrobin : jrobins) {
			printDouble(MetricType.GAUGE, camelToSnake(jrobin.getName()),
					"javamelody value per minute", jrobin.getLastValue());
		}
	}

}
