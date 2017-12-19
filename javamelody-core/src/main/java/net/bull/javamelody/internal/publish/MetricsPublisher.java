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
package net.bull.javamelody.internal.publish;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import net.bull.javamelody.Parameter;
import net.bull.javamelody.internal.model.JavaInformations;

/**
 * Publish metrics to Graphite, AWS CloudWatch, Datadog, StatsD or InfluxDB.
 * @author Emeric Vernat
 */
public abstract class MetricsPublisher {
	public static List<MetricsPublisher> getMetricsPublishers(
			List<JavaInformations> javaInformationsList) {
		assert javaInformationsList != null && !javaInformationsList.isEmpty();
		final StringBuilder sb = new StringBuilder();
		for (final JavaInformations javaInformations : javaInformationsList) {
			if (sb.length() != 0) {
				sb.append('_');
			}
			sb.append(javaInformations.getHost().replaceFirst("@.*", ""));
		}
		String contextPath = Parameter.APPLICATION_NAME.getValue();
		if (contextPath == null) {
			contextPath = javaInformationsList.get(0).getContextPath();
		}
		if (contextPath == null) {
			// for NodesCollector in Jenkins, contextPath is null
			contextPath = "NA";
		} else if (contextPath.isEmpty()) {
			// for CloudWatch, InfluxDB, Datadog, a tag/dimension is not supposed to be empty
			contextPath = "/";
		}
		final String hosts = sb.toString();
		return getMetricsPublishers(contextPath, hosts);
	}

	private static List<MetricsPublisher> getMetricsPublishers(String contextPath, String hosts) {
		final List<MetricsPublisher> metricsPublishers = new ArrayList<MetricsPublisher>();
		final Graphite graphite = Graphite.getInstance(contextPath, hosts);
		final Statsd statsd = Statsd.getInstance(contextPath, hosts);
		final CloudWatch cloudWatch = CloudWatch.getInstance(contextPath, hosts);
		final InfluxDB influxDb = InfluxDB.getInstance(contextPath, hosts);
		final Datadog datadog = Datadog.getInstance(contextPath, hosts);
		if (graphite != null) {
			metricsPublishers.add(graphite);
		}
		if (statsd != null) {
			metricsPublishers.add(statsd);
		}
		if (cloudWatch != null) {
			metricsPublishers.add(cloudWatch);
		}
		if (influxDb != null) {
			metricsPublishers.add(influxDb);
		}
		if (datadog != null) {
			metricsPublishers.add(datadog);
		}
		if (metricsPublishers.isEmpty()) {
			return Collections.emptyList();
		}
		return metricsPublishers;
	}

	public abstract void addValue(String metric, double value) throws IOException;

	public abstract void send() throws IOException;

	public abstract void stop();
}
