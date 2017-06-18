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
package net.bull.javamelody.internal.model;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Publish metrics to Graphite and AWS CloudWatch.
 * @author Emeric Vernat
 */
abstract class MetricsPublisher {
	static List<MetricsPublisher> getMetricsPublishers(
			List<JavaInformations> javaInformationsList) {
		assert javaInformationsList != null && !javaInformationsList.isEmpty();
		final List<MetricsPublisher> metricsPublishers = new ArrayList<MetricsPublisher>();
		final StringBuilder sb = new StringBuilder();
		for (final JavaInformations javaInformations : javaInformationsList) {
			if (sb.length() != 0) {
				sb.append('_');
			}
			sb.append(javaInformations.getHost().replaceFirst("@.*", ""));
		}
		final String contextPath = javaInformationsList.get(0).getContextPath();
		final String hosts = sb.toString();
		final Graphite graphite = Graphite.getInstance(contextPath, hosts);
		final CloudWatch cloudWatch = CloudWatch.getInstance(contextPath, hosts);
		if (graphite != null) {
			metricsPublishers.add(graphite);
		}
		if (cloudWatch != null) {
			metricsPublishers.add(cloudWatch);
		}
		if (metricsPublishers.isEmpty()) {
			return Collections.emptyList();
		}
		return metricsPublishers;
	}

	abstract void addValue(String metric, double value) throws IOException;

	abstract void send() throws IOException;

	abstract void stop();
}
