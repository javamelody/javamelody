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
package net.bull.javamelody.internal.web;

import java.io.IOException;
import java.io.PrintWriter;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.Collection;
import java.util.List;
import java.util.Locale;

import net.bull.javamelody.Parameter;
import net.bull.javamelody.internal.model.CacheInformations;
import net.bull.javamelody.internal.model.Collector;
import net.bull.javamelody.internal.model.Counter;
import net.bull.javamelody.internal.model.CounterRequest;
import net.bull.javamelody.internal.model.JRobin;
import net.bull.javamelody.internal.model.JavaInformations;
import net.bull.javamelody.internal.model.MemoryInformations;
import net.bull.javamelody.internal.model.TomcatInformations;

/**
 * Produces a report of the data in {@link JavaInformations} in the Prometheus text format
 * to enable collection by a <a href='https://prometheus.io/>Prometheus</a> server.
 *
 * Metric names have been adjusted to match Prometheus recommendations.
 *
 * The {@link JavaInformations} fields to Prometheus metric mappings are done statically
 * to avoid any performance penalties of Java Reflection.  Additional metadata (description, type)
 * about each statistic is merged in as well.
 *
 * In the spirit of JavaMelody, special attention is paid to performance so that exposing
 * these metrics should have very little performance overhead on applications.
 *
 * This implementation directly outputs the Prometheus text format avoiding dependence on any
 * additional libraries.
 *
 * Exposed Metrics:
 * (From {@link JavaInformations}
 *  javamelody_memory_used_bytes
 *  javamelody_memory_max_bytes
 *  javamelody_memory_used_pct
 *  javamelody_memory_perm_gen_used_bytes
 *  javamelody_memory_perm_gen_max_bytes
 *  javamelody_memory_perm_gen_used_pct
 *  javamelody_sessions_active_count
 *  javamelody_sessions_age_avg_minutes
 *  javamelody_connections_used_count
 *  javamelody_connections_max_count
 *  javamelody_connections_active_count
 *  javamelody_connections_used_pct
 *  javamelody_system_load_avg
 *  javamelody_system_cpu_load_pct
 *  javamelody_system_unix_file_descriptors_open_count
 *  javamelody_system_unix_file_descriptors_max
 *  javamelody_system_unix_file_descriptors_open_pct
 *  javamelody_system_processors_count
 *  javamelody_system_tmp_space_free_bytes
 *  javamelody_jvm_start_time
 *  javamelody_threads_count
 *  javamelody_threads_max_count
 *  javamelody_threads_started_count
 *  javamelody_threads_active_count
 *  javamelody_job_executing_count
 *  javamelody_tomcat_threads_max{tomcat_name="__name__"}
 *  javamelody_tomcat_thread_busy_count{tomcat_name="__name__"}
 *  javamelody_tomcat_received_bytes{tomcat_name="__name__"}
 *  javamelody_tomcat_sent_bytes{tomcat_name="__name__"}
 *  javamelody_tomcat_request_count{tomcat_name="__name__"}
 *  javamelody_tomcat_error_count{tomcat_name="__name__"}
 *  javamelody_tomcat_processing_time_millis{tomcat_name="__name__"}
 *  javamelody_tomcat_max_time_millis{tomcat_name="__name__"}
 *  javamelody_cache_in_memory_count{cache_name="__name__"}
 *  javamelody_cache_in_memory_used_pct{cache_name="__name__"}
 *  javamelody_cache_in_memory_hits_pct{cache_name="__name__"}
 *  javamelody_cache_on_disk_count{cache_name="__name__"}
 *  javamelody_cache_hits_pct{cache_name="__name__"}
 *
 *  (from {@link Collector} counters)
 *  javamelody_http_hits_count
 *  javamelody_http_errors_count
 *  javamelody_http_duration_millis
 *  javamelody_sql_hits_count
 *  javamelody_sql_errors_count
 *  javamelody_sql_duration_millis
 *  javamelody_jpa_hits_count
 *  javamelody_jpa_errors_count
 *  javamelody_jpa_duration_millis
 *  javamelody_ejb_hits_count
 *  javamelody_ejb_errors_count
 *  javamelody_ejb_duration_millis
 *  javamelody_spring_hits_count
 *  javamelody_spring_errors_count
 *  javamelody_spring_duration_millis
 *  javamelody_guice_hits_count
 *  javamelody_guice_errors_count
 *  javamelody_guice_duration_millis
 *  javamelody_services_hits_count
 *  javamelody_services_errors_count
 *  javamelody_services_duration_millis
 *  javamelody_struts_hits_count
 *  javamelody_struts_errors_count
 *  javamelody_struts_duration_millis
 *  javamelody_jsf_hits_count
 *  javamelody_jsf_errors_count
 *  javamelody_jsf_duration_millis
 *  javamelody_jsp_hits_count
 *  javamelody_jsp_errors_count
 *  javamelody_jsp_duration_millis
 *  javamelody_error_hits_count
 *  javamelody_error_errors_count
 *  javamelody_error_duration_millis
 *  javamelody_log_hits_count
 *  javamelody_log_errors_count
 *  javamelody_log_duration_millis
 *
 *  Additionally, the `lastValue` metrics can also be exported by setting the property PROMETHEUS_INCLUDE_LAST_VALUE='true'.
 *  Note - the `lastValue` metrics are already aggregated over time, where Prometheus prefers the raw counters and gauges.
 *  Also, obtaining the `lastValue` metrics appears to have a 5-10ms overhead.
 *
 *  The `lastValue` metrics are DISABLED by default
 *
 * @author https://github.com/slynn1324
 */
class PrometheusController {

	private enum MetricType {
		GAUGE("gauge"), COUNTER("counter");

		private final String code;

		MetricType(String code) {
			this.code = code;
		}

		public String getCode() {
			return code;
		}
	}

	private final JavaInformations javaInformations;
	private final Collector collector;
	private final PrintWriter out;
	private final DecimalFormat decimalFormat;

	PrometheusController(List<JavaInformations> javaInformations, Collector collector,
			PrintWriter out) throws IOException {
		super();
		assert javaInformations != null && !javaInformations.isEmpty();
		assert collector != null;
		assert out != null;
		// it doesn't make much sense to use a JavaMelody collector server with Prometheus
		// (which is effectively it's own collector server)
		if (javaInformations.size() > 1) {
			throw new IOException(
					"JavaMelody collector server not supported - configure Prometheus to scrape nodes.");
		}
		this.javaInformations = javaInformations.get(0);
		this.collector = collector;
		this.out = out;

		decimalFormat = new DecimalFormat();
		decimalFormat.setDecimalFormatSymbols(DecimalFormatSymbols.getInstance(Locale.US));
		decimalFormat.setGroupingUsed(false);
		decimalFormat.setMinimumIntegerDigits(1);
		decimalFormat.setMaximumFractionDigits(15);
	}

	/**
	 * Produce the full report.
	 * @throws IOException e
	 */
	void report() throws IOException {
		reportOnJavaInformations();
		reportOnCollector();
		if (Parameter.PROMETHEUS_INCLUDE_LAST_VALUE.getValueAsBoolean()) {
			reportOnLastValues();
		}
	}

	/**
	 * Reports on hits, errors, and duration sum for all counters in the collector.
	 *
	 * Bypasses the {@link JRobin#getLastValue()} methods to provide real-time counters as well as
	 * improving performance from bypassing JRobin reads in the getLastValue() method.
	 */
	private void reportOnCollector() {
		for (final Counter counter : collector.getCounters()) {
			final List<CounterRequest> requests = counter.getRequests();
			long hits = 0;
			long duration = 0;
			long errors = 0;
			for (final CounterRequest cr : requests) {
				hits += cr.getHits();
				duration += cr.getDurationsSum();
				errors += cr.getSystemErrors();
			}

			final String sanitizedName = sanitizeName(counter.getName());
			printLong(MetricType.COUNTER, "javamelody_" + sanitizedName + "_hits_count",
					"javamelody counter", hits);
			printLong(MetricType.COUNTER, "javamelody_" + sanitizedName + "_errors_count",
					"javamelody counter", errors);
			printLong(MetricType.COUNTER, "javamelody_" + sanitizedName + "_duration_millis",
					"javamelody counter", duration);
		}
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
	private void reportOnLastValues() throws IOException {
		Collection<JRobin> jrobins = collector.getDisplayedCounterJRobins();
		for (final JRobin jrobin : jrobins) {
			printDouble(MetricType.GAUGE,
					"javamelody_last_result_" + camelToSnake(jrobin.getName()),
					"javamelody value per minute", jrobin.getLastValue());
		}

		jrobins = collector.getDisplayedOtherJRobins();
		for (final JRobin jrobin : jrobins) {
			printDouble(MetricType.GAUGE,
					"javamelody_last_result_" + camelToSnake(jrobin.getName()),
					"javamelody value per minute", jrobin.getLastValue());
		}
	}

	/**
	 * Reports on information vailable in the {@link JavaInformations} class, including the {@link CacheInformations} and
	 * {@link TomcatInformations} sub collections.
	 */
	private void reportOnJavaInformations() {
		// memory
		final MemoryInformations memoryInformations = javaInformations.getMemoryInformations();
		printLong(MetricType.GAUGE, "javamelody_memory_used_bytes", "used memory in bytes",
				memoryInformations.getUsedMemory());
		printLong(MetricType.GAUGE, "javamelody_memory_max_bytes", "max memory in bytes",
				memoryInformations.getMaxMemory());
		printDouble(MetricType.GAUGE, "javamelody_memory_used_pct", "memory used percentage",
				memoryInformations.getUsedMemoryPercentage());
		printLong(MetricType.GAUGE, "javamelody_memory_perm_gen_used_bytes",
				"used perm gen memory in bytes", memoryInformations.getUsedPermGen());
		printLong(MetricType.GAUGE, "javamelody_memory_perm_gen_max_bytes",
				"max perm gen memory in bytes", memoryInformations.getMaxPermGen());
		printDouble(MetricType.GAUGE, "javamelody_memory_perm_gen_used_pct",
				"used perm gen memory percentage", memoryInformations.getUsedPermGenPercentage());

		// sessions
		printLong(MetricType.GAUGE, "javamelody_sessions_active_count", "active session count",
				javaInformations.getSessionCount());
		printLong(MetricType.GAUGE, "javamelody_sessions_age_avg_minutes",
				"session avg age in minutes", javaInformations.getSessionMeanAgeInMinutes());

		// connections
		printLong(MetricType.GAUGE, "javamelody_connections_used_count", "used connections count",
				javaInformations.getActiveConnectionCount());
		printLong(MetricType.GAUGE, "javamelody_connections_max_count", "max connections",
				javaInformations.getMaxConnectionCount());
		printLong(MetricType.GAUGE, "javamelody_connections_active_count", "active connections",
				javaInformations.getActiveConnectionCount());
		printDouble(MetricType.GAUGE, "javamelody_connections_used_pct",
				"used connections percentage", javaInformations.getUsedConnectionPercentage());

		// system
		printDouble(MetricType.GAUGE, "javamelody_system_load_avg", "system load average",
				javaInformations.getSystemLoadAverage());
		printDouble(MetricType.GAUGE, "javamelody_system_cpu_load_pct", "system cpu load",
				javaInformations.getSystemCpuLoad());
		printDouble(MetricType.GAUGE, "javamelody_system_unix_file_descriptors_open_count",
				"unix open file descriptors count",
				javaInformations.getUnixOpenFileDescriptorCount());
		printDouble(MetricType.GAUGE, "javamelody_system_unix_file_descriptors_max",
				"unix file descriptors max", javaInformations.getUnixMaxFileDescriptorCount());
		printDouble(MetricType.GAUGE, "javamelody_system_unix_file_descriptors_open_pct",
				"unix open file descriptors percentage",
				javaInformations.getUnixOpenFileDescriptorPercentage());
		printLong(MetricType.GAUGE, "javamelody_system_processors_count", "processors available",
				javaInformations.getAvailableProcessors());
		printLong(MetricType.GAUGE, "javamelody_system_tmp_space_free_bytes", "tmp space available",
				javaInformations.getFreeDiskSpaceInTemp());

		// jvm
		printLong(MetricType.GAUGE, "javamelody_jvm_start_time", "jvm start time",
				javaInformations.getStartDate().getTime());

		// threads
		printLong(MetricType.GAUGE, "javamelody_threads_count", "threads count",
				javaInformations.getThreadCount());
		printLong(MetricType.GAUGE, "javamelody_threads_max_count", "threads peak count",
				javaInformations.getPeakThreadCount());
		printLong(MetricType.COUNTER, "javamelody_threads_started_count", "total threads started",
				javaInformations.getTotalStartedThreadCount());
		printLong(MetricType.GAUGE, "javamelody_threads_active_count", "active thread count",
				javaInformations.getActiveThreadCount());

		// jobs
		printLong(MetricType.GAUGE, "javamelody_job_executing_count", "executing job count",
				javaInformations.getCurrentlyExecutingJobCount());

		// tomcat
		if (javaInformations.getTomcatInformationsList() != null) {
			for (final TomcatInformations tcInfo : javaInformations.getTomcatInformationsList()) {
				final String fields = "{tomcat_name=\"" + sanitizeName(tcInfo.getName()) + "\"}";
				printLong(MetricType.GAUGE, "javamelody_tomcat_threads_max" + fields,
						"tomcat max threads", tcInfo.getMaxThreads());
				printLong(MetricType.GAUGE, "javamelody_tomcat_thread_busy_count" + fields,
						"tomcat busy threads", tcInfo.getCurrentThreadsBusy());
				printLong(MetricType.COUNTER, "javamelody_tomcat_received_bytes" + fields,
						"tomcat received bytes", tcInfo.getBytesReceived());
				printLong(MetricType.COUNTER, "javamelody_tomcat_sent_bytes" + fields,
						"tomcat sent bytes", tcInfo.getBytesSent());
				printLong(MetricType.COUNTER, "javamelody_tomcat_request_count" + fields,
						"tomcat request count", tcInfo.getRequestCount());
				printLong(MetricType.COUNTER, "javamelody_tomcat_error_count" + fields,
						"tomcat error count", tcInfo.getErrorCount());
				printLong(MetricType.COUNTER, "javamelody_tomcat_processing_time_millis" + fields,
						"tomcat processing time", tcInfo.getProcessingTime());
				printLong(MetricType.GAUGE, "javamelody_tomcat_max_time_millis" + fields,
						"tomcat max time", tcInfo.getMaxTime());
			}
		}

		// caches
		if (javaInformations.getCacheInformationsList() != null) {
			for (final CacheInformations cacheInfo : javaInformations.getCacheInformationsList()) {
				final String fields = "{cache_name=\"" + sanitizeName(cacheInfo.getName()) + "\"}";
				printLong(MetricType.GAUGE, "javamelody_cache_in_memory_count" + fields,
						"cache in memory count", cacheInfo.getInMemoryObjectCount());
				printDouble(MetricType.GAUGE, "javamelody_cache_in_memory_used_pct" + fields,
						"in memory used percent",
						(double) cacheInfo.getInMemoryPercentUsed() / 100);
				printDouble(MetricType.GAUGE, "javamelody_cache_in_memory_hits_pct" + fields,
						"cache in memory hit percent",
						(double) cacheInfo.getInMemoryHitsRatio() / 100);
				printLong(MetricType.GAUGE, "javamelody_cache_on_disk_count" + fields,
						"cache on disk count", cacheInfo.getOnDiskObjectCount());
				printDouble(MetricType.GAUGE, "javamelody_cache_hits_pct" + fields,
						"cache hits percent", (double) cacheInfo.getHitsRatio() / 100);
				printLong(MetricType.COUNTER, "javamelody_cache_in_memory_hits_count" + fields,
						"cache in memory hit count", cacheInfo.getInMemoryHits());
				printLong(MetricType.COUNTER, "javamelody_cache_hits_count" + fields,
						"cache  hit count", cacheInfo.getCacheHits());
				printLong(MetricType.COUNTER, "javamelody_cache_misses_count" + fields,
						"cache misses count", cacheInfo.getCacheMisses());
			}
		}
	}

	/**
	 * Converts a camelCase or CamelCase string to camel_case
	 * @param camel String
	 * @return String
	 */
	private static String camelToSnake(String camel) {
		return camel.replaceAll("([a-z])([A-Z]+)", "$1_$2").toLowerCase();
	}

	/**
	 * converts to lowercase, replaces common separators with underscores, and strips all remaining non-alpha-numeric characters.
	 * @param name String
	 * @return String
	 */
	private static String sanitizeName(String name) {
		return name.toLowerCase().replaceAll("[- :]", "_").replaceAll("[^a-z0-9_]", "");
	}

	// prints a long metric value, including HELP and TYPE rows
	private void printLong(MetricType metricType, String name, String description, long value) {
		printHeader(metricType, name, description);
		out.print(name);
		out.print(' ');
		out.println(value);
	}

	// prints a double metric value, including HELP and TYPE rows
	private void printDouble(MetricType metricType, String name, String description, double value) {
		printHeader(metricType, name, description);
		out.print(name);
		out.print(' ');
		out.println(decimalFormat.format(value));
	}

	// prints the HELP and TYPE rows
	private void printHeader(MetricType metricType, String name, String description) {
		out.print("# HELP ");
		out.print(name);
		out.print(' ');
		out.println(description);

		out.print("# TYPE ");
		out.print(name);
		out.print(' ');
		out.println(metricType.getCode());
	}
}
