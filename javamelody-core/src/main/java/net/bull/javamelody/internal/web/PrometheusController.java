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
import java.util.regex.Pattern;

import net.bull.javamelody.Parameter;
import net.bull.javamelody.internal.common.Parameters;
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
 * to enable collection by a <a href='https://prometheus.io/'>Prometheus</a> server.
 *<br/><br/>
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
 *<br/><br/>
 * Exposed Metrics: <br/>
 * (From {@link JavaInformations})
 * <pre>
 *  javamelody_memory_used_bytes
 *  javamelody_memory_max_bytes
 *  javamelody_memory_used_pct
 *  javamelody_memory_perm_gen_used_bytes
 *  javamelody_memory_perm_gen_max_bytes
 *  javamelody_memory_perm_gen_used_pct
 *  javamelody_memory_gc_millis
 *  javamelody_sessions_active_count
 *  javamelody_sessions_age_avg_minutes
 *  javamelody_transactions_count
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
 *  javamelody_jvm_cpu_millis
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
 *  </pre>
 *  (from {@link Collector} counters)
 *  <pre>
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
 *  </pre>
 *  Additionally, the `lastValue` metrics can also be exported by setting the parameter PROMETHEUS_INCLUDE_LAST_VALUE=true.
 *  Note: the `lastValue` metrics are already aggregated over time, where Prometheus prefers the raw counters and gauges.
 *  Also, obtaining the `lastValue` metrics appears to have a 5-10ms overhead.
 *
 *  The `lastValue` metrics are DISABLED by default.
 *
 * @author https://github.com/slynn1324
 */
class PrometheusController {

	// Pre-Compiled Patterns. Pattern is thread-safe. Matcher is not.
	private static final Pattern CAMEL_TO_SNAKE_PATTERN = Pattern.compile("([a-z])([A-Z]+)");
	private static final Pattern SANITIZE_TO_UNDERSCORE_PATTERN = Pattern.compile("[- :]");
	private static final Pattern SANITIZE_REMOVE_PATTERN = Pattern.compile("[^a-z0-9_]");

	private static final String EMPTY_STRING = "";
	private static final String UNDERSCORE = "_";

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
		// memory
		reportOnMemoryInformations(javaInformations.getMemoryInformations());

		// jvm & system
		reportOnJavaInformations();

		// tomcat
		if (javaInformations.getTomcatInformationsList() != null) {
			reportOnTomcatConnectors();
		}

		if (javaInformations.isCacheEnabled()) {
			reportOnCaches();
		}

		reportOnCollector();

		if (Parameter.PROMETHEUS_INCLUDE_LAST_VALUE.getValueAsBoolean()) {
			reportOnLastValues();
		}
	}

	private void reportOnTomcatConnectors() {

		// TODO - Is filtering by tcInfo.getRequestCount() > 0 necessary?
		final List<TomcatInformations> connectors = javaInformations.getTomcatInformationsList();
		final int numberOfConnectors = connectors.size();

		final TomcatConnectorDataPrinter printer = new TomcatConnectorDataPrinter(numberOfConnectors);

		for (int i = 0; i < numberOfConnectors; i++) {
			 final TomcatInformations connector = connectors.get(i);

			 printer.registerConnector(i, connector);
		}

		printer.printAllTomcatInformationMetrics();
	}

	private class LabeledDataPrinter {

		final int numberOfLabels;
		final String[] labels;

		private LabeledDataPrinter(final int numberOfLabels) {
			this.numberOfLabels = numberOfLabels;
			this.labels = new String[numberOfLabels];
		}

		void registerLabel(final int number, final String label) {
			labels[number] = label;
		}

		void printFormattedValuesWithLabels(final MetricType metricType,
											final String metricName,
											final String metricDescription,
											final String[] metricValues) {
			printHeader(metricType, metricName, metricDescription);

			for (int i = 0; i < numberOfLabels; i++) {
				out.print(metricName);
				out.print(labels[i]);
				out.print(' ');
				out.println(metricValues[i]);
			}
		}

	}

	private class TomcatConnectorDataPrinter extends LabeledDataPrinter {

		private final String[] threadsMax;
		private final String[] currentThreadsBusy;
		private final String[] bytesReceived;
		private final String[] bytesSent;
		private final String[] requestCount;
		private final String[] errorCount;
		private final String[] processingTime;
		private final String[] maxTime;

		private TomcatConnectorDataPrinter(final int numberOfLabels) {
			super(numberOfLabels);
			threadsMax = new String[numberOfLabels];
			currentThreadsBusy = new String[numberOfLabels];
			bytesReceived = new String[numberOfLabels];
			bytesSent = new String[numberOfLabels];
			requestCount = new String[numberOfLabels];
			errorCount = new String[numberOfLabels];
			processingTime = new String[numberOfLabels];
			maxTime = new String[numberOfLabels];
		}

		void registerConnector(final int number, final TomcatInformations tcInfo) {
			registerLabel(number, "{tomcat_name=\"" + sanitizeName(tcInfo.getName()) + "\"}");

			threadsMax[number] = Integer.toString(tcInfo.getMaxThreads());
			currentThreadsBusy[number] = Integer.toString(tcInfo.getCurrentThreadsBusy());
			bytesReceived[number] = Long.toString(tcInfo.getBytesReceived());
			bytesSent[number] = Long.toString(tcInfo.getBytesSent());
			requestCount[number] = Integer.toString(tcInfo.getRequestCount());
			errorCount[number] = Integer.toString(tcInfo.getErrorCount());
			processingTime[number] = Long.toString(tcInfo.getProcessingTime());
			maxTime[number] = Long.toString(tcInfo.getMaxTime());
		}

		private void printAllTomcatInformationMetrics() {
			printFormattedValuesWithLabels(MetricType.GAUGE, "javamelody_tomcat_threads_max",
				"tomcat max threads", threadsMax);

			printFormattedValuesWithLabels(MetricType.GAUGE, "javamelody_tomcat_thread_busy_count",
				"tomcat currently busy threads", currentThreadsBusy);

			printFormattedValuesWithLabels(MetricType.COUNTER, "javamelody_tomcat_received_bytes",
				"tomcat total received bytes", bytesReceived);

			printFormattedValuesWithLabels(MetricType.COUNTER, "javamelody_tomcat_sent_bytes",
				"tomcat total sent bytes", bytesSent);

			printFormattedValuesWithLabels(MetricType.COUNTER, "javamelody_tomcat_request_count",
				"tomcat total request count", requestCount);

			printFormattedValuesWithLabels(MetricType.COUNTER, "javamelody_tomcat_error_count",
				"tomcat total error count", errorCount);

			printFormattedValuesWithLabels(MetricType.COUNTER, "javamelody_tomcat_processing_time_millis",
				"tomcat total processing time", processingTime);

			printFormattedValuesWithLabels(MetricType.GAUGE, "javamelody_tomcat_max_time_millis",
				"tomcat max time for single request", maxTime);
		}

	}

	private class CacheReportDataPrinter extends LabeledDataPrinter {

		private final String[] inMemoryObjectCount;
		private final String[] inMemoryUsedPct;
		private final String[] inMemoryHitsPct;
		private final String[] onDiskObjectCount;
		private final String[] cacheHitsPct;
		private final String[] inMemoryHits;
		private final String[] cacheHits;
		private final String[] cacheMisses;

		private CacheReportDataPrinter(final int size) {
			super(size);
			inMemoryObjectCount = new String[numberOfLabels];
			inMemoryUsedPct = new String[numberOfLabels];
			inMemoryHitsPct = new String[numberOfLabels];
			onDiskObjectCount = new String[numberOfLabels];
			cacheHitsPct = new String[numberOfLabels];
			inMemoryHits = new String[numberOfLabels];
			cacheHits = new String[numberOfLabels];
			cacheMisses = new String[numberOfLabels];
		}

		void registerCache(final int number, final CacheInformations cacheInfo) {

			registerLabel(number, "{cache_name=\"" + sanitizeName(cacheInfo.getName()) + "\"}");

			inMemoryObjectCount[number] = Long.toString(cacheInfo.getInMemoryObjectCount());
			inMemoryUsedPct[number] = decimalFormat.format((double) cacheInfo.getInMemoryPercentUsed() / 100);
			inMemoryHitsPct[number] = decimalFormat.format((double) cacheInfo.getInMemoryHitsRatio() / 100);
			onDiskObjectCount[number] = Long.toString(cacheInfo.getOnDiskObjectCount());
			cacheHitsPct[number] = decimalFormat.format((double) cacheInfo.getHitsRatio() / 100);
			inMemoryHits[number] = Long.toString(cacheInfo.getInMemoryHits());
			cacheHits[number] = Long.toString(cacheInfo.getCacheHits());
			cacheMisses[number] = Long.toString(cacheInfo.getCacheMisses());
		}

		private void printAllCacheMetrics() {
			printFormattedValuesWithLabels(MetricType.GAUGE,
				"javamelody_cache_in_memory_count", "cache in memory count",
				inMemoryObjectCount);

			printFormattedValuesWithLabels(MetricType.GAUGE,
				"javamelody_cache_in_memory_used_pct", "in memory used percent",
				inMemoryUsedPct);

			printFormattedValuesWithLabels(MetricType.GAUGE,
				"javamelody_cache_in_memory_hits_pct", "cache in memory hit percent",
				inMemoryHitsPct);

			printFormattedValuesWithLabels(MetricType.GAUGE,
				"javamelody_cache_on_disk_count", "cache on disk count",
				onDiskObjectCount);

			printFormattedValuesWithLabels(MetricType.GAUGE,
				"javamelody_cache_hits_pct", "cache hits percent",
				cacheHitsPct);

			printFormattedValuesWithLabels(MetricType.COUNTER,
				"javamelody_cache_in_memory_hits_count", "total cache in memory hit count",
				inMemoryHits);

			printFormattedValuesWithLabels(MetricType.COUNTER,
				"javamelody_cache_hits_count", "total cache hit count",
				cacheHits);

			printFormattedValuesWithLabels(MetricType.COUNTER,
				"javamelody_cache_misses_count", "cache misses count",
				cacheMisses);
		}

	}


	private void reportOnCaches() {
		final List<CacheInformations> cacheInformations = javaInformations.getCacheInformationsList();

		final int numberOfCaches = cacheInformations.size();
		final CacheReportDataPrinter printer = new CacheReportDataPrinter(numberOfCaches);

		for (int i = 0; i < numberOfCaches; i++) {
			final CacheInformations cacheInfo = cacheInformations.get(i);
			printer.registerCache(i, cacheInfo);
		}

		printer.printAllCacheMetrics();
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
			printDouble(MetricType.GAUGE, "javamelody_last_value_" + camelToSnake(jrobin.getName()),
					"javamelody value per minute", jrobin.getLastValue());
		}

		jrobins = collector.getDisplayedOtherJRobins();
		for (final JRobin jrobin : jrobins) {
			printDouble(MetricType.GAUGE, "javamelody_last_value_" + camelToSnake(jrobin.getName()),
					"javamelody value per minute", jrobin.getLastValue());
		}
	}

	/**
	 * Reports on information vailable in the {@link JavaInformations} class.
	 */
	private void reportOnJavaInformations() {
		// sessions
		if (javaInformations.getSessionCount() >= 0) {
			printLong(MetricType.GAUGE, "javamelody_sessions_active_count", "active session count",
					javaInformations.getSessionCount());
			printLong(MetricType.GAUGE, "javamelody_sessions_age_avg_minutes",
					"session avg age in minutes", javaInformations.getSessionMeanAgeInMinutes());
		}

		// connections
		if (!Parameters.isNoDatabase()) {
			printLong(MetricType.COUNTER, "javamelody_transactions_count", "transactions count",
					javaInformations.getTransactionCount());
			printLong(MetricType.GAUGE, "javamelody_connections_used_count",
					"used connections count", javaInformations.getActiveConnectionCount());
			printLong(MetricType.GAUGE, "javamelody_connections_active_count", "active connections",
					javaInformations.getActiveConnectionCount());
			if (javaInformations.getMaxConnectionCount() > 0) {
				printLong(MetricType.GAUGE, "javamelody_connections_max_count", "max connections",
						javaInformations.getMaxConnectionCount());
				printDouble(MetricType.GAUGE, "javamelody_connections_used_pct",
						"used connections percentage",
						javaInformations.getUsedConnectionPercentage());
			}
		}

		// system
		if (javaInformations.getSystemLoadAverage() >= 0) {
			printDouble(MetricType.GAUGE, "javamelody_system_load_avg", "system load average",
					javaInformations.getSystemLoadAverage());
		}
		if (javaInformations.getSystemCpuLoad() >= 0) {
			printDouble(MetricType.GAUGE, "javamelody_system_cpu_load_pct", "system cpu load",
					javaInformations.getSystemCpuLoad());
		}
		if (javaInformations.getUnixOpenFileDescriptorCount() >= 0) {
			printDouble(MetricType.GAUGE, "javamelody_system_unix_file_descriptors_open_count",
					"unix open file descriptors count",
					javaInformations.getUnixOpenFileDescriptorCount());
			printDouble(MetricType.GAUGE, "javamelody_system_unix_file_descriptors_max",
					"unix file descriptors max", javaInformations.getUnixMaxFileDescriptorCount());
			printDouble(MetricType.GAUGE, "javamelody_system_unix_file_descriptors_open_pct",
					"unix open file descriptors percentage",
					javaInformations.getUnixOpenFileDescriptorPercentage());
		}
		if (javaInformations.getFreeDiskSpaceInTemp() >= 0) {
			printLong(MetricType.GAUGE, "javamelody_system_tmp_space_free_bytes",
					"tmp space available", javaInformations.getFreeDiskSpaceInTemp());
		}

		// jvm
		printLong(MetricType.GAUGE, "javamelody_jvm_start_time", "jvm start time",
				javaInformations.getStartDate().getTime());
		printLong(MetricType.COUNTER, "javamelody_jvm_cpu_millis", "jvm cpu millis",
				javaInformations.getProcessCpuTimeMillis());
		printLong(MetricType.GAUGE, "javamelody_system_processors_count", "processors available",
				javaInformations.getAvailableProcessors());

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
		if (javaInformations.isJobEnabled()) {
			printLong(MetricType.GAUGE, "javamelody_job_executing_count", "executing job count",
					javaInformations.getCurrentlyExecutingJobCount());
		}
	}

	private void reportOnMemoryInformations(MemoryInformations memoryInformations) {
		printLong(MetricType.GAUGE, "javamelody_memory_used_bytes", "used memory in bytes",
				memoryInformations.getUsedMemory());
		printLong(MetricType.GAUGE, "javamelody_memory_max_bytes", "max memory in bytes",
				memoryInformations.getMaxMemory());
		printDouble(MetricType.GAUGE, "javamelody_memory_used_pct", "memory used percentage",
				memoryInformations.getUsedMemoryPercentage());
		if (memoryInformations.getUsedPermGen() > 0) {
			printLong(MetricType.GAUGE, "javamelody_memory_perm_gen_used_bytes",
					"used perm gen memory in bytes", memoryInformations.getUsedPermGen());
			if (memoryInformations.getMaxPermGen() > 0) {
				printLong(MetricType.GAUGE, "javamelody_memory_perm_gen_max_bytes",
						"max perm gen memory in bytes", memoryInformations.getMaxPermGen());
				printDouble(MetricType.GAUGE, "javamelody_memory_perm_gen_used_pct",
						"used perm gen memory percentage",
						memoryInformations.getUsedPermGenPercentage());
			}
		}

		printDouble(MetricType.COUNTER, "javamelody_memory_gc_millis", "gc time millis",
				memoryInformations.getGarbageCollectionTimeMillis());
	}

	/**
	 * Converts a camelCase or CamelCase string to camel_case
	 * @param camel String
	 * @return String
	 */
	private static String camelToSnake(final String camel) {
		return CAMEL_TO_SNAKE_PATTERN.matcher(camel).replaceAll("$1_$2").toLowerCase(Locale.US);
	}

	/**
	 * converts to lowercase, replaces common separators with underscores, and strips all remaining non-alpha-numeric characters.
	 * @param name String
	 * @return String
	 */
	private static String sanitizeName(final String name) {
		final String lowerCaseName = name.toLowerCase(Locale.US);
		final String separatorReplacedName = SANITIZE_TO_UNDERSCORE_PATTERN.matcher(lowerCaseName).replaceAll(UNDERSCORE);

		return SANITIZE_REMOVE_PATTERN.matcher(separatorReplacedName).replaceAll(EMPTY_STRING);
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
