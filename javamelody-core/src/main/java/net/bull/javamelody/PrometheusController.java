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
package net.bull.javamelody;

import java.io.IOException;
import java.io.PrintWriter;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.Locale;
import java.util.regex.Pattern;

import net.bull.javamelody.internal.model.JavaInformations;

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
 * Implementations will/should directly output the Prometheus text format avoiding dependency 
 * on any additional libraries.
 * 
 * This class is abstract and provides basic formatting methods for implementations.
 * 
 * JavaMelody provides two implementations: 
 * <ul>
 * <li><code>DefaultPrometheusController</code> which outputs default metrics</li>
 * <li><code>LastValuePrometheusController</code> which outputs the `last value` metrics. 
 *         (optional, specify includeLastValue=true as request parameter)</li>
 * </ul>
 * 
 * Additional implementations can be provided with a custom controller factory.
 * See {@link PrometheusControllerFactory} for details
 *
 * @author https://github.com/slynn1324, Stefan Penndorf, Emeric Vernat, Roland Praml
 */
public abstract class PrometheusController {

	private static final DecimalFormatSymbols DECIMAL_FORMAT_SYMBOLS;

	static {
		DECIMAL_FORMAT_SYMBOLS = DecimalFormatSymbols.getInstance(Locale.US);
		// setNaN for #806: on Java 8 and before, decimalFormat prints \uFFFD ('<?>') instead of NaN
		DECIMAL_FORMAT_SYMBOLS.setNaN("NaN");
	}

	// Pre-Compiled Patterns. Pattern is thread-safe. Matcher is not.
	private static final Pattern CAMEL_TO_SNAKE_PATTERN = Pattern.compile("([a-z])([A-Z]+)");
	private static final Pattern SANITIZE_TO_UNDERSCORE_PATTERN = Pattern.compile("[- :]");
	private static final Pattern SANITIZE_REMOVE_PATTERN = Pattern.compile("[^a-z0-9_]");

	private static final String EMPTY_STRING = "";
	private static final String UNDERSCORE = "_";

	public enum MetricType {
		GAUGE("gauge"), COUNTER("counter");

		private final String code;

		MetricType(String code) {
			this.code = code;
		}

		public String getCode() {
			return code;
		}
	}

	private final PrintWriter out;
	private final DecimalFormat decimalFormat;
	private final String metricPrefix;

	/**
	 * Constructor for Prometheus Controller with custom metric prefix
	 *
	 * @param out the PrintWriter
	 * @param metricPrefix a custom metric prefix (must meet the prometheus naming conventions)
	 */
	public PrometheusController(PrintWriter out, String metricPrefix) {
		super();
		this.metricPrefix = metricPrefix;
		this.out = out;
		assert out != null;
		decimalFormat = new DecimalFormat();
		decimalFormat.setDecimalFormatSymbols(DECIMAL_FORMAT_SYMBOLS);
		decimalFormat.setGroupingUsed(false);
		decimalFormat.setMinimumIntegerDigits(1);
		decimalFormat.setMaximumFractionDigits(15);
	}

	/**
	 * Produce the full report.
	 * @throws IOException e
	 */
	public abstract void report() throws IOException;

	/**
	 * Converts a camelCase or CamelCase string to camel_case
	 * @param camel String
	 * @return String
	 */
	protected static String camelToSnake(String camel) {
		return CAMEL_TO_SNAKE_PATTERN.matcher(camel).replaceAll("$1_$2").toLowerCase(Locale.US);
	}

	/**
	 * converts to lowercase, replaces common separators with underscores, and strips all remaining non-alpha-numeric characters.
	 * @param name String
	 * @return String
	 */
	protected static String sanitizeName(String name) {
		final String lowerCaseName = name.toLowerCase(Locale.US);
		final String separatorReplacedName = SANITIZE_TO_UNDERSCORE_PATTERN.matcher(lowerCaseName)
				.replaceAll(UNDERSCORE);
		return SANITIZE_REMOVE_PATTERN.matcher(separatorReplacedName).replaceAll(EMPTY_STRING);
	}

	// prints a long metric value, including HELP and TYPE rows
	protected void printLong(MetricType metricType, String name, String description, long value) {
		printHeader(metricType, name, description);
		printLongWithFields(name, null, value);
	}

	// prints a double metric value, including HELP and TYPE rows
	protected void printDouble(MetricType metricType, String name, String description,
			double value) {
		printHeader(metricType, name, description);
		printDoubleWithFields(name, null, value);
	}

	// prints a long metric value with optional fields
	protected void printLongWithFields(String name, String fields, long value) {
		print(metricPrefix);
		print(name);
		if (fields != null) {
			print(fields);
		}
		print(' ');
		println(String.valueOf(value));
	}

	// prints a double metric value with optional fields
	protected void printDoubleWithFields(String name, String fields, double value) {
		print(metricPrefix);
		print(name);
		if (fields != null) {
			print(fields);
		}
		print(' ');
		println(decimalFormat.format(value));
	}

	// prints the HELP and TYPE rows
	protected void printHeader(MetricType metricType, String name, String description) {
		print("# HELP ");
		print(metricPrefix);
		print(name);
		print(' ');
		println(description);

		print("# TYPE ");
		print(metricPrefix);
		print(name);
		print(' ');
		println(metricType.getCode());
	}

	private void print(String s) {
		out.print(s);
	}

	private void print(char c) {
		out.print(c);
	}

	private void println(String s) {
		out.print(s);
		// out.println() prints "\r\n" on Windows and Prometheus does not recognize "\r\n" as EOL
		// (in Prometheus: "no token found" and in promtool check metrics:
		// error while linting: text format parsing error in line 2: unknown metric type "gauge\r")
		out.print('\n');
	}
}
