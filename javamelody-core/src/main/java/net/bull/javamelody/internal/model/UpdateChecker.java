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

import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;
import java.util.Timer;
import java.util.TimerTask;

import javax.sql.DataSource;

import net.bull.javamelody.JdbcWrapper;
import net.bull.javamelody.Parameter;
import net.bull.javamelody.internal.common.Parameters;

/**
 * Checks for update of javamelody version.
 * @author Emeric Vernat
 */
public final class UpdateChecker {
	static final String COLLECTOR_SERVER_APPLICATION_TYPE = "Collector server";

	private static final char SEPARATOR = '|';

	private static final String SERVER_URL = "http://javamelody.org/usage/stats";

	private static String newJavamelodyVersion;

	private final Collector collector;

	private final String applicationType;

	private final String serverUrl;

	private UpdateChecker(final Collector collector, final String applicationType,
			final String serverUrl) {
		super();
		assert applicationType != null;
		assert collector != null || COLLECTOR_SERVER_APPLICATION_TYPE.equals(applicationType);
		this.collector = collector;
		this.applicationType = applicationType;
		this.serverUrl = serverUrl;
	}

	public static void init(Timer timer, Collector collector, String applicationType) {
		if (!Parameter.UPDATE_CHECK_DISABLED.getValueAsBoolean()) {
			final UpdateChecker updateChecker = new UpdateChecker(collector, applicationType,
					SERVER_URL);
			final TimerTask updateCheckerTimerTask = new TimerTask() {
				@Override
				public void run() {
					try {
						updateChecker.checkForUpdate();
					} catch (final Throwable t) { // NOPMD
						// probablement pas connecté à Internet, tant pis
					}
				}
			};
			// on laisse 10 minutes pour que la webapp démarre tranquillement, puis toutes les 24h
			timer.scheduleAtFixedRate(updateCheckerTimerTask, 10L * 60 * 1000,
					24L * 60 * 60 * 1000);
		}
	}

	static UpdateChecker createForTest(final Collector collector, final String applicationType,
			final String serverUrl) {
		return new UpdateChecker(collector, applicationType, serverUrl);
	}

	public static String getNewJavamelodyVersion() {
		return newJavamelodyVersion;
	}

	private static void setNewJavamelodyVersion(final String javamelodyVersion) {
		newJavamelodyVersion = javamelodyVersion;
	}

	void checkForUpdate() throws IOException {
		final String anonymousData = getAnonymousData();
		final HttpURLConnection connection = (HttpURLConnection) new URL(serverUrl)
				.openConnection();
		connection.setUseCaches(false);
		connection.setDoOutput(true);
		connection.setRequestMethod("POST");
		connection.setConnectTimeout(60000);
		connection.setReadTimeout(60000);
		connection.setRequestProperty("data", anonymousData);
		connection.connect();

		final Properties properties = new Properties();
		final InputStream input = connection.getInputStream();
		try {
			properties.load(input);
		} finally {
			input.close();
		}
		final String javamelodyVersion = properties.getProperty("version");
		if (javamelodyVersion != null && Parameters.JAVAMELODY_VERSION != null
				&& javamelodyVersion.compareTo(Parameters.JAVAMELODY_VERSION) > 0) {
			setNewJavamelodyVersion(javamelodyVersion);
		}
	}

	private String getAnonymousData() throws IOException {
		final JavaInformations javaInformations = new JavaInformations(
				Parameters.getServletContext(), true);
		// compute a hash number as unique id
		final String uniqueId = hash(
				Parameters.getHostAddress() + '_' + javaInformations.getContextPath());
		final String javamelodyVersion = Parameters.JAVAMELODY_VERSION;
		final String serverInfo = javaInformations.getServerInfo();
		final String javaVersion = javaInformations.getJavaVersion();
		final String jvmVersion = javaInformations.getJvmVersion();
		final String maxMemory = String
				.valueOf(javaInformations.getMemoryInformations().getMaxMemory() / 1024 / 1024);
		final String availableProcessors = String
				.valueOf(javaInformations.getAvailableProcessors());
		final String os = javaInformations.getOS();
		final String databases = getDatabasesUsed();
		final String countersUsed = getCountersUsed();
		final String parametersUsed = getParametersUsed();
		final String featuresUsed = getFeaturesUsed(javaInformations);
		final String locale = Locale.getDefault().toString();
		final long usersMean = getUsersMean();
		final int collectorApplications;
		if (COLLECTOR_SERVER_APPLICATION_TYPE.equals(applicationType)) {
			collectorApplications = Parameters.getCollectorUrlsByApplications().size();
		} else {
			collectorApplications = -1;
		}

		return "{uniqueId=" + encode(uniqueId) + ", serverInfo=" + encode(serverInfo)
				+ ", javamelodyVersion=" + encode(javamelodyVersion) + ", applicationType="
				+ encode(applicationType) + ", javaVersion=" + encode(javaVersion) + ", jvmVersion="
				+ encode(jvmVersion) + ", maxMemory=" + encode(maxMemory) + ", availableProcessors="
				+ encode(availableProcessors) + ", os=" + encode(os) + ", databases="
				+ encode(databases) + ", countersUsed=" + encode(countersUsed) + ", parametersUsed="
				+ encode(parametersUsed) + ", featuresUsed=" + encode(featuresUsed) + ", locale="
				+ encode(locale) + ", usersMean=" + usersMean + ", collectorApplications="
				+ collectorApplications + '}';
	}

	private long getUsersMean() throws IOException {
		if (collector != null) {
			final JRobin httpSessionsJRobin = collector.getJRobin("httpSessions");
			if (httpSessionsJRobin != null) {
				final double usersMean = httpSessionsJRobin.getMeanValue(getYesterdayRange());
				// round to closest integer
				return Math.round(usersMean);
			}
		}
		return 0;
	}

	private Range getYesterdayRange() {
		final Calendar calendar = Calendar.getInstance();
		calendar.set(Calendar.HOUR_OF_DAY, 0);
		calendar.set(Calendar.MINUTE, 0);
		calendar.set(Calendar.SECOND, 0);
		calendar.set(Calendar.MILLISECOND, 0);
		final Date endDate = calendar.getTime();
		calendar.add(Calendar.DAY_OF_YEAR, -1);
		final Date startDate = calendar.getTime();
		return Range.createCustomRange(startDate, endDate);
	}

	private String getParametersUsed() {
		final StringBuilder sb = new StringBuilder();
		for (final Parameter parameter : Parameter.values()) {
			final String value = parameter.getValue();
			if (value != null) {
				if (sb.length() != 0) {
					sb.append(SEPARATOR);
				}
				sb.append(parameter.getCode());
			}
		}
		return sb.toString();
	}

	private String getCountersUsed() {
		if (collector == null) {
			return "";
		}
		try {
			final List<Counter> counters = collector
					.getRangeCountersToBeDisplayed(Period.TOUT.getRange());
			final StringBuilder sb = new StringBuilder();
			for (final Counter counter : counters) {
				if (sb.length() != 0) {
					sb.append(SEPARATOR);
				}
				sb.append(counter.getName());
			}
			return sb.toString();
		} catch (final IOException e) {
			return e.getClass().getSimpleName();
		}
	}

	private String getFeaturesUsed(JavaInformations javaInformations) {
		final List<String> features = new ArrayList<String>();
		if (Parameters.isPdfEnabled()) {
			features.add("pdf");
		}
		if (javaInformations.isCacheEnabled()) {
			features.add("caches");
		}
		if (javaInformations.isJobEnabled()) {
			features.add("jobs");
		}
		if (features.isEmpty()) {
			return "";
		}
		final StringBuilder sb = new StringBuilder();
		for (final String s : features) {
			if (sb.length() != 0) {
				sb.append(SEPARATOR);
			}
			sb.append(s);
		}
		return sb.toString();
	}

	private String getDatabasesUsed() {
		if (Parameters.isNoDatabase()) {
			return "";
		}
		final StringBuilder result = new StringBuilder();
		try {
			if (Parameters.getLastConnectUrl() != null) {
				final Connection connection = DriverManager.getConnection(
						Parameters.getLastConnectUrl(), Parameters.getLastConnectInfo());
				connection.setAutoCommit(false);
				return getDatabaseInfo(connection);
			}

			final Map<String, DataSource> dataSources = JdbcWrapper.getJndiAndSpringDataSources();
			for (final DataSource dataSource : dataSources.values()) {
				final Connection connection = dataSource.getConnection();
				if (result.length() > 0) {
					result.append(SEPARATOR);
				}
				result.append(getDatabaseInfo(connection));
			}
		} catch (final Exception e) {
			result.append(e);
		}
		return result.toString();
	}

	private String getDatabaseInfo(final Connection connection) throws SQLException {
		try {
			final DatabaseMetaData metaData = connection.getMetaData();
			return metaData.getDatabaseProductName() + ' ' + metaData.getDatabaseProductVersion();
		} finally {
			connection.close();
		}
	}

	private static String encode(final String s) {
		if (s != null) {
			return "\"" + s.replace("\\", "\\\\").replace("\"", "\\\"").replace('\n', ' ')
					.replace('\r', ' ') + "\"";
		}
		return null;
	}

	private static MessageDigest getMessageDigestInstance() {
		// SHA1 est un algorithme de hashage qui évite les conflits à 2^80 près entre
		// les identifiants supposés uniques (SHA1 est mieux que MD5 qui est mieux que CRC32).
		try {
			return MessageDigest.getInstance("SHA-1");
		} catch (final NoSuchAlgorithmException e) {
			// ne peut arriver car SHA1 est un algorithme disponible par défaut dans le JDK Sun
			throw new IllegalStateException(e);
		}
	}

	private static String hash(String value) {
		final MessageDigest messageDigest = getMessageDigestInstance();
		messageDigest.update(value.getBytes());
		final byte[] digest = messageDigest.digest();

		final StringBuilder sb = new StringBuilder(digest.length * 2);
		// encodage en chaîne hexadécimale,
		// puisque les caractères bizarres ne peuvent être utilisés sur un système de fichiers
		int j;
		for (final byte element : digest) {
			j = element < 0 ? 256 + element : element;
			if (j < 16) {
				sb.append('0');
			}
			sb.append(Integer.toHexString(j));
		}

		return sb.toString();
	}
}
