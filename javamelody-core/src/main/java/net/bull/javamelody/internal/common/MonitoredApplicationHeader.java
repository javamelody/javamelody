package net.bull.javamelody.internal.common;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import org.apache.logging.log4j.util.Strings;

/**
 * Classe pour l'en-tête URL de l'application contrôlée.
 * @author Mohamed Omer
 */
public class MonitoredApplicationHeader {

	private static final String COLLECTOR_APPLICATIONS_HEADER_FILENAME = "applications.header.properties";

	private final Map<String, String> headersMap = new HashMap<String, String>();
	private final String headerAsString;

	public MonitoredApplicationHeader(String headerAsString) {
		this.headerAsString = headerAsString;
		parseHeader(headerAsString);
	}

	private void parseHeader(String headers) {
		String[] headerParts = headers.split(";");
		for (String header : headerParts) {
			String[] headerKeyValue = header.split(":");
			if (headerKeyValue.length == 2) {
				headersMap.put(headerKeyValue[0], headerKeyValue[1]);
			}
		}
	}

	public String get(String key) {
		return headersMap.get(key);
	}

	public String getAsString() {
		return this.headerAsString;
	}

	public Map<String, String> toMap() {
		return new HashMap<String, String>(headersMap);
	}

	public static String toWritableString(Map<String, String> headers) {
		if (headers.size() <= 0) {
			return Strings.EMPTY;
		}
		StringBuilder builder = new StringBuilder();
		for (String key : headers.keySet()) {
			builder.append(key).append(":").append(headers.get(key)).append(";");
		}
		return builder.toString();
	}

	public static Map<String, String> readCollectorApplicationsHeaders() throws IOException {
		File headersFile = new File(Parameters.getStorageDirectory(""),
				COLLECTOR_APPLICATIONS_HEADER_FILENAME);
		Map<String, String> result = new HashMap<String, String>();
		if (!headersFile.exists())
			return result;
		Properties headers = new Properties();
		FileInputStream inStream = new FileInputStream(headersFile);
		try {
			headers.load(inStream);
		} finally {
			inStream.close();
		}
		for (Object key : headers.keySet()) {
			result.put(String.valueOf(key), String.valueOf(headers.get(key)));
		}
		return result;
	}

	public static void writeCollectorApplicationsHeaders(Map<String, String> headersMap)
			throws IOException {
		final Properties properties = new Properties();
		for (final Map.Entry<String, String> entry : headersMap.entrySet()) {
			properties.put(entry.getKey(), entry.getValue());
		}
		final File headersFile = new File(Parameters.getStorageDirectory(""),
				COLLECTOR_APPLICATIONS_HEADER_FILENAME);
		final File directory = headersFile.getParentFile();
		if (!directory.mkdirs() && !directory.exists()) {
			throw new IOException("JavaMelody directory can't be created: " + directory.getPath());
		}
		final FileOutputStream output = new FileOutputStream(headersFile);
		try {
			properties.store(output, "url headers of the applications to monitor");
		} finally {
			output.close();
		}
	}
}
