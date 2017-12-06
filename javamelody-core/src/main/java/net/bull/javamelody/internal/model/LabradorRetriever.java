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

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Serializable;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLConnection;
import java.util.Arrays;
import java.util.Collections;
import java.util.Map;
import java.util.zip.GZIPInputStream;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.log4j.Logger;

import net.bull.javamelody.internal.common.HttpParameter;
import net.bull.javamelody.internal.common.HttpPart;
import net.bull.javamelody.internal.common.I18N;
import net.bull.javamelody.internal.common.Parameters;

/**
 * Classe permettant d'ouvrir une connexion http et de récupérer les objets java sérialisés dans la réponse.
 * Utilisée dans le serveur de collecte.
 * @author Emeric Vernat
 */
public class LabradorRetriever {
	@SuppressWarnings("all")
	private static final Logger LOGGER = Logger.getLogger("javamelody");

	/** Timeout des connections serveur en millisecondes (0 : pas de timeout). */
	private static final int CONNECTION_TIMEOUT = 20000;

	/** Timeout de lecture des connections serveur en millisecondes (0 : pas de timeout). */
	private static final int READ_TIMEOUT = 60000;

	private final URL url;
	private final Map<String, String> headers;

	// Rq: les configurations suivantes sont celles par défaut, on ne les change pas
	//	    static { HttpURLConnection.setFollowRedirects(true);
	//	    URLConnection.setDefaultAllowUserInteraction(true); }

	private static class CounterInputStream extends InputStream {
		private final InputStream inputStream;
		private int dataLength;

		CounterInputStream(InputStream inputStream) {
			super();
			this.inputStream = inputStream;
		}

		int getDataLength() {
			return dataLength;
		}

		@Override
		public int read() throws IOException {
			final int result = inputStream.read();
			if (result != -1) {
				dataLength += 1;
			}
			return result;
		}

		@Override
		public int read(byte[] bytes) throws IOException {
			final int result = inputStream.read(bytes);
			if (result != -1) {
				dataLength += result;
			}
			return result;
		}

		@Override
		public int read(byte[] bytes, int off, int len) throws IOException {
			final int result = inputStream.read(bytes, off, len);
			if (result != -1) {
				dataLength += result;
			}
			return result;
		}

		@Override
		public long skip(long n) throws IOException {
			return inputStream.skip(n);
		}

		@Override
		public int available() throws IOException {
			return inputStream.available();
		}

		@Override
		public void close() throws IOException {
			inputStream.close();
		}

		@Override
		public boolean markSupported() {
			return false; // Assume that mark is NO good for a counterInputStream
		}
	}

	public LabradorRetriever(URL url) {
		this(url, null);
	}

	public LabradorRetriever(URL url, Map<String, String> headers) {
		super();
		assert url != null;
		this.url = url;
		this.headers = headers;
	}

	<T> T call() throws IOException {
		if (shouldMock()) {
			// ce générique doit être conservé pour la compilation javac en intégration continue
			return this.<T> createMockResultOfCall();
		}
		final long start = System.currentTimeMillis();
		int dataLength = -1;
		try {
			final URLConnection connection = openConnection();
			// pour traductions (si on vient de CollectorServlet.forwardActionAndUpdateData,
			// cela permet d'avoir les messages dans la bonne langue)
			connection.setRequestProperty("Accept-Language", I18N.getCurrentLocale().getLanguage());

			// Rq: on ne gère pas ici les éventuels cookie de session http,
			// puisque le filtre de monitoring n'est pas censé créer des sessions
			//		if (cookie != null) { connection.setRequestProperty("Cookie", cookie); }

			connection.connect();

			//		final String setCookie = connection.getHeaderField("Set-Cookie");
			//		if (setCookie != null) { cookie = setCookie; }

			final CounterInputStream counterInputStream = new CounterInputStream(
					connection.getInputStream());

			final T result;
			try {
				@SuppressWarnings("unchecked")
				final T tmp = (T) read(connection, counterInputStream);
				result = tmp;
			} finally {
				counterInputStream.close();
				dataLength = counterInputStream.getDataLength();
			}
			if (LOGGER.isDebugEnabled()) {
				LOGGER.debug("read on " + url + " : " + result);
			}

			if (result instanceof RuntimeException) {
				throw (RuntimeException) result;
			} else if (result instanceof Error) {
				throw (Error) result;
			} else if (result instanceof IOException) {
				throw (IOException) result;
			} else if (result instanceof Exception) {
				throw createIOException((Exception) result);
			}
			return result;
		} catch (final ClassNotFoundException e) {
			throw createIOException(e);
		} finally {
			LOGGER.info("http call done in " + (System.currentTimeMillis() - start) + " ms with "
					+ dataLength / 1024 + " KB read for " + url);
		}
	}

	private static IOException createIOException(Exception e) {
		// Rq: le constructeur de IOException avec message et cause n'existe qu'en jdk 1.6
		return new IOException(e.getMessage(), e);
	}

	public void copyTo(HttpServletRequest httpRequest, HttpServletResponse httpResponse)
			throws IOException {
		if (shouldMock()) {
			return;
		}
		assert httpRequest != null;
		assert httpResponse != null;
		final long start = System.currentTimeMillis();
		int dataLength = -1;
		try {
			final URLConnection connection = openConnection();
			// pour traductions
			connection.setRequestProperty("Accept-Language",
					httpRequest.getHeader("Accept-Language"));
			connection.connect();
			httpResponse.setContentType(connection.getContentType());
			final OutputStream output = httpResponse.getOutputStream();
			dataLength = pump(output, connection);
		} finally {
			LOGGER.info("http call done in " + (System.currentTimeMillis() - start) + " ms with "
					+ dataLength / 1024 + " KB read for " + url);
		}
	}

	void downloadTo(OutputStream output) throws IOException {
		if (shouldMock()) {
			return;
		}
		assert output != null;
		final long start = System.currentTimeMillis();
		int dataLength = -1;
		try {
			final URLConnection connection = openConnection();
			connection.connect();

			dataLength = pump(output, connection);
		} finally {
			LOGGER.info("http call done in " + (System.currentTimeMillis() - start) + " ms with "
					+ dataLength / 1024 + " KB read for " + url);
		}
	}

	private int pump(OutputStream output, URLConnection connection) throws IOException {
		final int dataLength;
		final CounterInputStream counterInputStream = new CounterInputStream(
				connection.getInputStream());
		InputStream input = counterInputStream;
		try {
			if ("gzip".equals(connection.getContentEncoding())) {
				input = new GZIPInputStream(input);
			}
			TransportFormat.pump(input, output);
		} finally {
			try {
				input.close();
			} finally {
				close(connection);
				dataLength = counterInputStream.getDataLength();
			}
		}
		return dataLength;
	}

	public void post(ByteArrayOutputStream payload) throws IOException {
		final HttpURLConnection connection = (HttpURLConnection) openConnection();
		connection.setRequestMethod("POST");
		connection.setDoOutput(true);

		if (payload != null) {
			final OutputStream outputStream = connection.getOutputStream();
			payload.writeTo(outputStream);
			outputStream.flush();
		}

		final int status = connection.getResponseCode();
		if (status >= HttpURLConnection.HTTP_BAD_REQUEST) {
			final ByteArrayOutputStream errorOutputStream = new ByteArrayOutputStream();
			TransportFormat.pump(connection.getErrorStream(), errorOutputStream);
			final String msg = "Error connecting to " + url + '(' + status + "): "
					+ errorOutputStream.toString("UTF-8");
			throw new IOException(msg);
		}
		connection.disconnect();
	}

	/**
	 * Ouvre la connection http.
	 * @return Object
	 * @throws IOException   Exception de communication
	 */
	private URLConnection openConnection() throws IOException {
		final URLConnection connection = url.openConnection();
		connection.setUseCaches(false);
		if (CONNECTION_TIMEOUT > 0) {
			connection.setConnectTimeout(CONNECTION_TIMEOUT);
		}
		if (READ_TIMEOUT > 0) {
			connection.setReadTimeout(READ_TIMEOUT);
		}
		// grâce à cette propriété, l'application retournera un flux compressé si la taille
		// dépasse x Ko
		connection.setRequestProperty("Accept-Encoding", "gzip");
		if (headers != null) {
			for (final Map.Entry<String, String> entry : headers.entrySet()) {
				connection.setRequestProperty(entry.getKey(), entry.getValue());
			}
		}
		if (url.getUserInfo() != null) {
			final String authorization = Base64Coder.encodeString(url.getUserInfo());
			connection.setRequestProperty("Authorization", "Basic " + authorization);
		}
		return connection;
	}

	/**
	 * Lit l'objet renvoyé dans le flux de réponse.
	 * @return Object
	 * @param connection URLConnection
	 * @param inputStream InputStream à utiliser à la place de connection.getInputStream()
	 * @throws IOException   Exception de communication
	 * @throws ClassNotFoundException   Une classe transmise par le serveur n'a pas été trouvée
	 */
	private static Serializable read(URLConnection connection, InputStream inputStream)
			throws IOException, ClassNotFoundException {
		InputStream input = inputStream;
		try {
			if ("gzip".equals(connection.getContentEncoding())) {
				// si la taille du flux dépasse x Ko et que l'application a retourné un flux compressé
				// alors on le décompresse
				input = new GZIPInputStream(input);
			}
			final String contentType = connection.getContentType();
			final TransportFormat transportFormat;
			if (contentType != null) {
				if (contentType.startsWith("text/xml")) {
					transportFormat = TransportFormat.XML;
				} else if (contentType.startsWith("text/html")) {
					throw new IllegalStateException(
							"Unexpected html content type, maybe not authentified");
				} else {
					transportFormat = TransportFormat.SERIALIZED;
				}
			} else {
				transportFormat = TransportFormat.SERIALIZED;
			}
			return transportFormat.readSerializableFrom(input);
		} finally {
			try {
				input.close();
			} finally {
				close(connection);
			}
		}
	}

	private static void close(URLConnection connection) throws IOException {
		// ce close doit être fait en finally
		// (http://java.sun.com/j2se/1.5.0/docs/guide/net/http-keepalive.html)
		connection.getInputStream().close();

		if (connection instanceof HttpURLConnection) {
			final InputStream error = ((HttpURLConnection) connection).getErrorStream();
			if (error != null) {
				error.close();
			}
		}
	}

	private static boolean shouldMock() {
		return Boolean.parseBoolean(
				System.getProperty(Parameters.PARAMETER_SYSTEM_PREFIX + "mockLabradorRetriever"));
	}

	// bouchon pour tests unitaires
	@SuppressWarnings("unchecked")
	private <T> T createMockResultOfCall() throws IOException {
		final Object result;
		final String request = url.toString();
		if (!request.contains(HttpParameter.PART.getName() + '=')
				&& !request.contains(HttpParameter.JMX_VALUE.getName())
				|| request.contains(HttpPart.DEFAULT_WITH_CURRENT_REQUESTS.getName())) {
			final String message = request.contains("/test2") ? null
					: "ceci est message pour le rapport";
			result = Arrays.asList(new Counter(Counter.HTTP_COUNTER_NAME, null),
					new Counter("services", null), new Counter(Counter.ERROR_COUNTER_NAME, null),
					new JavaInformations(null, true), message);
		} else {
			result = LabradorMock.createMockResultOfPartCall(request);
		}
		return (T) result;
	}

	private static class LabradorMock { // NOPMD
		// CHECKSTYLE:OFF
		static Object createMockResultOfPartCall(String request) throws IOException {
			// CHECKSTYLE:ON
			final Object result;
			if (request.contains(HttpPart.SESSIONS.getName())
					&& request.contains(HttpParameter.SESSION_ID.getName())) {
				result = null;
			} else if (request.contains(HttpPart.SESSIONS.getName())
					|| request.contains(HttpPart.PROCESSES.getName())
					|| request.contains(HttpPart.JNDI.getName())
					|| request.contains(HttpPart.CONNECTIONS.getName())
					|| request.contains(HttpPart.MBEANS.getName())
					|| request.contains(HttpPart.HOTSPOTS.getName())) {
				result = Collections.emptyList();
			} else if (request.contains(HttpPart.CURRENT_REQUESTS.getName())
					|| request.contains(HttpPart.WEBAPP_VERSIONS.getName())
					|| request.contains(HttpPart.DEPENDENCIES.getName())) {
				result = Collections.emptyMap();
			} else if (request.contains(HttpPart.DATABASE.getName())) {
				try {
					result = new DatabaseInformations(0);
				} catch (final Exception e) {
					throw new IllegalStateException(e);
				}
			} else if (request.contains(HttpPart.HEAP_HISTO.getName())) {
				final InputStream input = LabradorMock.class.getResourceAsStream("/heaphisto.txt");
				try {
					result = new HeapHistogram(input, false);
				} finally {
					input.close();
				}
			} else if (request.contains(HttpPart.LAST_VALUE.getName())) {
				result = -1d;
			} else if (request.contains(HttpParameter.JMX_VALUE.getName())) {
				result = "-1";
			} else if (request.contains(HttpPart.JVM.getName())) {
				result = Collections
						.singletonList(new JavaInformations(Parameters.getServletContext(), false));
			} else {
				result = null;
			}
			return result;
		}

	}
}
