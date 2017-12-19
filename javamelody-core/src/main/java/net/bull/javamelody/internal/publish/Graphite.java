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

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.net.ConnectException;
import java.net.InetAddress;
import java.net.Socket;
import java.net.UnknownHostException;
import java.nio.charset.Charset;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.Locale;

import javax.net.SocketFactory;

import net.bull.javamelody.Parameter;
import net.bull.javamelody.internal.common.LOG;

/**
 * Publish chart data to <a href='http://graphiteapp.org/'>Graphite</a>.
 * @author Emeric Vernat
 */
class Graphite extends MetricsPublisher {
	private static final int DEFAULT_GRAPHITE_PORT = 2003;
	private static final char SEPARATOR = ' ';

	private final SocketFactory socketFactory;
	private final InetAddress address;
	private final int port;
	private final String prefix;

	private final DecimalFormat decimalFormat = new DecimalFormat("0.00",
			DecimalFormatSymbols.getInstance(Locale.US));
	private final ByteArrayOutputStream buffer = new ByteArrayOutputStream();
	private final Writer bufferWriter;
	private long lastTime;
	private String lastTimestamp;

	Graphite(SocketFactory socketFactory, InetAddress address, int port, Charset charset,
			String prefix) {
		super();
		assert socketFactory != null;
		assert address != null;
		assert charset != null;
		assert prefix != null;
		this.socketFactory = socketFactory;
		this.address = address;
		this.port = port;
		this.prefix = prefix;
		this.bufferWriter = new OutputStreamWriter(buffer, charset);
	}

	static Graphite getInstance(String contextPath, String hostName) {
		final String graphiteAddress = Parameter.GRAPHITE_ADDRESS.getValue();
		if (graphiteAddress != null) {
			assert contextPath != null;
			assert hostName != null;
			final String address;
			final int port;
			final int index = graphiteAddress.lastIndexOf(':');
			if (index != -1) {
				// we could use new URI("socket://" + graphiteAddress).getHost() and getPort() to parse the address
				address = graphiteAddress.substring(0, index);
				port = Integer
						.parseInt(graphiteAddress.substring(index + 1, graphiteAddress.length()));
			} else {
				address = graphiteAddress;
				port = DEFAULT_GRAPHITE_PORT;
			}
			// contextPath est du genre "/testapp"
			// hostName est du genre "www.host.com"
			final String prefix = ("javamelody." + contextPath.replace("/", "") + '.'
					+ hostName.replace('.', '_') + '.').replace(SEPARATOR, '_');
			try {
				return new Graphite(SocketFactory.getDefault(), InetAddress.getByName(address),
						port, Charset.forName("UTF-8"), prefix);
			} catch (final UnknownHostException e) {
				throw new IllegalArgumentException("Invalid host: " + address, e);
			}
		}
		return null;
	}

	@Override
	public synchronized void addValue(String metric, double value) throws IOException {
		final long timeInSeconds = System.currentTimeMillis() / 1000;
		if (lastTime != timeInSeconds) {
			lastTimestamp = String.valueOf(timeInSeconds);
			lastTime = timeInSeconds;
		}
		bufferWriter.append(prefix).append(metric).append(SEPARATOR);
		bufferWriter.append(decimalFormat.format(value)).append(SEPARATOR);
		bufferWriter.append(lastTimestamp).append('\n');
	}

	@Override
	public synchronized void send() throws IOException {
		try {
			bufferWriter.flush();
			final Socket socket = createSocket();
			try {
				buffer.writeTo(socket.getOutputStream());
				checkNoReturnedData(socket);
			} finally {
				socket.close();
			}
		} catch (final ConnectException e) {
			throw new IOException("Error connecting to Graphite at " + address + ':' + port, e);
		} finally {
			// finally to be sure to not keep too much data in buffer
			// including when the socket can't connect
			buffer.reset();
		}
	}

	private Socket createSocket() throws IOException {
		return socketFactory.createSocket(address, port);
	}

	/**
	 * The graphite protocol is a "one-way" streaming protocol and as such there is no easy way
	 * to check that we are sending the output to the wrong place. We can aim the graphite writer
	 * at any listening socket and it will never care if the data is being correctly handled. By
	 * logging any returned bytes we can help make it obvious that it's talking to the wrong
	 * server/socket. In particular if its aimed at the web port of a
	 * graphite server this will make it print out the HTTP error message.
	 * @param socket Socket
	 * @throws IOException e
	 */
	private void checkNoReturnedData(Socket socket) throws IOException {
		final InputStream input = socket.getInputStream();
		if (input.available() > 0) {
			final byte[] bytes = new byte[1000];
			final int toRead = Math.min(input.available(), bytes.length);
			final int read = input.read(bytes, 0, toRead);
			if (read > 0) {
				final String msg = "Data returned by graphite server when expecting no response! "
						+ "Probably aimed at wrong socket or server. Make sure you "
						+ "are publishing to the data port, not the dashboard port. First " + read
						+ " bytes of response: " + new String(bytes, 0, read, "UTF-8");
				LOG.warn(msg, new IOException(msg));
			}
		}
	}

	@Override
	public void stop() {
		// nothing
	}
}
