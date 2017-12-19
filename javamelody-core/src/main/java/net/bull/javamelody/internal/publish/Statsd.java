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
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.net.ConnectException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.StandardSocketOptions;
import java.net.UnknownHostException;
import java.nio.ByteBuffer;
import java.nio.channels.DatagramChannel;
import java.nio.charset.Charset;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.Locale;

import net.bull.javamelody.Parameter;
import net.bull.javamelody.internal.common.LOG;

/**
 * Publish chart data to <a href='https://github.com/etsy/statsd'>StatsD</a>.
 * @author Emeric Vernat
 */
class Statsd extends MetricsPublisher {
	// see https://github.com/etsy/statsd/blob/master/examples/StatsdClient.java

	private static final int DEFAULT_STATSD_PORT = 8125;

	private static final int SOCKET_BUFFER_SIZE = 4096;

	private final InetSocketAddress address;
	private final String prefix;

	private final DecimalFormat decimalFormat = new DecimalFormat("0.00",
			DecimalFormatSymbols.getInstance(Locale.US));
	private final ByteArrayOutputStream buffer = new ByteArrayOutputStream();
	private final Writer bufferWriter = new OutputStreamWriter(buffer, Charset.forName("UTF-8"));

	Statsd(InetAddress host, int port, String prefix) {
		super();
		this.address = new InetSocketAddress(host, port);
		this.prefix = prefix;
	}

	static Statsd getInstance(String contextPath, String hostName) {
		final String statsdAddress = Parameter.STATSD_ADDRESS.getValue();
		if (statsdAddress != null) {
			assert contextPath != null;
			assert hostName != null;
			final String address;
			final int port;
			final int index = statsdAddress.lastIndexOf(':');
			if (index != -1) {
				// we could use new URI("socket://" + statsdAddress).getHost() and getPort() to parse the address
				address = statsdAddress.substring(0, index);
				port = Integer.parseInt(statsdAddress.substring(index + 1, statsdAddress.length()));
			} else {
				address = statsdAddress;
				port = DEFAULT_STATSD_PORT;
			}
			// contextPath est du genre "/testapp"
			// hostName est du genre "www.host.com"
			final String prefix = "javamelody." + contextPath.replace("/", "") + '.' + hostName
					+ '.';
			try {
				return new Statsd(InetAddress.getByName(address), port, prefix);
			} catch (final UnknownHostException e) {
				throw new IllegalArgumentException("Invalid host: " + address, e);
			}
		}
		return null;
	}

	@Override
	public synchronized void addValue(String metric, double value) throws IOException {
		// String.format(Locale.ENGLISH, "%s:%d|ms", key, value) for a timing value
		// String.format(Locale.ENGLISH, "%s:%s|c", key, magnitude) to increment a counter
		// String.format(Locale.ENGLISH, "%s:%s|g", key, value) for a gauge value
		bufferWriter.append(prefix).append(metric).append(':');
		bufferWriter.append(decimalFormat.format(value)).append('\n');
	}

	@Override
	public synchronized void send() throws IOException {
		try {
			bufferWriter.flush();
			final byte[] bytes = buffer.toByteArray();
			final ByteBuffer byteBuffer = ByteBuffer.wrap(bytes);
			final DatagramChannel channel = createDatagramChannel();
			try {
				final int nbSentBytes = channel.send(byteBuffer, address);
				if (bytes.length != nbSentBytes) {
					final String msg = String.format(
							"Could not send entirely data to StatsD host %s:%d. Only sent %d bytes out of %d bytes",
							address.getHostName(), address.getPort(), nbSentBytes, bytes.length);
					LOG.warn(msg, new IOException(msg));
				}
			} finally {
				channel.close();
			}
		} catch (final ConnectException e) {
			throw new IOException("Error connecting to StatsD at " + address.getHostName() + ':'
					+ address.getPort(), e);
		} finally {
			// finally to be sure to not keep too much data in buffer
			// including when the socket can't connect
			buffer.reset();
		}
	}

	private DatagramChannel createDatagramChannel() throws IOException {
		final DatagramChannel channel = DatagramChannel.open();
		// Put this in non-blocking mode so send does not block forever.
		channel.configureBlocking(false);
		// Increase the size of the output buffer so that the size is larger than our buffer size.
		channel.setOption(StandardSocketOptions.SO_SNDBUF, SOCKET_BUFFER_SIZE);
		return channel;
	}

	@Override
	public void stop() {
		// nothing
	}
}
