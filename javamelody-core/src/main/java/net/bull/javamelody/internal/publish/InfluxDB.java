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
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.charset.Charset;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.Collections;
import java.util.Locale;
import java.util.Map;

import net.bull.javamelody.Parameter;
import net.bull.javamelody.internal.common.LOG;
import net.bull.javamelody.internal.model.LabradorRetriever;

/**
 * Publish chart data to <a href='https://www.influxdata.com/time-series-platform/'>InfluxDB</a>.
 * @author Emeric Vernat
 */
class InfluxDB extends MetricsPublisher {
	private static final char SEPARATOR = ' ';

	private final URL influxDbUrl;
	private final String prefix;
	private final String tags;

	private final Map<String, String> httpHeaders = Collections.singletonMap("Content-Type",
			"plain/text");
	private final DecimalFormat decimalFormat = new DecimalFormat("0.00",
			DecimalFormatSymbols.getInstance(Locale.US));
	private final ByteArrayOutputStream buffer = new ByteArrayOutputStream();
	private final Writer bufferWriter = new OutputStreamWriter(buffer, Charset.forName("UTF-8"));
	private long lastTime;
	private String lastTimestamp;

	InfluxDB(URL influxDbUrl, String prefix, String tags) {
		super();
		assert influxDbUrl != null;
		assert prefix != null;
		assert tags != null;
		this.influxDbUrl = influxDbUrl;
		this.prefix = prefix;
		this.tags = tags;
	}

	static InfluxDB getInstance(String contextPath, String hostName) {
		final String influxDbUrl = Parameter.INFLUXDB_URL.getValue();
		if (influxDbUrl != null) {
			assert contextPath != null;
			assert hostName != null;
			// contextPath est du genre "/testapp"
			// hostName est du genre "www.host.com"

			final String prefix = "javamelody.";
			// if needed, we could also replace "," and "=" in tags
			// see https://docs.influxdata.com/influxdb/v1.3/write_protocols/line_protocol_reference/#special-characters
			final String tags = (",application=" + contextPath + ",host=" + hostName)
					.replace(SEPARATOR, '_');
			try {
				// timestamps will be written with a precision of a second
				return new InfluxDB(new URL(influxDbUrl + "&precision=s"), prefix, tags);
			} catch (final MalformedURLException e) {
				throw new IllegalArgumentException(e);
			}
		}
		return null;
	}

	@Override
	public synchronized void addValue(String metric, double value) throws IOException {
		// ex curl -i -XPOST 'http://localhost:8086/write?db=mydb&precision=s' --data-binary
		// 'cpu_load_short,direction=in,host=server01,region=us-west value=2.0 1422568543702'
		final long timeInSeconds = System.currentTimeMillis() / 1000;
		if (lastTime != timeInSeconds) {
			lastTimestamp = String.valueOf(timeInSeconds);
			lastTime = timeInSeconds;
		}
		bufferWriter.append(prefix).append(metric).append(tags).append(SEPARATOR);
		bufferWriter.append("value=").append(decimalFormat.format(value)).append(SEPARATOR);
		bufferWriter.append(lastTimestamp).append('\n');
	}

	@Override
	public synchronized void send() throws IOException {
		try {
			bufferWriter.flush();
			// the stream could be compressed in gzip, with Content-Encoding=gzip
			new LabradorRetriever(influxDbUrl, httpHeaders).post(buffer);
		} catch (final Exception e) {
			LOG.warn(e.toString(), e);
		} finally {
			// finally to be sure to not keep too much data in buffer
			// including when the http url can't connect
			buffer.reset();
		}
	}

	@Override
	public void stop() {
		// nothing
	}
}
