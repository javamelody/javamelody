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
 * Publish chart data to <a href='https://www.datadoghq.com/'>Datadog</a>.
 * @author Emeric Vernat
 */
class Datadog extends MetricsPublisher {
	private static final String BEGIN_SERIES = "{ \"series\" : [";
	private static final String END_SERIES = "\n]}";
	private final URL datadogUrl;
	private final String prefix;
	private final String hostAndTags;

	private final Map<String, String> httpHeaders = Collections.singletonMap("Content-Type",
			"application/json");
	private final DecimalFormat decimalFormat = new DecimalFormat("0.00",
			DecimalFormatSymbols.getInstance(Locale.US));
	private final ByteArrayOutputStream buffer = new ByteArrayOutputStream();
	private final Writer bufferWriter = new OutputStreamWriter(buffer, Charset.forName("UTF-8"));
	private long lastTime;
	private String lastTimestamp;
	private boolean beginSeries;

	Datadog(String datadogApiKey, String prefix, String hostAndTags) {
		super();
		assert datadogApiKey != null;
		assert prefix != null;
		assert hostAndTags != null;
		try {
			this.datadogUrl = new URL(
					"https://app.datadoghq.com/api/v1/series?api_key=" + datadogApiKey);
		} catch (final MalformedURLException e) {
			throw new IllegalArgumentException(e);
		}
		this.prefix = prefix;
		this.hostAndTags = hostAndTags;
		try {
			bufferWriter.append(BEGIN_SERIES);
			beginSeries = true;
		} catch (final IOException e) {
			// can not happen
			throw new IllegalStateException(e);
		}
	}

	static Datadog getInstance(String contextPath, String hostName) {
		final String datadogApiKey = Parameter.DATADOG_API_KEY.getValue();
		if (datadogApiKey != null) {
			assert contextPath != null;
			assert hostName != null;
			// contextPath est du genre "/testapp"
			// hostName est du genre "www.host.com"

			final String prefix = "javamelody.";
			// see https://help.datadoghq.com/hc/en-us/articles/203764705-What-are-valid-metric-names-
			final String hostAndTags = "\"host\":\"" + hostName + "\",\"tags\":[\"application:"
					+ contextPath + "\"]";
			return new Datadog(datadogApiKey, prefix, hostAndTags);
		}
		return null;
	}

	@Override
	public synchronized void addValue(String metric, double value) throws IOException {
		/*
		https://docs.datadoghq.com/api/
		json example for timestamp now, host and tags are optional.
		"{ \"series\" :
		        [{\"metric\":\"page.views\",
		          \"points\":[[$currenttime, 1000]],
		          \"host\":\"myhost.example.com\",
		          \"tags\":[\"version:1\"]}
		        ]
		}"
		*/

		final long timeInSeconds = System.currentTimeMillis() / 1000;
		if (lastTime != timeInSeconds) {
			lastTimestamp = String.valueOf(timeInSeconds);
			lastTime = timeInSeconds;
		}
		if (beginSeries) {
			beginSeries = false;
		} else {
			bufferWriter.append(',');
		}
		bufferWriter.append("\n{\"metric\":\"").append(prefix).append(metric).append("\",");
		bufferWriter.append("\"points\":[[").append(lastTimestamp).append(',')
				.append(decimalFormat.format(value)).append("]],");
		bufferWriter.append(hostAndTags);
		bufferWriter.append('}');
	}

	@Override
	public synchronized void send() throws IOException {
		try {
			bufferWriter.append(END_SERIES);
			bufferWriter.flush();
			new LabradorRetriever(datadogUrl, httpHeaders).post(buffer);
		} catch (final Exception e) {
			LOG.warn(e.toString(), e);
		} finally {
			// finally to be sure to not keep too much data in buffer
			// including when the http url can't connect
			buffer.reset();
			bufferWriter.append(BEGIN_SERIES);
			beginSeries = true;
		}
	}

	@Override
	public void stop() {
		// nothing
	}
}
