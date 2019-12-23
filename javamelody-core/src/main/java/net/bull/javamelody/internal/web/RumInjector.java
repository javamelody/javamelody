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
package net.bull.javamelody.internal.web;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import net.bull.javamelody.internal.common.Parameters;
import net.bull.javamelody.internal.model.Counter;
import net.bull.javamelody.internal.model.CounterRequestContext;
import net.bull.javamelody.internal.web.HtmlInjectorResponseStream.HtmlToInject;

/**
 * Injection du script <a href='https://soasta.github.io/boomerang/doc/'>Boomerang</a>
 * avant le tag {@code </body>} pour le Real User Monitoring
 * (<a href='https://en.wikipedia.org/wiki/Real_user_monitoring'>RUM</a>).
 * @author Emeric Vernat
 */
public final class RumInjector implements HtmlToInject {
	private static final String BOOMERANG_FILENAME = "boomerang.min.js";

	private final long start = System.currentTimeMillis();
	private final HttpServletRequest httpRequest;
	private final String requestName;

	private RumInjector(HttpServletRequest httpRequest, String requestName) {
		super();
		this.httpRequest = httpRequest;
		this.requestName = requestName;
	}

	public static HttpServletResponse createRumResponseWrapper(HttpServletRequest httpRequest,
			HttpServletResponse httpResponse, String requestName) {
		if (HtmlInjectorServletResponseWrapper.acceptsRequest(httpRequest)) {
			final HtmlToInject htmlToInject = new RumInjector(httpRequest, requestName);
			return new HtmlInjectorServletResponseWrapper(httpRequest, httpResponse, htmlToInject);
		}
		return httpResponse;
	}

	private static String getRumUrlForBrowser(String requestName) {
		// for the RUM URL, we want the monitoring URL (<contextPath>/monitoring relative to the hostname),
		// but from the browser it may not be correct:
		// for example with an Apache proxy, the client may not have <contextPath> in the URL.
		// (And note that httpRequest.getRequestURI() and httpRequest.getRequestUrl() include the <contextPath>,
		// if there is an Apache proxy or not, so they do not help.)
		// So, based on the requestName, we use a relative monitoring URL for the browser whatever the <contextPath>

		// requestName is of the form "/path/to/file GET"
		// first remove " GET" part
		final int lastIndexOfSpace = requestName.lastIndexOf(' ');
		assert lastIndexOfSpace != -1;
		String tmp = requestName.substring(0, lastIndexOfSpace);
		// replace each subpath by ".."
		while (tmp.contains("//")) {
			tmp = tmp.replaceAll("//", "/");
		}
		tmp = tmp.replaceAll("/[^/]*", "/..");
		// remove first subpath
		if (tmp.startsWith("/..")) {
			tmp = tmp.substring(3);
		}
		String tmp2 = tmp + Parameters.getMonitoringPath();
		// remove first '/', including when tmp == Parameters.getMonitoringPath()
		while (tmp2.length() > 0 && tmp2.charAt(0) == '/') {
			tmp2 = tmp2.substring(1);
		}
		return tmp2;
	}

	@Override
	public String getContent() {
		final String httpRequestName = getHttpRequestName();
		final String rumUrl = getRumUrlForBrowser(httpRequestName);
		// approximation of server duration (may not be the real server duration, but not far in general)
		final long serverTime = System.currentTimeMillis() - start;
		return "\n<script src='" + rumUrl + "?resource=" + BOOMERANG_FILENAME
				+ "'></script>\n<script>BOOMR.init({beacon_url: '" + rumUrl
				+ "?part=rum', log: null});\nBOOMR.addVar('requestName', \"" + httpRequestName
				+ "\");\n" + "BOOMR.addVar('serverTime', " + serverTime + ");\n</script>\n";
	}

	private String getHttpRequestName() {
		return CounterRequestContext.getHttpRequestName(httpRequest, requestName);
	}

	@Override
	public String getBeforeTag() {
		// we suppose lowercase
		return "</body>";
	}

	public static boolean isRumResource(String resourceName) {
		return BOOMERANG_FILENAME.equals(resourceName);
	}

	public static void addRumHit(HttpServletRequest httpRequest, Counter httpCounter) {
		final String requestName = httpRequest.getParameter("requestName");
		if (requestName == null) {
			return;
		}
		try {
			final long serverTime = Long.parseLong(httpRequest.getParameter("serverTime"));
			final long timeToFirstByte = Long
					.parseLong(httpRequest.getParameter("timeToFirstByte"));
			final long domProcessing = Long.parseLong(httpRequest.getParameter("domProcessing"));
			final long pageRendering = Long.parseLong(httpRequest.getParameter("pageRendering"));
			final long networkTime = Math.max(timeToFirstByte - serverTime, 0);
			httpCounter.addRumHit(requestName, networkTime, domProcessing, pageRendering);
		} catch (final NumberFormatException e) {
			return;
		}

	}
}
