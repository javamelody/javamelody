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
package net.bull.javamelody.internal.common;

import javax.servlet.http.HttpServletRequest;

/**
 * Enumération des paramètres dans les requêtes http.
 * @author Emeric Vernat
 */
public enum HttpParameter {
	ACTION("action"),
	PART("part"),
	PERIOD("period"),
	COUNTER("counter"),
	GRAPH("graph"),
	SESSION_ID("sessionId"),
	THREAD_ID("threadId"),
	JOB_ID("jobId"),
	CACHE_ID("cacheId"),
	CACHE_KEY("cacheKey"),
	REQUEST("request"),
	PATH("path"),
	JMX_VALUE("jmxValue"),
	COLLECTOR("collector"),
	RESOURCE("resource"),
	FORMAT("format"),
	WIDTH("width"),
	HEIGHT("height"),
	MAX("max"),
	PATTERN("pattern"),
	REPORT("report"),
	TOKEN("token"),
	CLASS("class"),
	APPLICATION("application");

	private final String name;

	HttpParameter(String name) {
		this.name = name;
	}

	public String getParameterFrom(HttpServletRequest request) {
		return request.getParameter(name);
	}

	public String getName() {
		return name;
	}

	@Override
	public String toString() {
		return name;
	}
}
