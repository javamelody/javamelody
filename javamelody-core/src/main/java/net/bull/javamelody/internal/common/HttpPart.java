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
 * Enumération des valeurs pour le paramètre "part" dans les requêtes http.
 * @author Emeric Vernat
 */
public enum HttpPart {
	HEAP_HISTO("heaphisto"),
	PROCESSES("processes"),
	CURRENT_REQUESTS("currentRequests"),
	DEFAULT_WITH_CURRENT_REQUESTS("defaultWithCurrentRequests"),
	WEB_XML("web.xml"),
	POM_XML("pom.xml"),
	JNLP("jnlp"),
	JVM("jvm"),
	SESSIONS("sessions"),
	HOTSPOTS("hotspots"),
	DATABASE("database"),
	CONNECTIONS("connections"),
	GRAPH("graph"),
	LAST_VALUE("lastValue"),
	USAGES("usages"),
	JNDI("jndi"),
	MBEANS("mbeans"),
	CRASHES("crashes"),
	THREADS("threads"),
	THREADS_DUMP("threadsDump"),
	COUNTER_SUMMARY_PER_CLASS("counterSummaryPerClass"),
	RUNTIME_DEPENDENCIES("runtimeDependencies"),
	JROBINS("jrobins"),
	OTHER_JROBINS("otherJRobins"),
	EXPLAIN_PLAN("explainPlan"),
	APPLICATIONS("applications"),
	SOURCE("source"),
	DEPENDENCIES("dependencies"),
	SPRING_BEANS("springBeans"),
	RUM("rum"),
	WEBAPP_VERSIONS("webappVersions"),
	CACHE_KEYS("cacheKeys"),
	JCACHE_KEYS("jcacheKeys");

	private final String name;

	HttpPart(String name) {
		this.name = name;
	}

	public boolean isPart(HttpServletRequest request) {
		return name.equals(HttpParameter.PART.getParameterFrom(request));
	}

	public String getName() {
		return name;
	}

	@Override
	public String toString() {
		return name;
	}

	public static HttpPart getByName(String name) {
		for (final HttpPart httpPart : values()) {
			if (httpPart.name.equals(name)) {
				return httpPart;
			}
		}
		throw new IllegalArgumentException("Unknown part: " + name);
	}
}
