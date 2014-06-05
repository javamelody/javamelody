/*
 * Copyright 2008-2014 by Emeric Vernat
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
package net.bull.javamelody;

/**
 * Paramètres des requêtes http.
 * @author Emeric Vernat
 */
final class HttpParameters {
	static final String HTML_CHARSET = "UTF-8";
	static final String HTML_CONTENT_TYPE = "text/html; charset=" + HTML_CHARSET;
	static final String TEXT_CONTENT_TYPE = "text/plain; charset=" + HTML_CHARSET;
	static final String CONTENT_DISPOSITION = "Content-Disposition";
	static final String ACTION_PARAMETER = "action";
	static final String PART_PARAMETER = "part";
	static final String PERIOD_PARAMETER = "period";
	static final String SESSION_ID_PARAMETER = "sessionId";
	static final String THREAD_ID_PARAMETER = "threadId";
	static final String JOB_ID_PARAMETER = "jobId";
	static final String CACHE_ID_PARAMETER = "cacheId";
	static final String COLLECTOR_PARAMETER = "collector";
	static final String REQUEST_PARAMETER = "request";
	static final String PATH_PARAMETER = "path";
	static final String COUNTER_PARAMETER = "counter";
	static final String GRAPH_PARAMETER = "graph";
	static final String RESOURCE_PARAMETER = "resource";
	static final String FORMAT_PARAMETER = "format";
	static final String HTML_BODY_FORMAT = "htmlbody";
	static final String WIDTH_PARAMETER = "width";
	static final String HEIGHT_PARAMETER = "height";
	static final String MAX_PARAMETER = "max";
	static final String REPORT_PARAMETER = "report";
	static final String HEAP_HISTO_PART = "heaphisto";
	static final String PROCESSES_PART = "processes";
	static final String CURRENT_REQUESTS_PART = "currentRequests";
	static final String DEFAULT_WITH_CURRENT_REQUESTS_PART = "defaultWithCurrentRequests";
	static final String WEB_XML_PART = "web.xml";
	static final String POM_XML_PART = "pom.xml";
	static final String JNLP_PART = "jnlp";
	static final String SESSIONS_PART = "sessions";
	static final String HOTSPOTS_PART = "hotspots";
	static final String DATABASE_PART = "database";
	static final String CONNECTIONS_PART = "connections";
	static final String GRAPH_PART = "graph";
	static final String LAST_VALUE_PART = "lastValue";
	static final String JMX_VALUE = "jmxValue";
	static final String USAGES_PART = "usages";
	static final String JNDI_PART = "jndi";
	static final String MBEANS_PART = "mbeans";
	static final String THREADS_PART = "threads";
	static final String THREADS_DUMP_PART = "threadsDump";
	static final String COUNTER_SUMMARY_PER_CLASS_PART = "counterSummaryPerClass";
	static final String RUNTIME_DEPENDENCIES_PART = "runtimeDependencies";
	static final String JROBINS_PART = "jrobins";
	static final String OTHER_JROBINS_PART = "otherJRobins";
	static final String EXPLAIN_PLAN_PART = "explainPlan";
	static final String APPLICATIONS_PART = "applications";
	static final String DESKTOP_JAR_PART = "desktopJar";

	/**
	 * Constructeur privé: pas d'instance.
	 */
	private HttpParameters() {
		super();
	}
}
