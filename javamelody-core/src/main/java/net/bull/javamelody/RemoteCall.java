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
package net.bull.javamelody; // NOPMD

import static net.bull.javamelody.HttpParameters.CONNECTIONS_PART;
import static net.bull.javamelody.HttpParameters.CURRENT_REQUESTS_PART;
import static net.bull.javamelody.HttpParameters.DATABASE_PART;
import static net.bull.javamelody.HttpParameters.EXPLAIN_PLAN_PART;
import static net.bull.javamelody.HttpParameters.GRAPH_PARAMETER;
import static net.bull.javamelody.HttpParameters.HEAP_HISTO_PART;
import static net.bull.javamelody.HttpParameters.HEIGHT_PARAMETER;
import static net.bull.javamelody.HttpParameters.HOTSPOTS_PART;
import static net.bull.javamelody.HttpParameters.JMX_VALUE;
import static net.bull.javamelody.HttpParameters.JNDI_PART;
import static net.bull.javamelody.HttpParameters.JROBINS_PART;
import static net.bull.javamelody.HttpParameters.JVM_PART;
import static net.bull.javamelody.HttpParameters.LAST_VALUE_PART;
import static net.bull.javamelody.HttpParameters.MBEANS_PART;
import static net.bull.javamelody.HttpParameters.OTHER_JROBINS_PART;
import static net.bull.javamelody.HttpParameters.PART_PARAMETER;
import static net.bull.javamelody.HttpParameters.PATH_PARAMETER;
import static net.bull.javamelody.HttpParameters.PROCESSES_PART;
import static net.bull.javamelody.HttpParameters.REQUEST_PARAMETER;
import static net.bull.javamelody.HttpParameters.SESSIONS_PART;
import static net.bull.javamelody.HttpParameters.SESSION_ID_PARAMETER;
import static net.bull.javamelody.HttpParameters.WIDTH_PARAMETER;

import java.io.IOException;
import java.io.Serializable;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import net.bull.javamelody.SamplingProfiler.SampledMethod;

/**
 * Collecteur de données pour une application sur un serveur distant.
 * @author Emeric Vernat
 */
class RemoteCall {
	private final URL url;
	private String cookies;

	/**
	 * Constructeur.
	 * @param url URL
	 */
	RemoteCall(URL url) {
		super();
		assert url != null;
		this.url = url;
	}

	/**
	 * Constructeur.
	 * @param url String
	 * @throws MalformedURLException e
	 */
	RemoteCall(String url) throws MalformedURLException {
		super();
		assert url != null;
		this.url = new URL(url + "?format=serialized");
	}

	// utilisée dans scripts Jenkins par exemple
	JavaInformations collectJavaInformations() throws IOException {
		final URL jvmUrl = new URL(url.toString() + '&' + PART_PARAMETER + '=' + JVM_PART);
		final List<JavaInformations> list = collectForUrl(jvmUrl);
		return list.get(0);
	}

	// utilisée dans scripts Jenkins par exemple
	String collectMBeanAttribute(String jmxValueParameter) throws IOException {
		final URL mbeanAttributeUrl = new URL(url.toString() + '&' + JMX_VALUE + '='
				+ jmxValueParameter);
		return collectForUrl(mbeanAttributeUrl);
	}

	// utilisée dans scripts Jenkins par exemple
	double collectGraphLastValue(String graph) throws IOException {
		final URL lastValueUrl = new URL(url.toString() + '&' + PART_PARAMETER + '='
				+ LAST_VALUE_PART + '&' + GRAPH_PARAMETER + '=' + graph);
		return collectForUrl(lastValueUrl);
	}

	// result contains statistics in instances of Counter and also an instance of JavaInformations
	List<Serializable> collectData() throws IOException {
		return collectForUrl(url);
	}

	List<Serializable> executeActionAndCollectData(Action action, String counterName,
			String sessionId, String threadId, String jobId, String cacheId) throws IOException {
		assert action != null;
		final URL actionUrl = getActionUrl(action, counterName, sessionId, threadId, jobId, cacheId);
		return collectForUrl(actionUrl);
	}

	URL getActionUrl(Action action, String counterName, String sessionId, String threadId,
			String jobId, String cacheId) throws MalformedURLException {
		final StringBuilder actionUrl = new StringBuilder(url.toString());
		actionUrl.append("&action=").append(action);
		if (counterName != null) {
			actionUrl.append("&counter=").append(counterName);
		}
		if (sessionId != null) {
			actionUrl.append("&sessionId=").append(sessionId);
		}
		if (threadId != null) {
			actionUrl.append("&threadId=").append(threadId);
		}
		if (jobId != null) {
			actionUrl.append("&jobId=").append(jobId);
		}
		if (cacheId != null) {
			actionUrl.append("&cacheId=").append(cacheId);
		}
		return new URL(actionUrl.toString());
	}

	List<SessionInformations> collectSessionInformations(String sessionId) throws IOException {
		// sessionId est null si on veut toutes les sessions
		if (sessionId == null) {
			// récupération à la demande des sessions
			final URL sessionsUrl = new URL(url.toString() + '&' + PART_PARAMETER + '='
					+ SESSIONS_PART);
			return collectForUrl(sessionsUrl);
		}
		final URL sessionsUrl = new URL(url.toString() + '&' + PART_PARAMETER + '=' + SESSIONS_PART
				+ '&' + SESSION_ID_PARAMETER + '=' + sessionId);
		final SessionInformations session = collectForUrl(sessionsUrl);
		if (session != null) {
			return Collections.singletonList(session);
		}
		// si session est null, alors la session a été invalidée
		return Collections.emptyList();
	}

	List<SampledMethod> collectHotspots() throws IOException {
		// récupération à la demande des hotspots
		final URL hotspotsUrl = new URL(url.toString() + '&' + PART_PARAMETER + '=' + HOTSPOTS_PART);
		return collectForUrl(hotspotsUrl);
	}

	HeapHistogram collectHeapHistogram() throws IOException {
		// récupération à la demande de HeapHistogram
		final URL heapHistoUrl = new URL(url.toString() + '&' + PART_PARAMETER + '='
				+ HEAP_HISTO_PART);
		return collectForUrl(heapHistoUrl);
	}

	DatabaseInformations collectDatabaseInformations(int requestIndex) throws IOException {
		final URL databaseUrl = new URL(url.toString() + '&' + PART_PARAMETER + '=' + DATABASE_PART
				+ '&' + REQUEST_PARAMETER + '=' + requestIndex);
		return collectForUrl(databaseUrl);
	}

	@SuppressWarnings("unchecked")
	List<List<ConnectionInformations>> collectConnectionInformations() throws IOException {
		// récupération à la demande des connections
		final List<List<ConnectionInformations>> connectionInformations = new ArrayList<List<ConnectionInformations>>();
		final URL connectionsUrl = new URL(url.toString() + '&' + PART_PARAMETER + '='
				+ CONNECTIONS_PART);
		final Object result = collectForUrl(connectionsUrl);
		if (result instanceof List && !((List<?>) result).isEmpty()
				&& ((List<?>) result).get(0) instanceof List) {
			// pour le serveur de collecte
			final List<List<ConnectionInformations>> connections = (List<List<ConnectionInformations>>) result;
			connectionInformations.addAll(connections);
		} else {
			final List<ConnectionInformations> connections = (List<ConnectionInformations>) result;
			connectionInformations.add(connections);
		}
		return connectionInformations;
	}

	@SuppressWarnings("unchecked")
	Map<String, List<ProcessInformations>> collectProcessInformations() throws IOException {
		// récupération à la demande des processus
		final String title = I18N.getString("Processus");
		final Map<String, List<ProcessInformations>> processesByTitle = new LinkedHashMap<String, List<ProcessInformations>>();
		final URL processUrl = new URL(url.toString() + '&' + PART_PARAMETER + '=' + PROCESSES_PART);
		final Object result = collectForUrl(processUrl);
		if (result instanceof Map) {
			// pour le serveur de collecte et pour les nodes dans Jenkins
			final Map<String, List<ProcessInformations>> processByTitle = (Map<String, List<ProcessInformations>>) result;
			for (final Map.Entry<String, List<ProcessInformations>> entry : processByTitle
					.entrySet()) {
				String node = entry.getKey();
				if (!node.startsWith(title)) {
					// si serveur de collecte alors il y a déjà un titre, mais pas pour les nodes Jenkins
					node = title + " (" + entry.getKey() + ')';
				}
				final List<ProcessInformations> processList = entry.getValue();
				processesByTitle.put(node, processList);
			}
		} else {
			final List<ProcessInformations> processList = (List<ProcessInformations>) result;
			processesByTitle.put(title + " (" + getHostAndPort(url) + ')', processList);
		}
		return processesByTitle;
	}

	List<JndiBinding> collectJndiBindings(String path) throws IOException {
		// récupération à la demande des bindings JNDI,
		// contrairement aux requêtes en cours ou aux processus, un serveur de l'application suffira
		// car l'arbre JNDI est en général identique dans tout l'éventuel cluster
		final URL jndiUrl = new URL(url.toString() + '&' + PART_PARAMETER + '=' + JNDI_PART
				+ (path != null ? '&' + PATH_PARAMETER + '=' + path : ""));
		return collectForUrl(jndiUrl);
	}

	@SuppressWarnings("unchecked")
	Map<String, List<MBeanNode>> collectMBeans() throws IOException {
		// récupération à la demande des MBeans
		final String title = I18N.getString("MBeans");
		final Map<String, List<MBeanNode>> mbeansByTitle = new LinkedHashMap<String, List<MBeanNode>>();
		final URL mbeansUrl = new URL(url.toString() + '&' + PART_PARAMETER + '=' + MBEANS_PART);
		final Object result = collectForUrl(mbeansUrl);
		if (result instanceof Map) {
			// pour le serveur de collecte et les nodes dans Jenkins
			final Map<String, List<MBeanNode>> mbeansByNodeName = (Map<String, List<MBeanNode>>) result;
			for (final Map.Entry<String, List<MBeanNode>> entry : mbeansByNodeName.entrySet()) {
				String node = entry.getKey();
				if (!node.startsWith(title)) {
					// si serveur de collecte alors il y a déjà un titre, mais pas pour les nodes Jenkins
					node = title + " (" + entry.getKey() + ')';
				}
				final List<MBeanNode> mbeans = entry.getValue();
				mbeansByTitle.put(node, mbeans);
			}
		} else {
			final List<MBeanNode> mbeans = (List<MBeanNode>) result;
			mbeansByTitle.put(title + " (" + getHostAndPort(url) + ')', mbeans);
		}
		return mbeansByTitle;
	}

	Map<JavaInformations, List<CounterRequestContext>> collectCurrentRequests() throws IOException {
		// récupération à la demande des requêtes en cours
		final URL currentRequestsUrl = new URL(url.toString() + '&' + PART_PARAMETER + '='
				+ CURRENT_REQUESTS_PART);
		return collectForUrl(currentRequestsUrl);
	}

	Map<String, byte[]> collectJRobins(int width, int height) throws IOException {
		final URL jrobinNamesUrl = new URL(url.toString() + '&' + PART_PARAMETER + '='
				+ JROBINS_PART + '&' + WIDTH_PARAMETER + '=' + width + '&' + HEIGHT_PARAMETER + '='
				+ height);
		return collectForUrl(jrobinNamesUrl);
	}

	Map<String, byte[]> collectOtherJRobins(int width, int height) throws IOException {
		final URL otherJRobinNamesUrl = new URL(url.toString() + '&' + PART_PARAMETER + '='
				+ OTHER_JROBINS_PART + '&' + WIDTH_PARAMETER + '=' + width + '&' + HEIGHT_PARAMETER
				+ '=' + height);
		return collectForUrl(otherJRobinNamesUrl);
	}

	byte[] collectJRobin(String graphName, int width, int height) throws IOException {
		final URL jrobinUrl = new URL(url.toString() + '&' + GRAPH_PARAMETER + '=' + graphName
				+ '&' + PART_PARAMETER + '=' + JROBINS_PART + '&' + WIDTH_PARAMETER + '=' + width
				+ '&' + HEIGHT_PARAMETER + '=' + height);
		return collectForUrl(jrobinUrl);
	}

	String collectSqlRequestExplainPlan(String sqlRequest) throws IOException {
		final URL explainPlanUrl = new URL(url.toString() + '&' + PART_PARAMETER + '='
				+ EXPLAIN_PLAN_PART);
		final Map<String, String> headers = new HashMap<String, String>();
		headers.put(REQUEST_PARAMETER, sqlRequest);
		if (cookies != null) {
			headers.put("Cookie", cookies);
		}
		final LabradorRetriever labradorRetriever = new LabradorRetriever(explainPlanUrl, headers);
		return labradorRetriever.call();
	}

	private <T> T collectForUrl(URL myUrl) throws IOException {
		final LabradorRetriever labradorRetriever;
		if (cookies != null) {
			final Map<String, String> headers = Collections.singletonMap("Cookie", cookies);
			labradorRetriever = new LabradorRetriever(myUrl, headers);
		} else {
			labradorRetriever = new LabradorRetriever(myUrl);
		}
		return labradorRetriever.call();
	}

	static String getHostAndPort(URL url) {
		if (url.getPort() != -1) {
			return url.getHost() + ':' + url.getPort();
		}
		// port est -1 si c'est le port par défaut (80)
		return url.getHost();
	}

	URL getURL() {
		return url;
	}

	void setCookies(String cookies) {
		// cookies peut être null
		this.cookies = cookies;
	}
}
