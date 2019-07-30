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
package net.bull.javamelody.internal.model; // NOPMD

import java.io.IOException;
import java.io.Serializable;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import net.bull.javamelody.internal.common.HttpParameter;
import net.bull.javamelody.internal.common.HttpPart;
import net.bull.javamelody.internal.common.I18N;
import net.bull.javamelody.internal.model.SamplingProfiler.SampledMethod;

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
		if (url.indexOf('?') == -1) {
			this.url = new URL(url + "?format=serialized");
		} else {
			this.url = new URL(url + "&format=serialized");
		}
	}

	// utilisée dans scripts Jenkins par exemple
	// see https://github.com/javamelody/javamelody/wiki/ScriptsAndAlerts
	JavaInformations collectJavaInformations() throws IOException {
		final URL jvmUrl = new URL(url.toString() + '&' + HttpParameter.PART + '=' + HttpPart.JVM);
		final List<JavaInformations> list = collectForUrl(jvmUrl);
		return list.get(0);
	}

	// utilisée dans scripts Jenkins par exemple
	String collectMBeanAttribute(String jmxValueParameter) throws IOException {
		final URL mbeanAttributeUrl = new URL(
				url.toString() + '&' + HttpParameter.JMX_VALUE + '=' + jmxValueParameter);
		return collectForUrl(mbeanAttributeUrl);
	}

	// utilisée dans scripts Jenkins par exemple
	double collectGraphLastValue(String graph) throws IOException {
		final URL lastValueUrl = new URL(url.toString() + '&' + HttpParameter.PART + '='
				+ HttpPart.LAST_VALUE + '&' + HttpParameter.GRAPH + '=' + graph);
		return collectForUrl(lastValueUrl);
	}

	// result contains statistics in instances of Counter and also an instance of JavaInformations
	List<Serializable> collectData() throws IOException {
		return collectForUrl(url);
	}

	// pourrait être utilisée dans scripts Jenkins par exemple
	List<Serializable> executeActionAndCollectData(Action action, String counterName,
			String sessionId, String threadId, String jobId, String cacheId) throws IOException {
		assert action != null;
		final URL actionUrl = getActionUrl(action, counterName, sessionId, threadId, jobId,
				cacheId);
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
			final URL sessionsUrl = new URL(
					url.toString() + '&' + HttpParameter.PART + '=' + HttpPart.SESSIONS);
			return collectForUrl(sessionsUrl);
		}
		final URL sessionsUrl = new URL(url.toString() + '&' + HttpParameter.PART + '='
				+ HttpPart.SESSIONS + '&' + HttpParameter.SESSION_ID + '=' + sessionId);
		final SessionInformations session = collectForUrl(sessionsUrl);
		if (session != null) {
			return Collections.singletonList(session);
		}
		// si session est null, alors la session a été invalidée
		return Collections.emptyList();
	}

	List<SampledMethod> collectHotspots() throws IOException {
		// récupération à la demande des hotspots
		final URL hotspotsUrl = new URL(
				url.toString() + '&' + HttpParameter.PART + '=' + HttpPart.HOTSPOTS);
		return collectForUrl(hotspotsUrl);
	}

	HeapHistogram collectHeapHistogram() throws IOException {
		// récupération à la demande de HeapHistogram
		final URL heapHistoUrl = new URL(
				url.toString() + '&' + HttpParameter.PART + '=' + HttpPart.HEAP_HISTO);
		return collectForUrl(heapHistoUrl);
	}

	DatabaseInformations collectDatabaseInformations(int requestIndex) throws IOException {
		final URL databaseUrl = new URL(url.toString() + '&' + HttpParameter.PART + '='
				+ HttpPart.DATABASE + '&' + HttpParameter.REQUEST + '=' + requestIndex);
		return collectForUrl(databaseUrl);
	}

	@SuppressWarnings("unchecked")
	List<List<ConnectionInformations>> collectConnectionInformations() throws IOException {
		// récupération à la demande des connections
		final List<List<ConnectionInformations>> connectionInformations = new ArrayList<List<ConnectionInformations>>();
		final URL connectionsUrl = new URL(
				url.toString() + '&' + HttpParameter.PART + '=' + HttpPart.CONNECTIONS);
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
		final URL processUrl = new URL(
				url.toString() + '&' + HttpParameter.PART + '=' + HttpPart.PROCESSES);
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
		final URL jndiUrl = new URL(url.toString() + '&' + HttpParameter.PART + '=' + HttpPart.JNDI
				+ (path != null ? '&' + HttpParameter.PATH.toString() + '=' + path : ""));
		return collectForUrl(jndiUrl);
	}

	@SuppressWarnings("unchecked")
	Map<String, List<MBeanNode>> collectMBeans() throws IOException {
		// récupération à la demande des MBeans
		final String title = I18N.getString("MBeans");
		final Map<String, List<MBeanNode>> mbeansByTitle = new LinkedHashMap<String, List<MBeanNode>>();
		final URL mbeansUrl = new URL(
				url.toString() + '&' + HttpParameter.PART + '=' + HttpPart.MBEANS);
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

	Map<String, MavenArtifact> collectWebappDependencies() throws IOException {
		// récupération à la demande des dépendances
		final URL dependenciesUrl = new URL(
				url.toString() + '&' + HttpParameter.PART + '=' + HttpPart.DEPENDENCIES);
		return collectForUrl(dependenciesUrl);
	}

	Map<String, Date> collectWebappVersions() throws IOException {
		final URL webappVersionsUrl = new URL(
				url.toString() + '&' + HttpParameter.PART + '=' + HttpPart.WEBAPP_VERSIONS);
		return collectForUrl(webappVersionsUrl);
	}

	Map<JavaInformations, List<CounterRequestContext>> collectCurrentRequests() throws IOException {
		// récupération à la demande des requêtes en cours
		final URL currentRequestsUrl = new URL(
				url.toString() + '&' + HttpParameter.PART + '=' + HttpPart.CURRENT_REQUESTS);
		return collectForUrl(currentRequestsUrl);
	}

	Map<String, byte[]> collectJRobins(int width, int height) throws IOException {
		final URL jrobinNamesUrl = new URL(url.toString() + '&' + HttpParameter.PART + '='
				+ HttpPart.JROBINS + '&' + HttpParameter.WIDTH + '=' + width + '&'
				+ HttpParameter.HEIGHT + '=' + height);
		return collectForUrl(jrobinNamesUrl);
	}

	Map<String, byte[]> collectOtherJRobins(int width, int height) throws IOException {
		final URL otherJRobinNamesUrl = new URL(url.toString() + '&' + HttpParameter.PART + '='
				+ HttpPart.OTHER_JROBINS + '&' + HttpParameter.WIDTH + '=' + width + '&'
				+ HttpParameter.HEIGHT + '=' + height);
		return collectForUrl(otherJRobinNamesUrl);
	}

	byte[] collectJRobin(String graphName, int width, int height) throws IOException {
		final URL jrobinUrl = new URL(url.toString() + '&' + HttpParameter.GRAPH + '=' + graphName
				+ '&' + HttpParameter.PART + '=' + HttpPart.JROBINS + '&' + HttpParameter.WIDTH
				+ '=' + width + '&' + HttpParameter.HEIGHT + '=' + height);
		return collectForUrl(jrobinUrl);
	}

	String collectSqlRequestExplainPlan(String sqlRequest) throws IOException {
		final URL explainPlanUrl = new URL(
				url.toString() + '&' + HttpParameter.PART + '=' + HttpPart.EXPLAIN_PLAN);
		final Map<String, String> headers = new HashMap<String, String>();
		headers.put(HttpParameter.REQUEST.getName(), sqlRequest);
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
