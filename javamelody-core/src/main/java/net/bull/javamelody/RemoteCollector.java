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

import static net.bull.javamelody.HttpParameters.DEFAULT_WITH_CURRENT_REQUESTS_PART;
import static net.bull.javamelody.HttpParameters.PART_PARAMETER;

import java.io.IOException;
import java.io.Serializable;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import net.bull.javamelody.SamplingProfiler.SampledMethod;

/**
 * Collecteur de données pour une application sur un ou plusieurs serveur(s) distant(s) : utilisé par serveur de collecte et par IHM Swing.
 * @author Emeric Vernat
 */
class RemoteCollector {
	private final String application;
	private List<URL> urls;
	private Collector collector;
	private List<JavaInformations> javaInformationsList;
	private Map<JavaInformations, List<CounterRequestContext>> currentRequests;
	private String cookies;
	private boolean aggregationDisabled;

	/**
	 * Constructeur.
	 * @param application Nom de l'application
	 * @param urls URLs
	 */
	RemoteCollector(String application, List<URL> urls) {
		super();
		assert application != null;
		assert urls != null;
		this.application = application;
		this.urls = urls;
	}

	String collectData() throws IOException {
		return collectDataWithUrls(urls);
	}

	String collectDataIncludingCurrentRequests() throws IOException {
		final List<URL> urlsWithCurrentRequests = new ArrayList<URL>();
		for (final URL url : urls) {
			urlsWithCurrentRequests.add(new URL(url.toString() + '&' + PART_PARAMETER + '='
					+ DEFAULT_WITH_CURRENT_REQUESTS_PART));
		}
		return collectDataWithUrls(urlsWithCurrentRequests);
	}

	private String collectDataWithUrls(List<URL> urlsForCollect) throws IOException {
		final List<JavaInformations> javaInfosList = new ArrayList<JavaInformations>();
		final Map<JavaInformations, List<CounterRequestContext>> counterRequestContextsByJavaInformations = new HashMap<JavaInformations, List<CounterRequestContext>>();
		final StringBuilder sb = new StringBuilder();
		for (final URL url : urlsForCollect) {
			final List<Counter> counters = new ArrayList<Counter>();
			final List<Serializable> serialized = createRemoteCall(url).collectData();
			dispatchSerializables(serialized, counters, javaInfosList,
					counterRequestContextsByJavaInformations, sb);
			if (this.collector == null || aggregationDisabled) {
				this.collector = new Collector(application, counters);
			} else {
				addRequestsAndErrors(counters);
			}
		}
		this.javaInformationsList = javaInfosList;
		this.currentRequests = counterRequestContextsByJavaInformations;
		final String messageForReport;
		if (sb.length() == 0) {
			messageForReport = null;
		} else {
			messageForReport = sb.toString();
		}
		return messageForReport;
	}

	private void dispatchSerializables(
			List<Serializable> serialized,
			List<Counter> counters,
			List<JavaInformations> javaInfosList,
			Map<JavaInformations, List<CounterRequestContext>> counterRequestContextsByJavaInformations,
			StringBuilder sb) {
		JavaInformations latestJavaInformations = null;
		final List<CounterRequestContext> counterRequestContextsList = new ArrayList<CounterRequestContext>();
		for (final Serializable serializable : serialized) {
			if (serializable instanceof Counter) {
				final Counter counter = (Counter) serializable;
				counter.setApplication(application);
				counters.add(counter);
			} else if (serializable instanceof JavaInformations) {
				final JavaInformations newJavaInformations = (JavaInformations) serializable;
				latestJavaInformations = newJavaInformations;
				javaInfosList.add(newJavaInformations);
			} else if (serializable instanceof String) {
				sb.append(serializable).append('\n');
			} else if (serializable instanceof CounterRequestContext) {
				final CounterRequestContext counterRequestContext = (CounterRequestContext) serializable;
				counterRequestContextsList.add(counterRequestContext);
			}
		}
		if (!counterRequestContextsList.isEmpty()) {
			counterRequestContextsByJavaInformations.put(latestJavaInformations,
					counterRequestContextsList);
		}
	}

	String executeActionAndCollectData(Action action, String counterName, String sessionId,
			String threadId, String jobId, String cacheId) throws IOException {
		assert action != null;
		final List<URL> actionUrls = new ArrayList<URL>(urls.size());
		for (final URL url : urls) {
			final URL actionUrl = createRemoteCall(url).getActionUrl(action, counterName,
					sessionId, threadId, jobId, cacheId);
			actionUrls.add(actionUrl);
		}
		return collectDataWithUrls(actionUrls);
	}

	List<SessionInformations> collectSessionInformations(String sessionId) throws IOException {
		// sessionId est null si on veut toutes les sessions
		if (sessionId == null) {
			// récupération à la demande des sessions
			final List<SessionInformations> sessionsInformations = new ArrayList<SessionInformations>();
			for (final URL url : urls) {
				final List<SessionInformations> sessions = createRemoteCall(url)
						.collectSessionInformations(null);
				sessionsInformations.addAll(sessions);
			}
			SessionListener.sortSessions(sessionsInformations);
			return sessionsInformations;
		}
		for (final URL url : urls) {
			final List<SessionInformations> sessions = createRemoteCall(url)
					.collectSessionInformations(sessionId);
			if (!sessions.isEmpty()) {
				return sessions;
			}
		}
		// session non trouvée, alors la session a été invalidée
		return Collections.emptyList();
	}

	List<SampledMethod> collectHotspots() throws IOException {
		// récupération à la demande des hotspots
		final Map<SampledMethod, SampledMethod> map = new HashMap<SampledMethod, SampledMethod>();
		for (final URL url : urls) {
			final List<SampledMethod> hotspots = createRemoteCall(url).collectHotspots();
			if (urls.size() == 1) {
				// s'il n'y a qu'un serveur, inutile d'aller plus loin pour fusionner les données
				return hotspots;
			}
			for (final SampledMethod method : hotspots) {
				// SampledMethod implémente hashCode et equals
				final SampledMethod previous = map.get(method);
				if (previous == null) {
					map.put(method, method);
				} else {
					previous.setCount(previous.getCount() + method.getCount());
				}
			}
		}
		final List<SampledMethod> hotspots = new ArrayList<SampledMethod>(map.values());
		Collections.sort(hotspots);
		return hotspots;
	}

	HeapHistogram collectHeapHistogram() throws IOException {
		// récupération à la demande des HeapHistogram
		HeapHistogram heapHistoTotal = null;
		for (final URL url : urls) {
			final HeapHistogram heapHisto = createRemoteCall(url).collectHeapHistogram();
			if (heapHistoTotal == null) {
				heapHistoTotal = heapHisto;
			} else {
				heapHistoTotal.add(heapHisto);
			}
		}
		return heapHistoTotal;
	}

	DatabaseInformations collectDatabaseInformations(int requestIndex) throws IOException {
		final URL url = urls.get(0);
		return createRemoteCall(url).collectDatabaseInformations(requestIndex);
	}

	List<List<ConnectionInformations>> collectConnectionInformations() throws IOException {
		// récupération à la demande des connections
		final List<List<ConnectionInformations>> connectionInformations = new ArrayList<List<ConnectionInformations>>();
		for (final URL url : urls) {
			final List<List<ConnectionInformations>> connections = createRemoteCall(url)
					.collectConnectionInformations();
			connectionInformations.addAll(connections);
		}
		return connectionInformations;
	}

	Map<String, List<ProcessInformations>> collectProcessInformations() throws IOException {
		// récupération à la demande des processus
		final Map<String, List<ProcessInformations>> result = new LinkedHashMap<String, List<ProcessInformations>>();
		for (final URL url : urls) {
			final Map<String, List<ProcessInformations>> processesByTitle = createRemoteCall(url)
					.collectProcessInformations();
			result.putAll(processesByTitle);
		}
		return result;
	}

	List<JndiBinding> collectJndiBindings(String path) throws IOException {
		// récupération à la demande des bindings JNDI,
		// contrairement aux requêtes en cours ou aux processus, un serveur de l'application suffira
		// car l'arbre JNDI est en général identique dans tout l'éventuel cluster
		final URL url = urls.get(0);
		return createRemoteCall(url).collectJndiBindings(path);
	}

	Map<String, List<MBeanNode>> collectMBeans() throws IOException {
		// récupération à la demande des MBeans
		final Map<String, List<MBeanNode>> result = new LinkedHashMap<String, List<MBeanNode>>();
		for (final URL url : urls) {
			final Map<String, List<MBeanNode>> mbeansByTitle = createRemoteCall(url)
					.collectMBeans();
			result.putAll(mbeansByTitle);
		}
		return result;
	}

	Map<JavaInformations, List<CounterRequestContext>> collectCurrentRequests() throws IOException {
		// récupération à la demande des requêtes en cours
		final Map<JavaInformations, List<CounterRequestContext>> result = new LinkedHashMap<JavaInformations, List<CounterRequestContext>>();
		for (final URL url : urls) {
			final Map<JavaInformations, List<CounterRequestContext>> requests = createRemoteCall(
					url).collectCurrentRequests();
			result.putAll(requests);
		}
		return result;
	}

	List<List<ThreadInformations>> getThreadInformationsLists() {
		final List<List<ThreadInformations>> result = new ArrayList<List<ThreadInformations>>();
		for (final JavaInformations javaInformations : this.javaInformationsList) {
			result.add(new ArrayList<ThreadInformations>(javaInformations
					.getThreadInformationsList()));
		}
		return result;
	}

	Map<String, byte[]> collectJRobins(int width, int height) throws IOException {
		final URL url = urls.get(0);
		return createRemoteCall(url).collectJRobins(width, height);
	}

	Map<String, byte[]> collectOtherJRobins(int width, int height) throws IOException {
		final URL url = urls.get(0);
		return createRemoteCall(url).collectOtherJRobins(width, height);
	}

	byte[] collectJRobin(String graphName, int width, int height) throws IOException {
		final URL url = urls.get(0);
		return createRemoteCall(url).collectJRobin(graphName, width, height);
	}

	String collectSqlRequestExplainPlan(String sqlRequest) throws IOException {
		final URL url = urls.get(0);
		return createRemoteCall(url).collectSqlRequestExplainPlan(sqlRequest);
	}

	private void addRequestsAndErrors(List<Counter> counters) {
		for (final Counter newCounter : counters) {
			final Counter counter = collector.getCounterByName(newCounter.getName());
			// counter.isDisplayed() peut changer pour spring, ejb ou services selon l'utilisation
			counter.setDisplayed(newCounter.isDisplayed());
			counter.addRequestsAndErrors(newCounter);
		}
	}

	private RemoteCall createRemoteCall(URL url) {
		final RemoteCall remoteCall = new RemoteCall(url);
		remoteCall.setCookies(cookies);
		return remoteCall;
	}

	static String getHostAndPort(URL url) {
		if (url.getPort() != -1) {
			return url.getHost() + ':' + url.getPort();
		}
		// port est -1 si c'est le port par défaut (80)
		return url.getHost();
	}

	String getApplication() {
		return application;
	}

	List<URL> getURLs() {
		return urls;
	}

	Collector getCollector() {
		return collector;
	}

	List<JavaInformations> getJavaInformationsList() {
		return javaInformationsList;
	}

	Map<JavaInformations, List<CounterRequestContext>> getCurrentRequests() {
		return currentRequests;
	}

	// cette méthode est utilisée dans l'ihm Swing
	void setURLs(List<URL> newURLs) {
		assert urls != null;
		this.urls = newURLs;
	}

	// cette méthode est utilisée dans l'ihm Swing
	void setCookies(String cookies) {
		// cookies peut être null
		this.cookies = cookies;
	}

	// cette méthode est utilisée dans l'ihm Swing
	void disableAggregation() {
		aggregationDisabled = true;
	}
}
