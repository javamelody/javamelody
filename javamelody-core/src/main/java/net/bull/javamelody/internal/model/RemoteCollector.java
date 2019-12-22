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
package net.bull.javamelody.internal.model;

import java.io.IOException;
import java.io.Serializable;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import net.bull.javamelody.SessionListener;
import net.bull.javamelody.internal.common.Parameters;
import net.bull.javamelody.internal.model.SamplingProfiler.SampledMethod;

/**
 * Collecteur de données pour une application sur un ou plusieurs serveur(s) distant(s) :
 * utilisé par serveur de collecte et par IHM Swing.
 * @author Emeric Vernat
 */
public class RemoteCollector {
	private final String application;
	private List<URL> urls;
	private Collector collector;
	private List<JavaInformations> javaInformationsList;
	private Map<JavaInformations, List<CounterRequestContext>> currentRequests;
	private final List<Counter> newCounters = new ArrayList<Counter>();
	private List<RemoteCollector> remoteCollectors;
	private final boolean aggregatedApplication;
	private final boolean aggregationApplication;
	private String cookies;
	private boolean aggregationDisabled;

	/**
	 * Constructeur.
	 * @param application Nom de l'application
	 * @param urls URLs
	 * @throws IOException e
	 */
	public RemoteCollector(String application, List<URL> urls) throws IOException {
		super();
		assert application != null;
		assert urls != null;
		this.application = application;
		this.urls = urls;
		this.aggregatedApplication = isAggregatedApplication();
		this.aggregationApplication = isAggregationApplication();
	}

	String collectData() throws IOException {
		return collectDataWithUrls(getURLs());
	}

	public String collectDataIncludingCurrentRequests() throws IOException {
		final List<URL> urlsWithCurrentRequests = new ArrayList<URL>();
		for (final URL url : getURLs()) {
			urlsWithCurrentRequests.add(new URL(url + "&part=defaultWithCurrentRequests"));
		}
		return collectDataWithUrls(urlsWithCurrentRequests);
	}

	private String collectDataWithUrls(List<URL> urlsForCollect) throws IOException {
		final List<JavaInformations> javaInfosList = new ArrayList<JavaInformations>();
		final Map<JavaInformations, List<CounterRequestContext>> counterRequestContextsByJavaInformations = new HashMap<JavaInformations, List<CounterRequestContext>>();
		final StringBuilder sb = new StringBuilder();
		IOException exception = null;
		if (!aggregationApplication) {
			for (final URL url : urlsForCollect) {
				try {
					final List<Counter> counters = new ArrayList<Counter>();
					final List<Serializable> serialized = createRemoteCall(url).collectData();
					dispatchSerializables(serialized, counters, javaInfosList,
							counterRequestContextsByJavaInformations, sb);
					addRequestsAndErrors(counters);
					if (aggregatedApplication) {
						newCounters.addAll(counters);
					}
				} catch (final IOException e) {
					exception = e;
					// if a node of the application is no longer reachable, collect data for the others
					continue;
				}
			}
		} else {
			assert remoteCollectors != null;
			for (final RemoteCollector remoteCollector : remoteCollectors) {
				javaInfosList.addAll(remoteCollector.getJavaInformationsList());
				counterRequestContextsByJavaInformations
						.putAll(remoteCollector.getCurrentRequests());
				final List<Counter> counters = remoteCollector.getNewCounters();
				if (counters.isEmpty()) {
					// lors de la première collecte, il faut récupérer les counters et pas seulement les deltas
					counters.addAll(remoteCollector.getCollector().getCounters());
				}
				addRequestsAndErrors(counters);
				if (aggregatedApplication) {
					newCounters.addAll(counters);
				}
				// les nouveaux counters ont été aggrégés dans cette application, on peut les oublier
				counters.clear();
			}
		}
		if (exception != null && javaInfosList.isEmpty()) {
			// but if there is no node reachable, throw the exception to say that the application is not available
			throw exception;
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

	private void dispatchSerializables(List<Serializable> serialized, List<Counter> counters,
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

	private boolean isAggregatedApplication() throws IOException {
		for (final List<String> applications : Parameters.getApplicationsByAggregationApplication()
				.values()) {
			if (applications.contains(application)) {
				return true;
			}
		}
		return false;
	}

	final boolean isAggregationApplication() throws IOException {
		return Parameters.getApplicationsByAggregationApplication().containsKey(application);
	}

	public String executeActionAndCollectData(Action action, String counterName, String sessionId,
			String threadId, String jobId, String cacheId) throws IOException {
		assert action != null;
		final List<URL> actionUrls = new ArrayList<URL>(getURLs().size());
		for (final URL url : getURLs()) {
			final URL actionUrl = createRemoteCall(url).getActionUrl(action, counterName, sessionId,
					threadId, jobId, cacheId);
			actionUrls.add(actionUrl);
		}
		return collectDataWithUrls(actionUrls);
	}

	public List<SessionInformations> collectSessionInformations(String sessionId)
			throws IOException {
		// sessionId est null si on veut toutes les sessions
		if (sessionId == null) {
			// récupération à la demande des sessions
			final List<SessionInformations> sessionsInformations = new ArrayList<SessionInformations>();
			for (final URL url : getURLs()) {
				final List<SessionInformations> sessions = createRemoteCall(url)
						.collectSessionInformations(null);
				sessionsInformations.addAll(sessions);
			}
			SessionListener.sortSessions(sessionsInformations);
			return sessionsInformations;
		}
		for (final URL url : getURLs()) {
			final List<SessionInformations> sessions = createRemoteCall(url)
					.collectSessionInformations(sessionId);
			if (!sessions.isEmpty()) {
				return sessions;
			}
		}
		// session non trouvée, alors la session a été invalidée
		return Collections.emptyList();
	}

	public List<SampledMethod> collectHotspots() throws IOException {
		// récupération à la demande des hotspots
		final Map<SampledMethod, SampledMethod> map = new HashMap<SampledMethod, SampledMethod>();
		for (final URL url : getURLs()) {
			final List<SampledMethod> hotspots = createRemoteCall(url).collectHotspots();
			if (getURLs().size() == 1) {
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

	public HeapHistogram collectHeapHistogram() throws IOException {
		// récupération à la demande des HeapHistogram
		HeapHistogram heapHistoTotal = null;
		for (final URL url : getURLs()) {
			final HeapHistogram heapHisto = createRemoteCall(url).collectHeapHistogram();
			if (heapHistoTotal == null) {
				heapHistoTotal = heapHisto;
			} else {
				heapHistoTotal.add(heapHisto);
			}
		}
		return heapHistoTotal;
	}

	public DatabaseInformations collectDatabaseInformations(int requestIndex) throws IOException {
		final URL url = getURLs().get(0);
		return createRemoteCall(url).collectDatabaseInformations(requestIndex);
	}

	public List<List<ConnectionInformations>> collectConnectionInformations() throws IOException {
		// récupération à la demande des connections
		final List<List<ConnectionInformations>> connectionInformations = new ArrayList<List<ConnectionInformations>>();
		for (final URL url : getURLs()) {
			final List<List<ConnectionInformations>> connections = createRemoteCall(url)
					.collectConnectionInformations();
			connectionInformations.addAll(connections);
		}
		return connectionInformations;
	}

	public Map<String, List<ProcessInformations>> collectProcessInformations() throws IOException {
		// récupération à la demande des processus
		final Map<String, List<ProcessInformations>> result = new LinkedHashMap<String, List<ProcessInformations>>();
		for (final URL url : getURLs()) {
			final Map<String, List<ProcessInformations>> processesByTitle = createRemoteCall(url)
					.collectProcessInformations();
			result.putAll(processesByTitle);
		}
		return result;
	}

	public List<JndiBinding> collectJndiBindings(String path) throws IOException {
		// récupération à la demande des bindings JNDI,
		// contrairement aux requêtes en cours ou aux processus, un serveur de l'application suffira
		// car l'arbre JNDI est en général identique dans tout l'éventuel cluster
		final URL url = getURLs().get(0);
		return createRemoteCall(url).collectJndiBindings(path);
	}

	public Map<String, List<MBeanNode>> collectMBeans() throws IOException {
		// récupération à la demande des MBeans
		final Map<String, List<MBeanNode>> result = new LinkedHashMap<String, List<MBeanNode>>();
		for (final URL url : getURLs()) {
			final Map<String, List<MBeanNode>> mbeansByTitle = createRemoteCall(url)
					.collectMBeans();
			result.putAll(mbeansByTitle);
		}
		return result;
	}

	public Map<String, MavenArtifact> collectWebappDependencies() throws IOException {
		// récupération à la demande des dépendances,
		// contrairement aux requêtes en cours ou aux processus, un serveur de l'application suffira
		// car le résultat est identique dans tout l'éventuel cluster
		final URL url = getURLs().get(0);
		return createRemoteCall(url).collectWebappDependencies();
	}

	public Map<String, Date> collectWebappVersions() throws IOException {
		final URL url = getURLs().get(0);
		return createRemoteCall(url).collectWebappVersions();
	}

	public Map<JavaInformations, List<CounterRequestContext>> collectCurrentRequests()
			throws IOException {
		// récupération à la demande des requêtes en cours
		final Map<JavaInformations, List<CounterRequestContext>> result = new LinkedHashMap<JavaInformations, List<CounterRequestContext>>();
		for (final URL url : getURLs()) {
			final Map<JavaInformations, List<CounterRequestContext>> requests = createRemoteCall(
					url).collectCurrentRequests();
			result.putAll(requests);
		}
		return result;
	}

	List<List<ThreadInformations>> getThreadInformationsLists() {
		final List<List<ThreadInformations>> result = new ArrayList<List<ThreadInformations>>();
		for (final JavaInformations javaInformations : this.javaInformationsList) {
			result.add(new ArrayList<ThreadInformations>(
					javaInformations.getThreadInformationsList()));
		}
		return result;
	}

	public Map<String, byte[]> collectJRobins(int width, int height) throws IOException {
		final URL url = getURLs().get(0);
		return createRemoteCall(url).collectJRobins(width, height);
	}

	public Map<String, byte[]> collectOtherJRobins(int width, int height) throws IOException {
		final URL url = getURLs().get(0);
		return createRemoteCall(url).collectOtherJRobins(width, height);
	}

	public byte[] collectJRobin(String graphName, int width, int height) throws IOException {
		final URL url = getURLs().get(0);
		return createRemoteCall(url).collectJRobin(graphName, width, height);
	}

	public String collectSqlRequestExplainPlan(String sqlRequest) throws IOException {
		final URL url = getURLs().get(0);
		return createRemoteCall(url).collectSqlRequestExplainPlan(sqlRequest);
	}

	private void addRequestsAndErrors(List<Counter> counters) {
		if (this.collector == null || aggregationDisabled) {
			this.collector = new Collector(application, counters);
		} else {
			for (final Counter newCounter : counters) {
				final Counter counter = collector.getCounterByName(newCounter.getName());
				// counter.isDisplayed() peut changer pour spring, ejb ou services selon l'utilisation
				counter.setDisplayed(newCounter.isDisplayed());
				counter.addRequestsAndErrors(newCounter);
			}
		}
	}

	private RemoteCall createRemoteCall(URL url) {
		final RemoteCall remoteCall = new RemoteCall(url);
		remoteCall.setCookies(cookies);
		return remoteCall;
	}

	public static String getHostAndPort(URL url) {
		if (url.getPort() != -1) {
			return url.getHost() + ':' + url.getPort();
		}
		// port est -1 si c'est le port par défaut (80)
		return url.getHost();
	}

	public String getApplication() {
		return application;
	}

	public List<URL> getURLs() {
		if (aggregationApplication) {
			final List<URL> result = new ArrayList<URL>();
			for (final RemoteCollector remoteCollector : remoteCollectors) {
				result.addAll(remoteCollector.getURLs());
			}
			return result;
		}
		return urls;
	}

	public Collector getCollector() {
		return collector;
	}

	public List<JavaInformations> getJavaInformationsList() {
		return javaInformationsList;
	}

	public Map<JavaInformations, List<CounterRequestContext>> getCurrentRequests() {
		return currentRequests;
	}

	List<Counter> getNewCounters() {
		return newCounters;
	}

	void setRemoteCollectors(List<RemoteCollector> remoteCollectors) {
		this.remoteCollectors = remoteCollectors;
	}

	// cette méthode est utilisée dans l'ihm Swing
	public void setURLs(List<URL> newURLs) {
		assert newURLs != null;
		this.urls = newURLs;
	}

	// cette méthode est utilisée dans l'ihm Swing
	public void setCookies(String cookies) {
		// cookies peut être null
		this.cookies = cookies;
	}

	// cette méthode est utilisée dans l'ihm Swing
	public void disableAggregation() {
		aggregationDisabled = true;
	}
}
