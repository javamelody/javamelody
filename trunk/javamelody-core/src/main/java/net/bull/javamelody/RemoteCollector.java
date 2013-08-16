/*
 * Copyright 2008-2012 by Emeric Vernat
 *
 *     This file is part of Java Melody.
 *
 * Java Melody is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Java Melody is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Java Melody.  If not, see <http://www.gnu.org/licenses/>.
 */
package net.bull.javamelody;

import static net.bull.javamelody.HttpParameters.CONNECTIONS_PART;
import static net.bull.javamelody.HttpParameters.CURRENT_REQUESTS_PART;
import static net.bull.javamelody.HttpParameters.DATABASE_PART;
import static net.bull.javamelody.HttpParameters.DEFAULT_WITH_CURRENT_REQUESTS_PART;
import static net.bull.javamelody.HttpParameters.EXPLAIN_PLAN_PART;
import static net.bull.javamelody.HttpParameters.GRAPH_PARAMETER;
import static net.bull.javamelody.HttpParameters.HEAP_HISTO_PART;
import static net.bull.javamelody.HttpParameters.HEIGHT_PARAMETER;
import static net.bull.javamelody.HttpParameters.HOTSPOTS_PART;
import static net.bull.javamelody.HttpParameters.JNDI_PART;
import static net.bull.javamelody.HttpParameters.JROBINS_PART;
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
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import net.bull.javamelody.SamplingProfiler.SampledMethod;

/**
 * Collecteur de données pour une application sur un ou plusieurs serveur(s) distant() : utilisé par serveur de collecte et par IHM Swing.
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
			final List<Serializable> serialized = collectForUrl(url);
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
			actionUrls.add(new URL(actionUrl.toString()));
		}
		return collectDataWithUrls(actionUrls);
	}

	List<SessionInformations> collectSessionInformations(String sessionId) throws IOException {
		// sessionId est null si on veut toutes les sessions
		if (sessionId == null) {
			// récupération à la demande des sessions
			final List<SessionInformations> sessionsInformations = new ArrayList<SessionInformations>();
			for (final URL url : urls) {
				final URL sessionsUrl = new URL(url.toString() + '&' + PART_PARAMETER + '='
						+ SESSIONS_PART);
				final List<SessionInformations> sessions = collectForUrl(sessionsUrl);
				sessionsInformations.addAll(sessions);
			}
			SessionListener.sortSessions(sessionsInformations);
			return sessionsInformations;
		}
		SessionInformations found = null;
		for (final URL url : urls) {
			final URL sessionsUrl = new URL(url.toString() + '&' + PART_PARAMETER + '='
					+ SESSIONS_PART + '&' + SESSION_ID_PARAMETER + '=' + sessionId);
			final SessionInformations session = collectForUrl(sessionsUrl);
			if (session != null) {
				found = session;
				break;
			}
		}
		if (found == null) {
			// si found est toujours null, alors la session a été invalidée
			return Collections.emptyList();
		}
		return Collections.singletonList(found);
	}

	List<SampledMethod> collectHotspots() throws IOException {
		// récupération à la demande des hotspots
		final Map<SampledMethod, SampledMethod> map = new HashMap<SampledMethod, SampledMethod>();
		for (final URL url : urls) {
			final URL hotspotsUrl = new URL(url.toString() + '&' + PART_PARAMETER + '='
					+ HOTSPOTS_PART);
			final List<SampledMethod> hotspots = collectForUrl(hotspotsUrl);
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
			final URL heapHistoUrl = new URL(url.toString() + '&' + PART_PARAMETER + '='
					+ HEAP_HISTO_PART);
			final HeapHistogram heapHisto = collectForUrl(heapHistoUrl);
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
		final URL databaseUrl = new URL(url.toString() + '&' + PART_PARAMETER + '=' + DATABASE_PART
				+ '&' + REQUEST_PARAMETER + '=' + requestIndex);
		return collectForUrl(databaseUrl);
	}

	@SuppressWarnings("unchecked")
	List<List<ConnectionInformations>> collectConnectionInformations() throws IOException {
		// récupération à la demande des connections
		final List<List<ConnectionInformations>> connectionInformations = new ArrayList<List<ConnectionInformations>>();
		for (final URL url : urls) {
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
		}
		return connectionInformations;
	}

	@SuppressWarnings("unchecked")
	Map<String, List<ProcessInformations>> collectProcessInformations() throws IOException {
		// récupération à la demande des processus
		final String title = I18N.getString("Processus");
		final Map<String, List<ProcessInformations>> processesByTitle = new LinkedHashMap<String, List<ProcessInformations>>();
		for (final URL url : urls) {
			final URL processUrl = new URL(url.toString() + '&' + PART_PARAMETER + '='
					+ PROCESSES_PART);
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
		}
		return processesByTitle;
	}

	List<JndiBinding> collectJndiBindings(String path) throws IOException {
		// récupération à la demande des bindings JNDI,
		// contrairement aux requêtes en cours ou aux processus, un serveur de l'application suffira
		// car l'arbre JNDI est en général identique dans tout l'éventuel cluster
		final URL url = urls.get(0);
		final URL jndiUrl = new URL(url.toString() + '&' + PART_PARAMETER + '=' + JNDI_PART
				+ (path != null ? '&' + PATH_PARAMETER + '=' + path : ""));
		return collectForUrl(jndiUrl);
	}

	@SuppressWarnings("unchecked")
	Map<String, List<MBeanNode>> collectMBeans() throws IOException {
		// récupération à la demande des MBeans
		final String title = I18N.getString("MBeans");
		final Map<String, List<MBeanNode>> mbeansByTitle = new LinkedHashMap<String, List<MBeanNode>>();
		for (final URL url : urls) {
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
		}
		return mbeansByTitle;
	}

	Map<JavaInformations, List<CounterRequestContext>> collectCurrentRequests() throws IOException {
		// récupération à la demande des requêtes en cours
		final Map<JavaInformations, List<CounterRequestContext>> requests = new LinkedHashMap<JavaInformations, List<CounterRequestContext>>();
		for (final URL url : urls) {
			final URL currentRequestsUrl = new URL(url.toString() + '&' + PART_PARAMETER + '='
					+ CURRENT_REQUESTS_PART);
			final Map<JavaInformations, List<CounterRequestContext>> result = collectForUrl(currentRequestsUrl);
			requests.putAll(result);
		}
		return requests;
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
		final URL jrobinNamesUrl = new URL(url.toString() + '&' + PART_PARAMETER + '='
				+ JROBINS_PART + '&' + WIDTH_PARAMETER + '=' + width + '&' + HEIGHT_PARAMETER + '='
				+ height);
		return collectForUrl(jrobinNamesUrl);
	}

	Map<String, byte[]> collectOtherJRobins(int width, int height) throws IOException {
		final URL url = urls.get(0);
		final URL otherJRobinNamesUrl = new URL(url.toString() + '&' + PART_PARAMETER + '='
				+ OTHER_JROBINS_PART + '&' + WIDTH_PARAMETER + '=' + width + '&' + HEIGHT_PARAMETER
				+ '=' + height);
		return collectForUrl(otherJRobinNamesUrl);
	}

	byte[] collectJRobin(String graphName, int width, int height) throws IOException {
		final URL url = urls.get(0);
		final URL jrobinUrl = new URL(url.toString() + '&' + GRAPH_PARAMETER + '=' + graphName
				+ '&' + PART_PARAMETER + '=' + JROBINS_PART + '&' + WIDTH_PARAMETER + '=' + width
				+ '&' + HEIGHT_PARAMETER + '=' + height);
		return collectForUrl(jrobinUrl);
	}

	String collectSqlRequestExplainPlan(String sqlRequest) throws IOException {
		final URL url = urls.get(0);
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

	private void addRequestsAndErrors(List<Counter> counters) {
		for (final Counter newCounter : counters) {
			final Counter counter = collector.getCounterByName(newCounter.getName());
			// counter.isDisplayed() peut changer pour spring, ejb ou services selon l'utilisation
			counter.setDisplayed(newCounter.isDisplayed());
			counter.addRequestsAndErrors(newCounter);
		}
	}

	private <T> T collectForUrl(URL url) throws IOException {
		final LabradorRetriever labradorRetriever;
		if (cookies != null) {
			final Map<String, String> headers = Collections.singletonMap("Cookie", cookies);
			labradorRetriever = new LabradorRetriever(url, headers);
		} else {
			labradorRetriever = new LabradorRetriever(url);
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
