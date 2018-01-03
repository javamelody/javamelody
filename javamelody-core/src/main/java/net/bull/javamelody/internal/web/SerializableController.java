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
package net.bull.javamelody.internal.web; // NOPMD

import java.io.IOException;
import java.io.Serializable;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import javax.management.JMException;
import javax.naming.NamingException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import net.bull.javamelody.JdbcWrapper;
import net.bull.javamelody.SessionListener;
import net.bull.javamelody.internal.common.HttpParameter;
import net.bull.javamelody.internal.common.HttpPart;
import net.bull.javamelody.internal.common.I18N;
import net.bull.javamelody.internal.model.Action;
import net.bull.javamelody.internal.model.Collector;
import net.bull.javamelody.internal.model.ConnectionInformations;
import net.bull.javamelody.internal.model.Counter;
import net.bull.javamelody.internal.model.CounterRequest;
import net.bull.javamelody.internal.model.CounterRequestAggregation;
import net.bull.javamelody.internal.model.CounterRequestContext;
import net.bull.javamelody.internal.model.DatabaseInformations;
import net.bull.javamelody.internal.model.JRobin;
import net.bull.javamelody.internal.model.JavaInformations;
import net.bull.javamelody.internal.model.JndiBinding;
import net.bull.javamelody.internal.model.MBeanNode;
import net.bull.javamelody.internal.model.MBeans;
import net.bull.javamelody.internal.model.MavenArtifact;
import net.bull.javamelody.internal.model.Period;
import net.bull.javamelody.internal.model.ProcessInformations;
import net.bull.javamelody.internal.model.Range;
import net.bull.javamelody.internal.model.SamplingProfiler.SampledMethod;
import net.bull.javamelody.internal.model.SessionInformations;
import net.bull.javamelody.internal.model.ThreadInformations;
import net.bull.javamelody.internal.model.TransportFormat;
import net.bull.javamelody.internal.model.VirtualMachine;
import net.bull.javamelody.internal.web.RequestToMethodMapper.RequestAttribute;
import net.bull.javamelody.internal.web.RequestToMethodMapper.RequestParameter;
import net.bull.javamelody.internal.web.RequestToMethodMapper.RequestPart;

/**
 * Contrôleur au sens MVC pour la partie des données sérialisées.
 * @author Emeric Vernat
 */
public class SerializableController {
	private static final String RANGE_KEY = "range";
	private static final String JAVA_INFORMATIONS_LIST_KEY = "javaInformationsList";
	private static final String MESSAGE_FOR_REPORT_KEY = "messageForReport";
	private static final RequestToMethodMapper<SerializableController> REQUEST_TO_METHOD_MAPPER = new RequestToMethodMapper<SerializableController>(
			SerializableController.class);
	private final Collector collector;

	public SerializableController(Collector collector) {
		super();
		assert collector != null;
		this.collector = collector;
	}

	void doSerializable(HttpServletRequest httpRequest, HttpServletResponse httpResponse,
			Serializable serializable) throws IOException {
		// l'appelant (un serveur d'agrégation par exemple) peut appeler
		// la page monitoring avec un format "serialized" ou "xml" en paramètre
		// pour avoir les données au format sérialisé java ou xml
		final String format = HttpParameter.FORMAT.getParameterFrom(httpRequest);
		final TransportFormat transportFormat = TransportFormat.valueOfIgnoreCase(format);
		// checkDependencies avant setContentType pour afficher correctement les erreurs
		transportFormat.checkDependencies();
		httpResponse.setContentType(transportFormat.getMimeType());
		final String fileName = "JavaMelody_" + getApplication().replace(' ', '_').replace("/", "")
				+ '_' + I18N.getCurrentDate().replace('/', '_') + '.' + transportFormat.getCode();
		final String contentDisposition = "inline;filename=" + fileName;
		// encoding des CRLF pour http://en.wikipedia.org/wiki/HTTP_response_splitting
		httpResponse.addHeader("Content-Disposition",
				contentDisposition.replace('\n', '_').replace('\r', '_'));

		transportFormat.writeSerializableTo(serializable, httpResponse.getOutputStream());
	}

	public Serializable createSerializable(HttpServletRequest httpRequest,
			List<JavaInformations> javaInformationsList, String messageForReport)
			throws IOException {
		final Range range = getRangeForSerializable(httpRequest);
		if (HttpParameter.PART.getParameterFrom(httpRequest) != null) {
			httpRequest.setAttribute(JAVA_INFORMATIONS_LIST_KEY, javaInformationsList);
			httpRequest.setAttribute(RANGE_KEY, range);
			httpRequest.setAttribute(MESSAGE_FOR_REPORT_KEY, messageForReport);

			return (Serializable) REQUEST_TO_METHOD_MAPPER.invokeAndReturn(httpRequest, this);
		} else if (HttpParameter.JMX_VALUE.getParameterFrom(httpRequest) != null) {
			// par sécurité
			Action.checkSystemActionsEnabled();
			final String jmxValue = HttpParameter.JMX_VALUE.getParameterFrom(httpRequest);
			return MBeans.getConvertedAttributes(jmxValue);
		}

		return createDefaultSerializable(javaInformationsList, range, messageForReport);
	}

	@RequestPart(HttpPart.THREADS)
	Serializable createThreadsSerializable(
			@RequestAttribute(JAVA_INFORMATIONS_LIST_KEY) List<JavaInformations> javaInformationsList) {
		return new ArrayList<ThreadInformations>(
				javaInformationsList.get(0).getThreadInformationsList());
	}

	@RequestPart(HttpPart.COUNTER_SUMMARY_PER_CLASS)
	Serializable createCounterSummaryPerClassSerializable(@RequestAttribute(RANGE_KEY) Range range,
			@RequestParameter(HttpParameter.COUNTER) String counterName,
			@RequestParameter(HttpParameter.GRAPH) String requestId) throws IOException {
		final Counter counter = collector.getRangeCounter(range, counterName).clone();
		final List<CounterRequest> requestList = new CounterRequestAggregation(counter)
				.getRequestsAggregatedOrFilteredByClassName(requestId);
		return new ArrayList<CounterRequest>(requestList);
	}

	@RequestPart(HttpPart.CURRENT_REQUESTS)
	Serializable createCurrentRequestsSerializable(
			@RequestAttribute(JAVA_INFORMATIONS_LIST_KEY) List<JavaInformations> javaInformationsList) {
		final Map<JavaInformations, List<CounterRequestContext>> result = new HashMap<JavaInformations, List<CounterRequestContext>>();
		result.put(javaInformationsList.get(0), getCurrentRequests());
		return (Serializable) result;
	}

	@RequestPart(HttpPart.DEFAULT_WITH_CURRENT_REQUESTS)
	@SuppressWarnings("unchecked")
	Serializable createDefaultWithCurrentRequestsSerializable(
			@RequestAttribute(JAVA_INFORMATIONS_LIST_KEY) List<JavaInformations> javaInformationsList,
			@RequestAttribute(MESSAGE_FOR_REPORT_KEY) String messageForReport,
			@RequestAttribute(RANGE_KEY) Range range) throws IOException {
		final List<Serializable> result = new ArrayList<Serializable>();
		result.addAll((List<Serializable>) createDefaultSerializable(javaInformationsList, range,
				messageForReport));
		result.addAll(getCurrentRequests());
		return (Serializable) result;
	}

	@RequestPart(HttpPart.JVM)
	Serializable createJvmSerializable(
			@RequestAttribute(JAVA_INFORMATIONS_LIST_KEY) List<JavaInformations> javaInformationsList) {
		return new ArrayList<JavaInformations>(javaInformationsList);
	}

	@RequestPart(HttpPart.SESSIONS)
	Serializable createSessionsSerializable(
			@RequestParameter(HttpParameter.SESSION_ID) String sessionId) {
		// par sécurité
		Action.checkSystemActionsEnabled();
		if (sessionId == null) {
			return new ArrayList<SessionInformations>(SessionListener.getAllSessionsInformations());
		}
		return SessionListener.getSessionInformationsBySessionId(sessionId);
	}

	@RequestPart(HttpPart.HOTSPOTS)
	Serializable createHotspotsSerializable() {
		// par sécurité
		Action.checkSystemActionsEnabled();
		return new ArrayList<SampledMethod>(collector.getHotspots());
	}

	@RequestPart(HttpPart.HEAP_HISTO)
	Serializable createHeapHistoSerializable() throws Exception { // NOPMD
		// par sécurité
		Action.checkSystemActionsEnabled();
		return VirtualMachine.createHeapHistogram();
	}

	@RequestPart(HttpPart.PROCESSES)
	Serializable createProcessesSerializable() throws IOException {
		// par sécurité
		Action.checkSystemActionsEnabled();
		return new ArrayList<ProcessInformations>(ProcessInformations.buildProcessInformations());
	}

	@RequestPart(HttpPart.JNDI)
	Serializable createJndiSerializable(@RequestParameter(HttpParameter.PATH) String path)
			throws NamingException {
		// par sécurité
		Action.checkSystemActionsEnabled();
		return new ArrayList<JndiBinding>(JndiBinding.listBindings(path));
	}

	@RequestPart(HttpPart.MBEANS)
	Serializable createMBeansSerializable() throws JMException {
		// par sécurité
		Action.checkSystemActionsEnabled();
		return new ArrayList<MBeanNode>(MBeans.getAllMBeanNodes());
	}

	@RequestPart(HttpPart.DEPENDENCIES)
	Serializable createDependenciesSerializable() throws IOException {
		// par sécurité
		Action.checkSystemActionsEnabled();
		final Map<String, MavenArtifact> webappDependencies = MavenArtifact.getWebappDependencies();
		for (final MavenArtifact dependency : webappDependencies.values()) {
			if (dependency != null) {
				// preload licenses with parent of dependency when needed
				dependency.getLicenseUrlsByName();
			}
		}
		return new TreeMap<String, MavenArtifact>(webappDependencies);
	}

	@RequestPart(HttpPart.LAST_VALUE)
	Serializable createLastValueSerializable(@RequestParameter(HttpParameter.GRAPH) String graph)
			throws IOException {
		if (graph != null) {
			final JRobin jrobin = collector.getJRobin(graph);
			final double lastValue;
			if (jrobin == null) {
				lastValue = -1;
			} else {
				lastValue = jrobin.getLastValue();
			}
			return lastValue;
		}
		final Collection<JRobin> jrobins = collector.getDisplayedCounterJRobins();
		final Map<String, Double> lastValues = new LinkedHashMap<String, Double>(jrobins.size());
		for (final JRobin jrobin : jrobins) {
			lastValues.put(jrobin.getName(), jrobin.getLastValue());
		}
		return (Serializable) lastValues;
	}

	@RequestPart(HttpPart.DATABASE)
	Serializable createDatabaseSerializable(
			@RequestParameter(HttpParameter.REQUEST) String requestIndex)
			throws SQLException, NamingException {
		// par sécurité
		Action.checkSystemActionsEnabled();
		final int index = DatabaseInformations.parseRequestIndex(requestIndex);
		return new DatabaseInformations(index);
	}

	@RequestPart(HttpPart.CONNECTIONS)
	Serializable createConnectionsSerializable() {
		// par sécurité
		Action.checkSystemActionsEnabled();
		return new ArrayList<ConnectionInformations>(JdbcWrapper.getConnectionInformationsList());
	}

	@RequestPart(HttpPart.WEBAPP_VERSIONS)
	Serializable createWebappVersionsSerializable() {
		return new LinkedHashMap<String, Date>(collector.getDatesByWebappVersions());
	}

	@RequestPart(HttpPart.GRAPH)
	Serializable getCounterRequestById(@RequestParameter(HttpParameter.GRAPH) String requestId,
			@RequestAttribute(RANGE_KEY) Range range) throws IOException {
		for (final Counter counter : collector.getCounters()) {
			if (counter.isRequestIdFromThisCounter(requestId)) {
				final Counter rangeCounter = collector.getRangeCounter(range, counter.getName())
						.clone();
				for (final CounterRequest request : rangeCounter.getRequests()) {
					if (requestId.equals(request.getId())) {
						return request;
					}
				}
			}
		}
		// non trouvé
		return null;
	}

	@RequestPart(HttpPart.JROBINS)
	Serializable getJRobinsImages(@RequestAttribute(RANGE_KEY) Range range,
			@RequestParameter(HttpParameter.WIDTH) String width,
			@RequestParameter(HttpParameter.HEIGHT) String height,
			@RequestParameter(HttpParameter.GRAPH) String graphName) throws IOException {
		// pour UI Swing
		final int myWidth = Integer.parseInt(width);
		final int myHeight = Integer.parseInt(height);
		if (graphName != null) {
			final JRobin jrobin = collector.getJRobin(graphName);
			if (jrobin != null) {
				return jrobin.graph(range, myWidth, myHeight);
			}
			return null;
		}
		final Collection<JRobin> jrobins = collector.getDisplayedCounterJRobins();
		return (Serializable) convertJRobinsToImages(jrobins, range, myWidth, myHeight);
	}

	@RequestPart(HttpPart.OTHER_JROBINS)
	Serializable getOtherJRobinsImages(@RequestAttribute(RANGE_KEY) Range range,
			@RequestParameter(HttpParameter.WIDTH) String width,
			@RequestParameter(HttpParameter.HEIGHT) String height) throws IOException {
		// pour UI Swing
		final Collection<JRobin> jrobins = collector.getDisplayedOtherJRobins();
		return (Serializable) convertJRobinsToImages(jrobins, range, Integer.parseInt(width),
				Integer.parseInt(height));
	}

	@RequestPart(HttpPart.EXPLAIN_PLAN)
	Serializable createExplainPlanSerializableFor(
			@RequestParameter(HttpParameter.REQUEST) String sqlRequest) {
		// pour UI Swing
		assert sqlRequest != null;
		try {
			// retourne le plan d'exécution ou null si la base de données ne le permet pas (ie non Oracle)
			return DatabaseInformations.explainPlanFor(sqlRequest);
		} catch (final Exception ex) {
			return ex.toString();
		}
	}

	private List<CounterRequestContext> getCurrentRequests() {
		final List<Counter> counters = collector.getCounters();
		final List<Counter> newCounters = new ArrayList<Counter>();
		for (final Counter counter : counters) {
			final Counter cloneLight = new Counter(counter.getName(), counter.getStorageName(),
					counter.getIconName(), counter.getChildCounterName());
			newCounters.add(cloneLight);
		}

		// note: ces contextes ont été clonés dans getRootCurrentContexts(newCounters) par getOrderedRootCurrentContexts()
		return collector.getRootCurrentContexts(newCounters);
	}

	private Map<String, byte[]> convertJRobinsToImages(Collection<JRobin> jrobins, Range range,
			int width, int height) throws IOException {
		final Map<String, byte[]> images = new LinkedHashMap<String, byte[]>(jrobins.size());
		for (final JRobin jrobin : jrobins) {
			final byte[] image = jrobin.graph(range, width, height);
			images.put(jrobin.getName(), image);
		}
		return images;
	}

	public Serializable createDefaultSerializable(List<JavaInformations> javaInformationsList,
			Range range, String messageForReport) throws IOException {
		final List<Counter> counters = collector.getRangeCounters(range);
		final List<Serializable> serialized = new ArrayList<Serializable>(
				counters.size() + javaInformationsList.size());
		// on clone les counters avant de les sérialiser pour ne pas avoir de problèmes de concurrences d'accès
		for (final Counter counter : counters) {
			serialized.add(counter.clone());
		}
		serialized.addAll(javaInformationsList);
		if (messageForReport != null) {
			serialized.add(messageForReport);
		}
		return (Serializable) serialized;
	}

	public Range getRangeForSerializable(HttpServletRequest httpRequest) {
		final Range range;
		final String period = HttpParameter.PERIOD.getParameterFrom(httpRequest);
		if (period == null) {
			// période tout par défaut pour Serializable, notamment pour le serveur de collecte
			range = Period.TOUT.getRange();
		} else {
			range = Range.parse(period);
		}
		return range;
	}

	private String getApplication() {
		return collector.getApplication();
	}
}
