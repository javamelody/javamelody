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
package net.bull.javamelody; // NOPMD

import static net.bull.javamelody.HttpParameters.CONNECTIONS_PART;
import static net.bull.javamelody.HttpParameters.CONTENT_DISPOSITION;
import static net.bull.javamelody.HttpParameters.COUNTER_PARAMETER;
import static net.bull.javamelody.HttpParameters.COUNTER_SUMMARY_PER_CLASS_PART;
import static net.bull.javamelody.HttpParameters.CURRENT_REQUESTS_PART;
import static net.bull.javamelody.HttpParameters.DATABASE_PART;
import static net.bull.javamelody.HttpParameters.DEFAULT_WITH_CURRENT_REQUESTS_PART;
import static net.bull.javamelody.HttpParameters.EXPLAIN_PLAN_PART;
import static net.bull.javamelody.HttpParameters.FORMAT_PARAMETER;
import static net.bull.javamelody.HttpParameters.GRAPH_PARAMETER;
import static net.bull.javamelody.HttpParameters.GRAPH_PART;
import static net.bull.javamelody.HttpParameters.HEAP_HISTO_PART;
import static net.bull.javamelody.HttpParameters.HEIGHT_PARAMETER;
import static net.bull.javamelody.HttpParameters.HOTSPOTS_PART;
import static net.bull.javamelody.HttpParameters.JNDI_PART;
import static net.bull.javamelody.HttpParameters.JROBINS_PART;
import static net.bull.javamelody.HttpParameters.MBEANS_PART;
import static net.bull.javamelody.HttpParameters.OTHER_JROBINS_PART;
import static net.bull.javamelody.HttpParameters.PART_PARAMETER;
import static net.bull.javamelody.HttpParameters.PATH_PARAMETER;
import static net.bull.javamelody.HttpParameters.PERIOD_PARAMETER;
import static net.bull.javamelody.HttpParameters.PROCESSES_PART;
import static net.bull.javamelody.HttpParameters.REQUEST_PARAMETER;
import static net.bull.javamelody.HttpParameters.SESSIONS_PART;
import static net.bull.javamelody.HttpParameters.SESSION_ID_PARAMETER;
import static net.bull.javamelody.HttpParameters.THREADS_PART;
import static net.bull.javamelody.HttpParameters.WIDTH_PARAMETER;

import java.io.IOException;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import net.bull.javamelody.SamplingProfiler.SampledMethod;

/**
 * Contrôleur au sens MVC pour la partie des données sérialisées.
 * @author Emeric Vernat
 */
class SerializableController {
	private final Collector collector;

	SerializableController(Collector collector) {
		super();
		assert collector != null;
		this.collector = collector;
	}

	void doSerializable(HttpServletRequest httpRequest, HttpServletResponse httpResponse,
			Serializable serializable) throws IOException {
		// l'appelant (un serveur d'agrégation par exemple) peut appeler
		// la page monitoring avec un format "serialized" ou "xml" en paramètre
		// pour avoir les données au format sérialisé java ou xml
		final String format = httpRequest.getParameter(FORMAT_PARAMETER);
		final TransportFormat transportFormat = TransportFormat.valueOfIgnoreCase(format);
		// checkDependencies avant setContentType pour afficher correctement les erreurs
		transportFormat.checkDependencies();
		httpResponse.setContentType(transportFormat.getMimeType());
		final String fileName = "JavaMelody_" + getApplication().replace(' ', '_').replace("/", "")
				+ '_' + I18N.getCurrentDate().replace('/', '_') + '.' + transportFormat.getCode();
		final String contentDisposition = "inline;filename=" + fileName;
		// encoding des CRLF pour http://en.wikipedia.org/wiki/HTTP_response_splitting
		httpResponse.addHeader(CONTENT_DISPOSITION,
				contentDisposition.replace('\n', '_').replace('\r', '_'));

		transportFormat.writeSerializableTo(serializable, httpResponse.getOutputStream());
	}

	// CHECKSTYLE:OFF
	Serializable createSerializable(HttpServletRequest httpRequest,
			List<JavaInformations> javaInformationsList, String messageForReport) throws Exception { // NOPMD
		// CHECKSTYLE:ON
		final String part = httpRequest.getParameter(PART_PARAMETER);
		if (SESSIONS_PART.equalsIgnoreCase(part)) {
			// par sécurité
			Action.checkSystemActionsEnabled();
			final String sessionId = httpRequest.getParameter(SESSION_ID_PARAMETER);
			if (sessionId == null) {
				return new ArrayList<SessionInformations>(
						SessionListener.getAllSessionsInformations());
			}
			return SessionListener.getSessionInformationsBySessionId(sessionId);
		} else if (HOTSPOTS_PART.equalsIgnoreCase(part)) {
			// par sécurité
			Action.checkSystemActionsEnabled();
			return new ArrayList<SampledMethod>(collector.getHotspots());
		} else if (HEAP_HISTO_PART.equalsIgnoreCase(part)) {
			// par sécurité
			Action.checkSystemActionsEnabled();
			return VirtualMachine.createHeapHistogram();
		} else if (PROCESSES_PART.equalsIgnoreCase(part)) {
			// par sécurité
			Action.checkSystemActionsEnabled();
			return new ArrayList<ProcessInformations>(
					ProcessInformations.buildProcessInformations());
		} else if (JNDI_PART.equalsIgnoreCase(part)) {
			// par sécurité
			Action.checkSystemActionsEnabled();
			final String path = httpRequest.getParameter(PATH_PARAMETER);
			return new ArrayList<JndiBinding>(JndiBinding.listBindings(path));
		} else if (MBEANS_PART.equalsIgnoreCase(part)) {
			// par sécurité
			Action.checkSystemActionsEnabled();
			return new ArrayList<MBeanNode>(MBeans.getAllMBeanNodes());
		} else if (DATABASE_PART.equalsIgnoreCase(part)) {
			// par sécurité
			Action.checkSystemActionsEnabled();
			final int requestIndex = DatabaseInformations.parseRequestIndex(httpRequest
					.getParameter(REQUEST_PARAMETER));
			return new DatabaseInformations(requestIndex);
		} else if (CONNECTIONS_PART.equalsIgnoreCase(part)) {
			// par sécurité
			Action.checkSystemActionsEnabled();
			return new ArrayList<ConnectionInformations>(
					JdbcWrapper.getConnectionInformationsList());
		}
		return createOtherSerializable(httpRequest, javaInformationsList, messageForReport);
	}

	@SuppressWarnings("unchecked")
	private Serializable createOtherSerializable(HttpServletRequest httpRequest,
			List<JavaInformations> javaInformationsList, String messageForReport)
			throws IOException {
		final Range range = getRangeForSerializable(httpRequest);
		final String part = httpRequest.getParameter(PART_PARAMETER);
		if (JROBINS_PART.equalsIgnoreCase(part)) {
			// pour UI Swing
			final int width = Integer.parseInt(httpRequest.getParameter(WIDTH_PARAMETER));
			final int height = Integer.parseInt(httpRequest.getParameter(HEIGHT_PARAMETER));
			final String graphName = httpRequest.getParameter(GRAPH_PARAMETER);
			return getJRobinsImages(range, width, height, graphName);
		} else if (OTHER_JROBINS_PART.equalsIgnoreCase(part)) {
			// pour UI Swing
			final int width = Integer.parseInt(httpRequest.getParameter(WIDTH_PARAMETER));
			final int height = Integer.parseInt(httpRequest.getParameter(HEIGHT_PARAMETER));
			final Collection<JRobin> jrobins = collector.getOtherJRobins();
			return (Serializable) convertJRobinsToImages(jrobins, range, width, height);
		} else if (THREADS_PART.equalsIgnoreCase(part)) {
			return new ArrayList<ThreadInformations>(javaInformationsList.get(0)
					.getThreadInformationsList());
		} else if (COUNTER_SUMMARY_PER_CLASS_PART.equalsIgnoreCase(part)) {
			final String counterName = httpRequest.getParameter(COUNTER_PARAMETER);
			final String requestId = httpRequest.getParameter(GRAPH_PARAMETER);
			final Counter counter = collector.getRangeCounter(range, counterName).clone();
			final List<CounterRequest> requestList = new CounterRequestAggregation(counter)
					.getRequestsAggregatedOrFilteredByClassName(requestId);
			return new ArrayList<CounterRequest>(requestList);
		} else if (GRAPH_PART.equalsIgnoreCase(part)) {
			final String requestId = httpRequest.getParameter(GRAPH_PARAMETER);
			return getCounterRequestById(requestId, range);
		} else if (CURRENT_REQUESTS_PART.equalsIgnoreCase(part)) {
			final Map<JavaInformations, List<CounterRequestContext>> result = new HashMap<JavaInformations, List<CounterRequestContext>>();
			result.put(javaInformationsList.get(0), getCurrentRequests());
			return (Serializable) result;
		} else if (DEFAULT_WITH_CURRENT_REQUESTS_PART.equalsIgnoreCase(part)) {
			final List<Serializable> result = new ArrayList<Serializable>();
			result.addAll((List<Serializable>) createDefaultSerializable(javaInformationsList,
					range, messageForReport));
			result.addAll(getCurrentRequests());
			return (Serializable) result;
		} else if (EXPLAIN_PLAN_PART.equalsIgnoreCase(part)) {
			// pour UI Swing,
			final String sqlRequest = httpRequest.getHeader(REQUEST_PARAMETER);
			return explainPlanFor(sqlRequest);
		}

		return createDefaultSerializable(javaInformationsList, range, messageForReport);
	}

	private Serializable getCounterRequestById(String requestId, Range range) throws IOException {
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

	private Serializable getJRobinsImages(Range range, int width, int height, String graphName)
			throws IOException {
		if (graphName != null) {
			final JRobin jrobin = collector.getJRobin(graphName);
			if (jrobin != null) {
				return jrobin.graph(range, width, height);
			}
			return null;
		}
		final Collection<JRobin> jrobins = collector.getCounterJRobins();
		return (Serializable) convertJRobinsToImages(jrobins, range, width, height);
	}

	private Serializable explainPlanFor(String sqlRequest) {
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
			if (collector.isJRobinDisplayed(jrobin)) {
				final byte[] image = jrobin.graph(range, width, height);
				images.put(jrobin.getName(), image);
			}
		}
		return images;
	}

	Serializable createDefaultSerializable(List<JavaInformations> javaInformationsList,
			Range range, String messageForReport) throws IOException {
		final List<Counter> counters = collector.getRangeCounters(range);
		final List<Serializable> serialized = new ArrayList<Serializable>(counters.size()
				+ javaInformationsList.size());
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

	Range getRangeForSerializable(HttpServletRequest httpRequest) {
		final Range range;
		if (httpRequest.getParameter(PERIOD_PARAMETER) == null) {
			// période tout par défaut pour Serializable, notamment pour le serveur de collecte
			range = Period.TOUT.getRange();
		} else {
			range = Range.parse(httpRequest.getParameter(PERIOD_PARAMETER));
		}
		return range;
	}

	private String getApplication() {
		return collector.getApplication();
	}
}
