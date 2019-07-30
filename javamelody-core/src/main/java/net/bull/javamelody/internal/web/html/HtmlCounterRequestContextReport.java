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
package net.bull.javamelody.internal.web.html;

import java.io.IOException;
import java.io.Writer;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import net.bull.javamelody.internal.common.I18N;
import net.bull.javamelody.internal.common.Parameters;
import net.bull.javamelody.internal.model.Counter;
import net.bull.javamelody.internal.model.CounterRequest;
import net.bull.javamelody.internal.model.CounterRequestContext;
import net.bull.javamelody.internal.model.PID;
import net.bull.javamelody.internal.model.Period;
import net.bull.javamelody.internal.model.ThreadInformations;

/**
 * Partie du rapport html pour les contextes de requêtes en cours.
 * @author Emeric Vernat
 */
public class HtmlCounterRequestContextReport extends HtmlAbstractReport {
	private final List<CounterRequestContext> rootCurrentContexts;
	private final Map<String, HtmlCounterReport> counterReportsByCounterName;
	private final Map<Long, ThreadInformations> threadInformationsByThreadId;
	private final boolean childHitsDisplayed;
	private final DecimalFormat integerFormat = I18N.createIntegerFormat();
	private final long timeOfSnapshot = System.currentTimeMillis();
	private final boolean stackTraceEnabled;
	private final int maxContextsDisplayed;
	private final boolean systemActionsEnabled = Parameters.isSystemActionsEnabled();
	private final HtmlThreadInformationsReport htmlThreadInformationsReport;

	/**
	 * Helper class used for html and pdf.
	 * @author Emeric Vernat
	 */
	public static class CounterRequestContextReportHelper {
		private final List<CounterRequestContext> contexts;
		private final boolean childHitsDisplayed;
		private final Map<String, CounterRequest> counterRequestsByRequestName = new HashMap<String, CounterRequest>();

		public CounterRequestContextReportHelper(List<CounterRequestContext> contexts,
				boolean childHitsDisplayed) {
			super();
			assert contexts != null;
			this.contexts = contexts;
			this.childHitsDisplayed = childHitsDisplayed;
		}

		public List<int[]> getRequestValues() {
			final List<int[]> result = new ArrayList<int[]>();
			final int contextsSize = contexts.size();
			final int[] durationMeans = new int[contextsSize];
			final int[] cpuTimes = new int[contextsSize];
			final int[] cpuTimesMeans = new int[contextsSize];
			//			final int[] allocatedKBytes = new int[contextsSize];
			//			final int[] allocatedKBytesMean = new int[contextsSize];
			int i = 0;
			for (final CounterRequestContext context : contexts) {
				final CounterRequest counterRequest = getCounterRequest(context);
				durationMeans[i] = counterRequest.getMean();
				cpuTimesMeans[i] = counterRequest.getCpuTimeMean();
				if (cpuTimesMeans[i] >= 0) {
					cpuTimes[i] = context.getCpuTime();
				} else {
					cpuTimes[i] = -1;
				}
				//				allocatedKBytesMean[i] = counterRequest.getAllocatedKBytesMean();
				//				if (allocatedKBytesMean[i] >= 0) {
				//					allocatedKBytes[i] = context.getAllocatedKBytes();
				//				} else {
				//					allocatedKBytes[i] = -1;
				//				}
				i++;
			}
			result.add(durationMeans);
			result.add(cpuTimes);
			result.add(cpuTimesMeans);
			//			result.add(allocatedKBytes);
			//			result.add(allocatedKBytesMean);
			if (childHitsDisplayed) {
				final int[] totalChildHits = new int[contextsSize];
				final int[] childHitsMeans = new int[contextsSize];
				final int[] totalChildDurationsSum = new int[contextsSize];
				final int[] childDurationsMeans = new int[contextsSize];
				i = 0;
				for (final CounterRequestContext context : contexts) {
					totalChildHits[i] = getValueOrIgnoreIfNoChildHitForContext(context,
							context.getTotalChildHits());
					final CounterRequest counterRequest = getCounterRequest(context);
					childHitsMeans[i] = getValueOrIgnoreIfNoChildHitForContext(context,
							counterRequest.getChildHitsMean());
					totalChildDurationsSum[i] = getValueOrIgnoreIfNoChildHitForContext(context,
							context.getTotalChildDurationsSum());
					childDurationsMeans[i] = getValueOrIgnoreIfNoChildHitForContext(context,
							counterRequest.getChildDurationsMean());
					i++;
				}
				result.add(totalChildHits);
				result.add(childHitsMeans);
				result.add(totalChildDurationsSum);
				result.add(childDurationsMeans);
			}
			return result;
		}

		private static int getValueOrIgnoreIfNoChildHitForContext(CounterRequestContext context,
				int value) {
			if (context.getParentCounter().getChildCounterName() == null) {
				// si le compteur parent du contexte n'a pas de compteur fils
				// (comme le compteur sql et au contraire du compteur http),
				// alors la valeur de childHits ou childDurations n'a pas de sens
				// (comme les hits sql fils pour une requête sql)
				return -1;
			}
			return value;
		}

		CounterRequest getCounterRequest(CounterRequestContext context) {
			final String requestName = context.getRequestName();
			CounterRequest counterRequest = counterRequestsByRequestName.get(requestName);
			if (counterRequest == null) {
				counterRequest = context.getParentCounter().getCounterRequest(context);
				counterRequestsByRequestName.put(requestName, counterRequest);
			}
			return counterRequest;
		}
	}

	HtmlCounterRequestContextReport(List<CounterRequestContext> rootCurrentContexts,
			Map<String, HtmlCounterReport> counterReportsByCounterName,
			List<ThreadInformations> threadInformationsList, boolean stackTraceEnabled,
			int maxContextsDisplayed, Writer writer) {
		super(writer);
		assert rootCurrentContexts != null;
		assert threadInformationsList != null;

		this.rootCurrentContexts = rootCurrentContexts;
		if (counterReportsByCounterName == null) {
			this.counterReportsByCounterName = new HashMap<String, HtmlCounterReport>();
		} else {
			this.counterReportsByCounterName = counterReportsByCounterName;
		}
		this.threadInformationsByThreadId = new HashMap<Long, ThreadInformations>(
				threadInformationsList.size());
		for (final ThreadInformations threadInformations : threadInformationsList) {
			this.threadInformationsByThreadId.put(threadInformations.getId(), threadInformations);
		}
		boolean oneRootHasChild = false;
		for (final CounterRequestContext rootCurrentContext : rootCurrentContexts) {
			if (rootCurrentContext.hasChildHits()) {
				oneRootHasChild = true;
				break;
			}
		}
		this.childHitsDisplayed = oneRootHasChild;
		this.htmlThreadInformationsReport = new HtmlThreadInformationsReport(threadInformationsList,
				stackTraceEnabled, writer);
		this.stackTraceEnabled = stackTraceEnabled;
		this.maxContextsDisplayed = maxContextsDisplayed;
	}

	@Override
	void toHtml() throws IOException {
		if (rootCurrentContexts.isEmpty()) {
			writeln("#Aucune_requete_en_cours#");
			return;
		}
		writeContexts(Collections.singletonList(rootCurrentContexts.get(0)));
		writeln("<div align='right' class='noPrint'>");
		writeln(getFormattedString("nb_requete_en_cours",
				integerFormat.format(rootCurrentContexts.size())));
		final String separator = "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;";
		if (isPdfEnabled()) {
			writeln(separator);
			write("<a href='?part=currentRequests&amp;format=pdf' title='#afficher_PDF#'>");
			write("<img src='?resource=pdf.png' alt='#PDF#'/> #PDF#</a>");
		}
		writeln(separator);
		if (rootCurrentContexts.size() <= maxContextsDisplayed) {
			writeln("<a href='?part=currentRequests' title='#Requetes_en_cours#'>");
			writeln("<img src='?resource=hourglass.png' alt='#Requetes_en_cours#' width='16' height='16'/>");
			writeln("#Voir_dans_une_nouvelle_page#</a>");
			writeln(separator);

			final String counterName = rootCurrentContexts.get(0).getParentCounter().getName();
			// PID dans l'id du div pour concaténation de pages et affichage dans serveur de collecte
			writeShowHideLink("contextDetails" + counterName + PID.getPID(), "#Details#");
			writeln(separator);
			writeln("</div> ");
			writeln("<div id='contextDetails" + counterName + PID.getPID()
					+ "' style='display: none;'>");
			writeContexts(rootCurrentContexts);
			writeln("</div>");
		} else {
			// le nombre de requêtes en cours dépasse le maximum pour être affiché dans le rapport
			// principal, donc on affiche seulement un lien vers la page à part
			writeln("<a href='?part=currentRequests' title='#Requetes_en_cours#'>#Details#</a>");
			writeln(separator);
			writeln("</div> ");
		}
	}

	void writeTitleAndDetails() throws IOException {
		writeTitle("hourglass.png", getString("Requetes_en_cours"));
		write("<br/>");

		if (rootCurrentContexts.isEmpty()) {
			writeln("#Aucune_requete_en_cours#");
			return;
		}
		writeContexts(rootCurrentContexts);

		writeln("<div align='right'>");
		writeln(getFormattedString("nb_requete_en_cours",
				integerFormat.format(rootCurrentContexts.size())));
		writeln("</div>");
	}

	private void writeContexts(List<CounterRequestContext> contexts) throws IOException {
		boolean displayRemoteUser = false;
		for (final CounterRequestContext context : contexts) {
			if (context.getRemoteUser() != null) {
				displayRemoteUser = true;
				break;
			}
		}
		final HtmlTable table = new HtmlTable();
		table.beginTable(getString("Requetes_en_cours"));
		write("<th>#Thread#</th>");
		if (displayRemoteUser) {
			write("<th>#Utilisateur#</th>");
		}
		write("<th>#Requete#</th>");
		write("<th class='sorttable_numeric'>#Duree_ecoulee#</th><th class='sorttable_numeric'>#Temps_moyen#</th>");
		write("<th class='sorttable_numeric'>#Temps_cpu#</th><th class='sorttable_numeric'>#Temps_cpu_moyen#</th>");
		// pas la peine de surcharger avec les Ko alloués
		//		write("<th class='sorttable_numeric'>#Ko_alloues#</th><th class='sorttable_numeric'>#Ko_alloues_moyens#</th>");
		// rq : tous ces contextes viennent du même compteur donc peu importe lequel des parentCounter
		if (childHitsDisplayed) {
			final String childCounterName = contexts.get(0).getParentCounter()
					.getChildCounterName();
			write("<th class='sorttable_numeric'>"
					+ getFormattedString("hits_fils", childCounterName));
			write("</th><th class='sorttable_numeric'>"
					+ getFormattedString("hits_fils_moyens", childCounterName));
			write("</th><th class='sorttable_numeric'>"
					+ getFormattedString("temps_fils", childCounterName));
			write("</th><th class='sorttable_numeric'>"
					+ getFormattedString("temps_fils_moyen", childCounterName) + "</th>");
		}
		if (stackTraceEnabled) {
			write("<th>#Methode_executee#</th>");
		}
		if (systemActionsEnabled) {
			writeln("<th class='noPrint'>#Tuer#</th>");
		}
		for (final CounterRequestContext context : contexts) {
			table.nextRow();
			writeContext(context, displayRemoteUser);
		}
		table.endTable();
	}

	private void writeContext(CounterRequestContext rootContext, boolean displayRemoteUser)
			throws IOException {
		// attention, cela ne marcherait pas sur le serveur de collecte, à partir du seul threadId
		// s'il y a plusieurs instances en cluster
		final ThreadInformations threadInformations = threadInformationsByThreadId
				.get(rootContext.getThreadId());
		write("<td valign='top'>");
		final String espace = "&nbsp;";
		if (threadInformations == null) {
			write(espace); // un décalage n'a pas permis de récupérer le thread de ce context
		} else {
			htmlThreadInformationsReport.writeThreadWithStackTrace(threadInformations);
		}
		if (displayRemoteUser) {
			write("</td> <td valign='top'>");
			if (rootContext.getRemoteUser() == null) {
				write(espace);
			} else {
				write(rootContext.getRemoteUser());
			}
		}
		final List<CounterRequestContext> contexts = new ArrayList<CounterRequestContext>(3);
		contexts.add(rootContext);
		contexts.addAll(rootContext.getChildContexts());
		final CounterRequestContextReportHelper counterRequestContextReportHelper = new CounterRequestContextReportHelper(
				contexts, childHitsDisplayed);
		write("</td> <td>");
		writeRequests(contexts, counterRequestContextReportHelper);

		write("</td> <td align='right' valign='top'>");
		writeDurations(contexts);

		for (final int[] requestValues : counterRequestContextReportHelper.getRequestValues()) {
			writeRequestValues(requestValues);
		}

		if (stackTraceEnabled) {
			write("</td> <td valign='top'>");
			if (threadInformations == null) {
				write(espace); // un décalage n'a pas permis de récupérer le thread de ce context
			} else {
				htmlThreadInformationsReport.writeExecutedMethod(threadInformations);
			}
		}
		if (threadInformations == null) {
			write("</td> <td class='noPrint'>");
			write(espace); // un décalage n'a pas permis de récupérer le thread de ce context
		} else {
			htmlThreadInformationsReport.writeKillThread(threadInformations);
		}
		write("</td>");
	}

	private void writeRequests(List<CounterRequestContext> contexts,
			CounterRequestContextReportHelper counterRequestContextReportHelper)
			throws IOException {
		int margin = 0;
		for (final CounterRequestContext context : contexts) {
			write("<div style='margin-left: ");
			write(Integer.toString(margin));
			writeln("px;' class='wrappedText'>");
			writeRequest(context, counterRequestContextReportHelper);
			write("</div>");
			margin += 10;
		}
	}

	private void writeRequest(CounterRequestContext context,
			CounterRequestContextReportHelper counterRequestContextReportHelper)
			throws IOException {
		final Counter parentCounter = context.getParentCounter();
		if (parentCounter.getIconName() != null) {
			write("<img src='?resource=");
			write(parentCounter.getIconName());
			write("' alt='");
			write(parentCounter.getName());
			write("' width='16' height='16' />&nbsp;");
		}
		// la période n'a pas d'importance pour writeRequestGraph
		final HtmlCounterReport counterReport = getCounterReport(parentCounter, Period.TOUT);
		final CounterRequest counterRequest = counterRequestContextReportHelper
				.getCounterRequest(context);
		counterReport.writeRequestName(counterRequest.getId(), context.getCompleteRequestName(),
				HtmlCounterReport.isRequestGraphDisplayed(parentCounter), true, false);
	}

	private HtmlCounterReport getCounterReport(Counter parentCounter, Period period) {
		HtmlCounterReport counterReport = counterReportsByCounterName.get(parentCounter.getName());
		if (counterReport == null) {
			counterReport = new HtmlCounterReport(parentCounter, period.getRange(), getWriter());
			counterReportsByCounterName.put(parentCounter.getName(), counterReport);
		}
		return counterReport;
	}

	private void writeDurations(List<CounterRequestContext> contexts) throws IOException {
		boolean first = true;
		for (final CounterRequestContext context : contexts) {
			if (!first) {
				writeln("<br/>");
			}
			final int duration = context.getDuration(timeOfSnapshot);

			final Counter parentCounter = context.getParentCounter();
			if (parentCounter.getIconName() != null) {
				// on remet l'icône ici avec la durée en plus de l'icône sur la requête http, façade
				// ou sql car la requête peut avoir des sauts de ligne (par ex. par manque de place)
				// et dans ce cas les durées ne sont plus du tout alignées
				write("<img src='?resource=");
				write(parentCounter.getIconName());
				write("' alt='");
				write(parentCounter.getName());
				write("' width='16' height='16' />&nbsp;");
			}

			final HtmlCounterReport counterReport = counterReportsByCounterName
					.get(parentCounter.getName());
			write("<span class='");
			write(counterReport.getSlaHtmlClass(duration));
			write("'>");
			write(integerFormat.format(duration));
			write("</span>");
			first = false;
		}
	}

	private void writeRequestValues(int[] requestValues) throws IOException {
		write("</td> <td align='right' valign='top'>");
		boolean first = true;
		for (final int value : requestValues) {
			if (!first) {
				writeln("<br/>");
			}
			if (value == -1) {
				write("&nbsp;");
			} else {
				write(integerFormat.format(value));
			}
			first = false;
		}
	}
}
