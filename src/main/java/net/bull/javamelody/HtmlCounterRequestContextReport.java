/*
 * Copyright 2008-2009 by Emeric Vernat, Bull
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

import java.io.IOException;
import java.io.Writer;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Partie du rapport html pour les contextes de requêtes en cours.
 * @author Emeric Vernat
 */
class HtmlCounterRequestContextReport {
	private final List<CounterRequestContext> rootCurrentContexts;
	private final Map<String, HtmlCounterReport> counterReportsByCounterName;
	private final Map<Long, ThreadInformations> threadInformationsByThreadId;
	private final Period period;
	private final Writer writer;
	private final boolean childHitsDisplayed;
	private final DecimalFormat integerFormat = I18N.createIntegerFormat();
	private final long timeOfSnapshot = System.currentTimeMillis();
	private final boolean stackTraceEnabled;
	private final HtmlThreadInformationsReport htmlThreadInformationsReport;

	// classe utilitaire utilisée pour html et pdf
	static class CounterRequestContextReportHelper {
		private final List<CounterRequestContext> contexts;
		private final boolean childHitsDisplayed;
		private final Map<String, CounterRequest> counterRequestsByRequestName = new HashMap<String, CounterRequest>();

		CounterRequestContextReportHelper(List<CounterRequestContext> contexts,
				boolean childHitsDisplayed) {
			super();
			assert contexts != null;
			this.contexts = contexts;
			this.childHitsDisplayed = childHitsDisplayed;
		}

		List<int[]> getRequestValues() {
			final List<int[]> result = new ArrayList<int[]>();
			final int contextsSize = contexts.size();
			final int[] durationMeans = new int[contextsSize];
			final int[] cpuTimes = new int[contextsSize];
			final int[] cpuTimesMeans = new int[contextsSize];
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
				i++;
			}
			result.add(durationMeans);
			result.add(cpuTimes);
			result.add(cpuTimesMeans);
			if (childHitsDisplayed) {
				final int[] totalChildHits = new int[contextsSize];
				final int[] childHitsMeans = new int[contextsSize];
				final int[] totalChildDurationsSum = new int[contextsSize];
				final int[] childDurationsMeans = new int[contextsSize];
				i = 0;
				for (final CounterRequestContext context : contexts) {
					totalChildHits[i] = getValueOrIgnoreIfNoChildHitForContext(context, context
							.getTotalChildHits());
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
			Period period, Writer writer) {
		super();
		assert rootCurrentContexts != null;
		assert counterReportsByCounterName != null;
		assert threadInformationsList != null;
		assert period != null;
		assert writer != null;

		this.rootCurrentContexts = rootCurrentContexts;
		this.counterReportsByCounterName = counterReportsByCounterName;
		this.threadInformationsByThreadId = new HashMap<Long, ThreadInformations>(
				threadInformationsList.size());
		for (final ThreadInformations threadInformations : threadInformationsList) {
			this.threadInformationsByThreadId.put(threadInformations.getId(), threadInformations);
		}
		this.period = period;
		this.writer = writer;
		boolean oneRootHasChild = false;
		for (final CounterRequestContext rootCurrentContext : rootCurrentContexts) {
			if (rootCurrentContext.getParentCounter().getChildCounterName() != null) {
				oneRootHasChild = true;
				break;
			}
		}
		this.childHitsDisplayed = oneRootHasChild;
		this.htmlThreadInformationsReport = new HtmlThreadInformationsReport(
				threadInformationsList, stackTraceEnabled, writer);
		this.stackTraceEnabled = stackTraceEnabled;
	}

	void toHtml() throws IOException {
		if (rootCurrentContexts.isEmpty()) {
			return;
		}
		writeContexts(Collections.singletonList(rootCurrentContexts.get(0)));
		writeln("<div align='right'>");
		writeln(I18N.getFormattedString("nb_requete_en_cours", integerFormat
				.format(rootCurrentContexts.size())));
		writeln("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
		final String counterName = rootCurrentContexts.get(0).getParentCounter().getName();
		// PID dans l'id du div pour concaténation de pages et affichage dans serveur de collecte
		writeShowHideLink("contextDetails" + counterName + PID.getPID(), "#Details#");
		writeln("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
		writeln("</div>");

		writeln("<div id='contextDetails" + counterName + PID.getPID()
				+ "' style='display: none;'>");
		writeContexts(rootCurrentContexts);
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
		writeln("<table class='sortable' width='100%' border='1' cellspacing='0' cellpadding='2' summary='#Requetes_en_cours#'>");
		write("<thead><tr><th>#Thread#</th>");
		if (displayRemoteUser) {
			write("<th>#Utilisateur#</th>");
		}
		write("<th>#Requete#</th>");
		write("<th class='sorttable_numeric'>#Duree_ecoulee#</th><th class='sorttable_numeric'>#Temps_moyen#</th>");
		write("<th class='sorttable_numeric'>#Temps_cpu#</th><th class='sorttable_numeric'>#Temps_cpu_moyen#</th>");
		// rq : tous ces contextes viennent du même compteur donc peu importe lequel des parentCounter
		if (childHitsDisplayed) {
			final String childCounterName = contexts.get(0).getParentCounter()
					.getChildCounterName();
			write("<th class='sorttable_numeric'>"
					+ I18N.getFormattedString("hits_fils", childCounterName));
			write("</th><th class='sorttable_numeric'>"
					+ I18N.getFormattedString("hits_fils_moyens", childCounterName));
			write("</th><th class='sorttable_numeric'>"
					+ I18N.getFormattedString("temps_fils", childCounterName));
			write("</th><th class='sorttable_numeric'>"
					+ I18N.getFormattedString("temps_fils_moyen", childCounterName) + "</th>");
		}
		if (stackTraceEnabled) {
			write("<th>#Methode_executee#</th>");
		}
		writeln("</tr></thead><tbody>");
		boolean odd = false;
		for (final CounterRequestContext context : contexts) {
			if (odd) {
				write("<tr class='odd' onmouseover=\"this.className='highlight'\" onmouseout=\"this.className='odd'\">");
			} else {
				write("<tr onmouseover=\"this.className='highlight'\" onmouseout=\"this.className=''\">");
			}
			odd = !odd; // NOPMD
			writeContext(context, displayRemoteUser);
			writeln("</tr>");
		}
		writeln("</tbody></table>");
	}

	private void writeContext(CounterRequestContext rootContext, boolean displayRemoteUser)
			throws IOException {
		// attention, cela ne marcherait pas sur le serveur de collecte, à partir du seul threadId
		// s'il y a plusieurs instances en cluster
		final ThreadInformations threadInformations = threadInformationsByThreadId.get(rootContext
				.getThreadId());
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
		write("</td>");
	}

	private void writeRequests(List<CounterRequestContext> contexts,
			CounterRequestContextReportHelper counterRequestContextReportHelper) throws IOException {
		int margin = 0;
		for (final CounterRequestContext context : contexts) {
			write("<div style='margin-left: ");
			write(Integer.toString(margin));
			writeln("px;'>");
			writeRequest(context, counterRequestContextReportHelper);
			write("</div>");
			margin += 10;
		}
	}

	private void writeRequest(CounterRequestContext context,
			CounterRequestContextReportHelper counterRequestContextReportHelper) throws IOException {
		final Counter parentCounter = context.getParentCounter();
		if (parentCounter.getIconName() != null) {
			write("<img src='?resource=");
			write(parentCounter.getIconName());
			write("' alt='");
			write(parentCounter.getName());
			write("' width='16' height='16' />&nbsp;");
		}
		final HtmlCounterReport counterReport = getCounterReport(parentCounter);
		final CounterRequest counterRequest = counterRequestContextReportHelper
				.getCounterRequest(context);
		counterReport.writeRequestGraph(counterRequest.getId(), context.getCompleteRequestName());
	}

	private HtmlCounterReport getCounterReport(Counter parentCounter) {
		HtmlCounterReport counterReport = counterReportsByCounterName.get(parentCounter.getName());
		if (counterReport == null) {
			counterReport = new HtmlCounterReport(parentCounter, period, writer);
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

			final HtmlCounterReport counterReport = counterReportsByCounterName.get(parentCounter
					.getName());
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

	private void writeShowHideLink(String idToShow, String label) throws IOException {
		writeln("<a href=\"javascript:showHide('" + idToShow + "');\" class='noPrint'><img id='"
				+ idToShow + "Img' src='?resource=bullets/plus.png' alt=''/> " + label + "</a>");
	}

	private void write(String html) throws IOException {
		I18N.writeTo(html, writer);
	}

	private void writeln(String html) throws IOException {
		I18N.writelnTo(html, writer);
	}
}
