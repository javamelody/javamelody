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
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.regex.Pattern;

import net.bull.javamelody.internal.common.I18N;
import net.bull.javamelody.internal.model.Counter;
import net.bull.javamelody.internal.model.CounterRequest;
import net.bull.javamelody.internal.model.CounterRequestAggregation;
import net.bull.javamelody.internal.model.Period;
import net.bull.javamelody.internal.model.Range;

/**
 * Partie du rapport html pour un compteur.
 * @author Emeric Vernat
 */
public class HtmlCounterReport extends HtmlAbstractReport {
	private static final Pattern SQL_KEYWORDS_PATTERN = Pattern.compile(
			"\\b(select|from|where|order by|group by|update|delete|insert into|values)\\b",
			Pattern.CASE_INSENSITIVE);
	private final Counter counter;
	private final Range range;
	private final CounterRequestAggregation counterRequestAggregation;
	private final HtmlCounterRequestGraphReport htmlCounterRequestGraphReport;
	private final DecimalFormat systemErrorFormat = I18N.createPercentFormat();
	private final DecimalFormat integerFormat = I18N.createIntegerFormat();

	HtmlCounterReport(Counter counter, Range range, Writer writer) {
		super(writer);
		assert counter != null;
		assert range != null;
		this.counter = counter;
		this.range = range;
		this.counterRequestAggregation = new CounterRequestAggregation(counter);
		this.htmlCounterRequestGraphReport = new HtmlCounterRequestGraphReport(range, writer);
	}

	@Override
	void toHtml() throws IOException {
		final List<CounterRequest> requests = counterRequestAggregation.getRequests();
		if (requests.isEmpty()) {
			writeNoRequests();
			return;
		}
		final String counterName = counter.getName();
		final CounterRequest globalRequest = counterRequestAggregation.getGlobalRequest();
		// 1. synthèse
		if (isErrorAndNotJobCounter()) {
			// il y a au moins une "request" d'erreur puisque la liste n'est pas vide
			assert !requests.isEmpty();
			final List<CounterRequest> summaryRequest = Collections.singletonList(requests.get(0));
			writeRequests(counterName, counter.getChildCounterName(), summaryRequest, false, true,
					false);
		} else {
			final List<CounterRequest> summaryRequests = Arrays.asList(globalRequest,
					counterRequestAggregation.getWarningRequest(),
					counterRequestAggregation.getSevereRequest());
			writeRequests(globalRequest.getName(), counter.getChildCounterName(), summaryRequests,
					false, false, false);
		}

		// 2. débit et liens
		writeSizeAndLinks(requests, globalRequest);

		// 3. détails par requêtes (non visible par défaut)
		writeln("<div id='details" + counterName + "' style='display: none;'>");
		writeRequests(counterName, counter.getChildCounterName(), requests,
				isRequestGraphDisplayed(counter), true, false);
		writeln("</div>");

		// 4. logs (non visible par défaut)
		if (isErrorCounter()) {
			writeln("<div id='logs" + counterName + "' style='display: none;'><div>");
			new HtmlCounterErrorReport(counter, getWriter()).toHtml();
			writeln("</div></div>");
		}
	}

	private void writeSizeAndLinks(List<CounterRequest> requests, CounterRequest globalRequest)
			throws IOException {
		final long end;
		if (range.getEndDate() != null) {
			// l'utilisateur a choisi une période personnalisée de date à date,
			// donc la fin est peut-être avant la date du jour
			end = Math.min(range.getEndDate().getTime(), System.currentTimeMillis());
		} else {
			end = System.currentTimeMillis();
		}
		// delta ni négatif ni à 0
		final long deltaMillis = Math.max(end - counter.getStartDate().getTime(), 1);
		final long hitsParMinute = 60 * 1000 * globalRequest.getHits() / deltaMillis;
		writeln("<div align='right'>");
		// Rq : si serveur utilisé de 8h à 20h (soit 12h) on peut multiplier par 2 ces hits par minute indiqués
		// pour avoir une moyenne sur les heures d'activité sans la nuit
		final String nbKey;
		if (isJobCounter()) {
			nbKey = "nb_jobs";
		} else if (isErrorCounter()) {
			nbKey = "nb_erreurs";
		} else {
			nbKey = "nb_requetes";
		}
		writeln(getFormattedString(nbKey, integerFormat.format(hitsParMinute),
				integerFormat.format(requests.size())));
		final String separator = "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;";
		if (counter.isBusinessFacadeCounter()) {
			writeln(separator);
			writeln("<a href='?part=counterSummaryPerClass&amp;counter=" + counter.getName()
					+ "' class='noPrint'>#Resume_par_classe#</a>");
			if (isPdfEnabled()) {
				writeln(separator);
				writeln("<a href='?part=runtimeDependencies&amp;format=pdf&amp;counter="
						+ counter.getName() + "' class='noPrint'>#Dependencies#</a>");
			}
		}
		writeln(separator);
		writeShowHideLink("details" + counter.getName(), "#Details#");
		if (isErrorCounter()) {
			writeln(separator);
			writeShowHideLink("logs" + counter.getName(), "#Dernieres_erreurs#");
		}
		writeln(separator);
		if (range.getPeriod() == Period.TOUT) {
			writeln("<a href='?action=clear_counter&amp;counter=" + counter.getName()
					+ getCsrfTokenUrlPart() + "' title='"
					+ getFormattedString("Vider_stats", counter.getName()) + '\'');
			writeln("class='noPrint' onclick=\"javascript:return confirm('"
					+ javascriptEncode(getFormattedString("confirm_vider_stats", counter.getName()))
					+ "');\">#Reinitialiser#</a>");
		}
		writeln("</div>");
	}

	private void writeNoRequests() throws IOException {
		if (isJobCounter()) {
			writeln("#Aucun_job#");
		} else if (isErrorCounter()) {
			writeln("#Aucune_erreur#");
		} else {
			writeln("#Aucune_requete#");
		}
	}

	private boolean isErrorCounter() {
		return counter.isErrorCounter();
	}

	private boolean isJobCounter() {
		return counter.isJobCounter();
	}

	private boolean isErrorAndNotJobCounter() {
		return isErrorCounter() && !isJobCounter();
	}

	public static boolean isRequestGraphDisplayed(Counter parentCounter) {
		return !(parentCounter.isErrorCounter() && !parentCounter.isJobCounter())
				&& !parentCounter.isJspOrStrutsCounter();
	}

	void writeRequestsAggregatedOrFilteredByClassName(String requestId) throws IOException {
		final List<CounterRequest> requestList = counterRequestAggregation
				.getRequestsAggregatedOrFilteredByClassName(requestId);
		final boolean includeSummaryPerClassLink = requestId == null;
		final boolean includeDetailLink = !includeSummaryPerClassLink;
		writeRequests(counter.getName(), counter.getChildCounterName(), requestList,
				includeDetailLink, includeDetailLink, includeSummaryPerClassLink);
	}

	private void writeRequests(String tableName, String childCounterName,
			List<CounterRequest> requestList, boolean includeGraph, boolean includeDetailLink,
			boolean includeSummaryPerClassLink) throws IOException {
		assert requestList != null;
		final HtmlTable table = new HtmlTable();
		table.beginTable(tableName);
		writeTableHead(childCounterName);
		for (final CounterRequest request : requestList) {
			table.nextRow();
			writeRequest(request, includeGraph, includeDetailLink, includeSummaryPerClassLink);
		}
		table.endTable();
	}

	private void writeTableHead(String childCounterName) throws IOException {
		if (isJobCounter()) {
			write("<th>#Job#</th>");
		} else if (isErrorCounter()) {
			write("<th>#Erreur#</th>");
		} else {
			write("<th>#Requete#</th>");
		}
		if (counterRequestAggregation.isTimesDisplayed()) {
			write("<th class='sorttable_numeric'>#temps_cumule#</th>");
			write("<th class='sorttable_numeric'>#Hits#</th>");
			write("<th class='sorttable_numeric'>#Temps_moyen#</th>");
			write("<th class='sorttable_numeric'>#Temps_max#</th>");
			write("<th class='sorttable_numeric'>#Ecart_type#</th>");
		} else {
			write("<th class='sorttable_numeric'>#Hits#</th>");
		}
		if (counterRequestAggregation.isCpuTimesDisplayed()) {
			write("<th class='sorttable_numeric'>#temps_cpu_cumule#</th>");
			write("<th class='sorttable_numeric'>#Temps_cpu_moyen#</th>");
		}
		if (counterRequestAggregation.isAllocatedKBytesDisplayed()) {
			write("<th class='sorttable_numeric'>#Ko_alloues_moyens#</th>");
		}
		if (!isErrorAndNotJobCounter()) {
			write("<th class='sorttable_numeric'>#erreur_systeme#</th>");
		}
		if (counterRequestAggregation.isResponseSizeDisplayed()) {
			write("<th class='sorttable_numeric'>#Taille_moyenne#</th>");
		}
		if (counterRequestAggregation.isChildHitsDisplayed()) {
			write("<th class='sorttable_numeric'>"
					+ getFormattedString("hits_fils_moyens", childCounterName));
			write("</th><th class='sorttable_numeric'>"
					+ getFormattedString("temps_fils_moyen", childCounterName) + "</th>");
		}
	}

	private void writeRequest(CounterRequest request, boolean includeGraph,
			boolean includeDetailLink, boolean includeSummaryPerClassLink) throws IOException {
		final String nextColumn = "</td> <td align='right'>";
		write("<td class='wrappedText'>");
		writeRequestName(request.getId(), request.getName(), includeGraph, includeDetailLink,
				includeSummaryPerClassLink);
		final CounterRequest globalRequest = counterRequestAggregation.getGlobalRequest();
		if (counterRequestAggregation.isTimesDisplayed()) {
			write(nextColumn);
			writePercentage(request.getDurationsSum(), globalRequest.getDurationsSum());
			write(nextColumn);
			write(integerFormat.format(request.getHits()));
			write(nextColumn);
			final int mean = request.getMean();
			write("<span class='");
			write(getSlaHtmlClass(mean));
			write("'>");
			write(integerFormat.format(mean));
			write("</span>");
			write(nextColumn);
			write(integerFormat.format(request.getMaximum()));
			write(nextColumn);
			write(integerFormat.format(request.getStandardDeviation()));
		} else {
			write(nextColumn);
			write(integerFormat.format(request.getHits()));
		}
		if (counterRequestAggregation.isCpuTimesDisplayed()) {
			write(nextColumn);
			writePercentage(request.getCpuTimeSum(), globalRequest.getCpuTimeSum());
			write(nextColumn);
			final int cpuTimeMean = request.getCpuTimeMean();
			write("<span class='");
			write(getSlaHtmlClass(cpuTimeMean));
			write("'>");
			write(integerFormat.format(cpuTimeMean));
			write("</span>");
		}
		if (counterRequestAggregation.isAllocatedKBytesDisplayed()) {
			write(nextColumn);
			final int allocatedKBytesMean = request.getAllocatedKBytesMean();
			write(integerFormat.format(allocatedKBytesMean));
		}
		if (!isErrorAndNotJobCounter()) {
			write(nextColumn);
			write(systemErrorFormat.format(request.getSystemErrorPercentage()));
		}
		if (counterRequestAggregation.isResponseSizeDisplayed()) {
			write(nextColumn);
			write(integerFormat.format(request.getResponseSizeMean() / 1024L));
		}
		if (counterRequestAggregation.isChildHitsDisplayed()) {
			write(nextColumn);
			write(integerFormat.format(request.getChildHitsMean()));
			write(nextColumn);
			write(integerFormat.format(request.getChildDurationsMean()));
		}
		write("</td>");
	}

	void writeRequestName(String requestId, String requestName, boolean includeGraph,
			boolean includeDetailLink, boolean includeSummaryPerClassLink) throws IOException {
		if (includeGraph) {
			assert includeDetailLink;
			assert !includeSummaryPerClassLink;
			htmlCounterRequestGraphReport.writeRequestGraph(requestId, requestName);
		} else if (includeDetailLink) {
			assert !includeSummaryPerClassLink;
			write("<a href='?part=graph&amp;graph=");
			write(requestId);
			write("'>");
			// writeDirectly pour ne pas gérer de traductions si le nom contient '#'
			writeDirectly(htmlEncodeRequestName(requestId, requestName));
			write("</a>");
		} else if (includeSummaryPerClassLink) {
			write("<a href='?part=counterSummaryPerClass&amp;counter=");
			write(counter.getName());
			write("&amp;graph=");
			write(requestId);
			write("'>");
			// writeDirectly pour ne pas gérer de traductions si le nom contient '#'
			writeDirectly(htmlEncodeRequestName(requestId, requestName));
			write("</a> ");
		} else {
			// writeDirectly pour ne pas gérer de traductions si le nom contient '#'
			writeDirectly(htmlEncodeRequestName(requestId, requestName));
		}
	}

	String getSlaHtmlClass(int mean) {
		final String color;
		if (mean < counterRequestAggregation.getWarningThreshold() || mean == 0) {
			// si cette moyenne est < à la moyenne globale + 1 écart-type (paramétrable), c'est bien
			// (si severeThreshold ou warningThreshold sont à 0 et mean à 0, c'est "info" et non "severe")
			color = "info";
		} else if (mean < counterRequestAggregation.getSevereThreshold()) {
			// sinon, si cette moyenne est < à la moyenne globale + 2 écart-types (paramétrable),
			// attention à cette requête qui est plus longue que les autres
			color = "warning";
		} else {
			// sinon, (cette moyenne est > à la moyenne globale + 2 écart-types),
			// cette requête est très longue par rapport aux autres ;
			// il peut être opportun de l'optimiser si possible
			color = "severe";
		}
		return color;
	}

	private void writePercentage(long dividende, long diviseur) throws IOException {
		if (diviseur == 0) {
			write("0");
		} else {
			write(integerFormat.format(100 * dividende / diviseur));
		}
	}

	/**
	 * Encode le nom d'une requête pour affichage en html, sans encoder les espaces en nbsp (insécables),
	 * et highlight les mots clés SQL.
	 * @param requestId Id de la requête
	 * @param requestName Nom de la requête à encoder
	 * @return String
	 */
	static String htmlEncodeRequestName(String requestId, String requestName) {
		if (requestId.startsWith(Counter.SQL_COUNTER_NAME)) {
			final String htmlEncoded = htmlEncodeButNotSpace(requestName);
			// highlight SQL keywords
			return SQL_KEYWORDS_PATTERN.matcher(htmlEncoded)
					.replaceAll("<span class='sqlKeyword'>$1</span>");
		}

		return htmlEncodeButNotSpace(requestName);
	}
}
