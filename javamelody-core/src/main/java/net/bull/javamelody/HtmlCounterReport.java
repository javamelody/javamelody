/*
 * Copyright 2008-2010 by Emeric Vernat
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
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Partie du rapport html pour un compteur.
 * @author Emeric Vernat
 */
class HtmlCounterReport {
	private static final boolean PDF_ENABLED = HtmlCoreReport.isPdfEnabled();
	private final Counter counter;
	private final Range range;
	private final Writer writer;
	private final CounterRequestAggregation counterRequestAggregation;
	private final HtmlCounterRequestGraphReport htmlCounterRequestGraphReport;
	private final DecimalFormat systemErrorFormat = I18N.createPercentFormat();
	private final DecimalFormat integerFormat = I18N.createIntegerFormat();

	static class HtmlCounterRequestGraphReport {
		private static final String SCRIPT_BEGIN = "<script type='text/javascript'>";
		private static final String SCRIPT_END = "</script>";
		private static int uniqueByPageAndGraphSequence;
		private final Range range;
		private final Writer writer;
		private final DecimalFormat systemErrorFormat = I18N.createPercentFormat();
		private final DecimalFormat nbExecutionsFormat = I18N.createPercentFormat();
		private final DecimalFormat integerFormat = I18N.createIntegerFormat();
		private List<Counter> counters;
		private Map<String, CounterRequest> requestsById;

		HtmlCounterRequestGraphReport(Range range, Writer writer) {
			super();
			assert range != null;
			assert writer != null;
			this.range = range;
			this.writer = writer;
		}

		void writeRequestGraph(String requestId, String requestName) throws IOException {
			uniqueByPageAndGraphSequence++;
			// la classe tooltip est configurée dans la css de HtmlReport
			write("<a class='tooltip' href='?part=graph&amp;graph=");
			write(requestId);
			write("'");
			// ce onmouseover sert à charger les graphs par requête un par un et à la demande
			// sans les charger tous au chargement de la page.
			// le onmouseover se désactive après chargement pour ne pas recharger une image déjà chargée
			write(" onmouseover=\"document.getElementById('");
			final String id = "id" + uniqueByPageAndGraphSequence;
			write(id);
			write("').src='?graph=");
			write(requestId);
			write("&amp;width=100&amp;height=50'; this.onmouseover=null;\" >");
			// avant mouseover on prend une image qui sera mise en cache
			write("<em><img src='?resource=db.png' id='");
			write(id);
			write("' alt='graph'/></em>");
			// writer.write pour ne pas gérer de traductions si le nom contient '#'
			writer.write(htmlEncode(requestName));
			write("</a>");
		}

		void writeRequestAndGraphDetail(Collector collector, String graphName) throws IOException {
			counters = collector.getRangeCountersToBeDisplayed(range);
			requestsById = mapAllRequestsById();
			final CounterRequest request = requestsById.get(graphName);
			if (request != null) {
				writeRequest(request);

				if (JdbcWrapper.SINGLETON.getSqlCounter().isRequestIdFromThisCounter(graphName)) {
					writeSqlRequestExplainPlan(request.getName());
				}
			}
			if (request == null || getCounterByRequestId(request) != null
					&& isRequestGraphDisplayed(getCounterByRequestId(request))) {
				writeln("<div id='track' class='noPrint'>");
				writeln("<div class='selected' id='handle'>");
				writeln("<img src='?resource=scaler_slider.gif' alt=''/>");
				writeln("</div></div>");

				writeln("<div align='center'><img class='synthèse' id='img' src='"
						+ "?width=960&amp;height=400&amp;graph=" + graphName
						+ "' alt='zoom'/></div>");
				writeln("<div align='right'><a href='?part=lastValue&amp;graph=" + graphName
						+ "' title=\"#Lien_derniere_valeur#\">_</a></div>");

				writeGraphDetailScript(graphName);
			}
			if (request != null && request.getStackTrace() != null) {
				writeln("<blockquote><blockquote><b>Stack-trace</b><br/><font size='-1'>");
				// writer.write pour ne pas gérer de traductions si la stack-trace contient '#'
				writer.write(htmlEncode(request.getStackTrace()).replaceAll("\t",
						"&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"));
				writeln("</font></blockquote></blockquote>");
			}
		}

		private void writeSqlRequestExplainPlan(String sqlRequest) throws IOException {
			try {
				final String explainPlan = DatabaseInformations.explainPlanFor(sqlRequest);
				// rq : si explainPlan était un tableau (ex: mysql),
				// on pourrait utiliser HtmlDatabaseInformationsReport.TableReport
				if (explainPlan != null) {
					writeln("<b>#Plan_d_execution#</b>");
					writeln("<div class='explainPlan'>");
					writer.write(explainPlan.replace(" ", "&nbsp;").replace("\n", "<br/>"));
					writeln("</div><hr/>");
				}
			} catch (final Exception e) {
				writeln("<b>#Plan_d_execution#</b> ");
				writeln(e.toString());
				writeln("<br/>");
			}
		}

		void writeRequestUsages(Collector collector, String requestId) throws IOException {
			assert requestId != null;
			counters = collector.getRangeCountersToBeDisplayed(range);
			CounterRequest myRequest = null;
			final List<CounterRequest> requests = new ArrayList<CounterRequest>();
			for (final Counter counter : counters) {
				for (final CounterRequest request : counter.getOrderedRequests()) {
					if (myRequest == null && request.getId().equals(requestId)) {
						myRequest = request;
					}
					if (request.containsChildRequest(requestId)) {
						requests.add(request);
					}
				}
			}
			writeRequestUsages(myRequest, requests);
		}

		private void writeRequestUsages(CounterRequest myRequest, List<CounterRequest> requests)
				throws IOException {
			writeln("<br/><b>#Utilisations_de#</b>");
			if (myRequest != null) {
				writer.write(htmlEncode(myRequest.getName()));
			}
			writeln("<br/><br/>");
			if (requests.isEmpty()) {
				writeln("#Aucune_requete#");
				return;
			}
			final boolean someUsagesDisplayed = getUsagesDisplayed(requests);
			writeln("<table class='sortable' width='100%' border='1' cellspacing='0' cellpadding='2' summary='#Utilisations_de#'>");
			write("<thead><tr><th>#Requete#</th>");
			if (someUsagesDisplayed) {
				write("<th class='noPrint'>#Chercher_utilisations#</th>");
			}
			writeln("</tr></thead><tbody>");
			boolean odd = false;
			for (final CounterRequest request : requests) {
				if (odd) {
					write("<tr class='odd' onmouseover=\"this.className='highlight'\" onmouseout=\"this.className='odd'\">");
				} else {
					write("<tr onmouseover=\"this.className='highlight'\" onmouseout=\"this.className=''\">");
				}
				odd = !odd; // NOPMD
				writeUsedRequest(request, someUsagesDisplayed);
				writeln("</tr>");
			}
			writeln("</tbody></table>");
		}

		private void writeUsedRequest(CounterRequest request, boolean someUsageDisplayed)
				throws IOException {
			writeln("<td>");
			writeCounterIcon(request);
			writeRequestGraph(request.getId(), request.getName());
			if (someUsageDisplayed) {
				writeln("</td><td align='center' class='noPrint'>");
				if (doesRequestDisplayUsages(request)) {
					writeln("<a href='?part=usages&amp;graph=" + request.getId() + "'>");
					writeln("<img src='?resource=find.png' alt='#Chercher_utilisations#' title='#Chercher_utilisations#'/></a>");
				} else {
					writeln("&nbsp;");
				}
			}
			writeln("</td>");
		}

		private boolean getUsagesDisplayed(List<CounterRequest> requests) {
			for (final CounterRequest request : requests) {
				if (doesRequestDisplayUsages(request)) {
					return true;
				}
			}
			return false;
		}

		private void writeRequest(CounterRequest request) throws IOException {
			final Map<String, Long> childRequests = request.getChildRequestsExecutionsByRequestId();
			writeln("<br/><table class='sortable' width='100%' border='1' cellspacing='0' cellpadding='2' summary='#Drill_down#'>");
			writeln("<thead><tr><th>#Requete#</th>");
			final boolean hasChildren = !childRequests.isEmpty();
			if (hasChildren) {
				writeln("<th class='sorttable_numeric'>#Hits_par_requete#</th>");
			}
			writeln("<th class='sorttable_numeric'>#Temps_moyen#</th><th class='sorttable_numeric'>#Temps_max#</th>");
			writeln("<th class='sorttable_numeric'>#Ecart_type#</th><th class='sorttable_numeric'>#Temps_cpu_moyen#</th>");
			writeln("<th class='sorttable_numeric'>#erreur_systeme#</th>");
			final Counter parentCounter = getCounterByRequestId(request);
			final boolean allChildHitsDisplayed = parentCounter != null
					&& parentCounter.getChildCounterName() != null;
			if (allChildHitsDisplayed) {
				final String childCounterName = parentCounter.getChildCounterName();
				writeln("<th class='sorttable_numeric'>"
						+ I18N.getFormattedString("hits_fils_moyens", childCounterName));
				writeln("</th><th class='sorttable_numeric'>"
						+ I18N.getFormattedString("temps_fils_moyen", childCounterName) + "</th>");
			}
			writeln("</tr></thead><tbody>");
			writeln("<tr onmouseover=\"this.className='highlight'\" onmouseout=\"this.className=''\"><td>");
			writeCounterIcon(request);
			writer.write(htmlEncode(request.getName()));
			if (hasChildren) {
				writeln("</td><td>&nbsp;");
			}
			writeRequestValues(request, allChildHitsDisplayed);
			writeln("</td></tr>");

			if (hasChildren) {
				writeChildRequests(request, childRequests, allChildHitsDisplayed);
			}
			writeln("</tbody></table>");
			if (doesRequestDisplayUsages(request)) {
				writeln("<div align='right' class='noPrint'>");
				writeln("<a href='?part=usages&amp;graph=" + request.getId() + "'>");
				writeln("<img src='?resource=find.png' alt='#Chercher_utilisations#' ");
				writeln("title='#Chercher_utilisations#'/> #Chercher_utilisations#</a></div>");
			} else {
				writeln("<br/>");
			}
		}

		private boolean doesRequestDisplayUsages(CounterRequest request) {
			final Counter parentCounter = getCounterByRequestId(request);
			return parentCounter != null && !parentCounter.isErrorCounter()
					&& !Counter.HTTP_COUNTER_NAME.equals(parentCounter.getName());
		}

		private void writeChildRequests(CounterRequest request, Map<String, Long> childRequests,
				boolean allChildHitsDisplayed) throws IOException {
			boolean odd = true;
			for (final Map.Entry<String, Long> entry : childRequests.entrySet()) {
				final CounterRequest childRequest = requestsById.get(entry.getKey());
				if (childRequest != null) {
					if (odd) {
						writeln("<tr class='odd' onmouseover=\"this.className='highlight'\" onmouseout=\"this.className='odd'\">");
					} else {
						writeln("<tr onmouseover=\"this.className='highlight'\" onmouseout=\"this.className=''\">");
					}
					odd = !odd; // NOPMD
					final Long nbExecutions = entry.getValue();
					final float executionsByRequest = (float) nbExecutions / request.getHits();
					writeChildRequest(childRequest, executionsByRequest, allChildHitsDisplayed);
					writeln("</tr>");
				}
			}
		}

		private void writeChildRequest(CounterRequest childRequest, float executionsByRequest,
				boolean allChildHitsDisplayed) throws IOException {
			writeln("<td>");
			writeln("<div style='margin-left: 10px;'>");
			writeCounterIcon(childRequest);
			writeRequestGraph(childRequest.getId(), childRequest.getName());
			writeln("</div></td><td align='right'>");
			write(nbExecutionsFormat.format(executionsByRequest));
			writeRequestValues(childRequest, allChildHitsDisplayed);
			writeln("</td>");
		}

		private void writeRequestValues(CounterRequest request, boolean allChildHitsDisplayed)
				throws IOException {
			final String nextColumn = "</td><td align='right'>";
			writeln(nextColumn);
			writeln(integerFormat.format(request.getMean()));
			writeln(nextColumn);
			writeln(integerFormat.format(request.getMaximum()));
			writeln(nextColumn);
			writeln(integerFormat.format(request.getStandardDeviation()));
			writeln(nextColumn);
			final String nbsp = "&nbsp;";
			if (request.getCpuTimeMean() >= 0) {
				writeln(integerFormat.format(request.getCpuTimeMean()));
			} else {
				writeln(nbsp);
			}
			writeln(nextColumn);
			writeln(systemErrorFormat.format(request.getSystemErrorPercentage()));
			if (allChildHitsDisplayed) {
				writeln(nextColumn);
				final boolean childHitsDisplayed = request.hasChildHits();
				if (childHitsDisplayed) {
					writeln(integerFormat.format(request.getChildHitsMean()));
				} else {
					writeln(nbsp);
				}
				writeln(nextColumn);
				if (childHitsDisplayed) {
					writeln(integerFormat.format(request.getChildDurationsMean()));
				} else {
					writeln(nbsp);
				}
			}
		}

		private void writeCounterIcon(CounterRequest request) throws IOException {
			final Counter parentCounter = getCounterByRequestId(request);
			if (parentCounter != null && parentCounter.getIconName() != null) {
				writeln("<img src='?resource=" + parentCounter.getIconName() + "' alt='"
						+ parentCounter.getName() + "' width='16' height='16' />&nbsp;");
			}
		}

		private void writeGraphDetailScript(String graphName) throws IOException {
			writeln(SCRIPT_BEGIN);
			writeln("function scaleImage(v, min, max) {");
			writeln("    var images = document.getElementsByClassName('synthèse');");
			writeln("    w = (max - min) * v + min;");
			writeln("    for (i = 0; i < images.length; i++) {");
			writeln("        images[i].style.width = w + 'px';");
			writeln("    }");
			writeln("}");

			// 'animate' our slider
			writeln("var slider = new Control.Slider('handle', 'track', {axis:'horizontal', alignX: 0, increment: 2});");

			// resize the image as the slider moves. The image quality would deteriorate, but it
			// would not be final anyway. Once slider is released the image is re-requested from the server, where
			// it is rebuilt from vector format
			writeln("slider.options.onSlide = function(value) {");
			writeln("  scaleImage(value, initialWidth, initialWidth / 2 * 3);");
			writeln("}");

			// this is where the slider is released and the image is reloaded
			// we use current style settings to work the required image dimensions
			writeln("slider.options.onChange = function(value) {");
			// chop off "px" and round up float values
			writeln("  width = Math.round(Element.getStyle('img','width').replace('px','')) - 80;");
			writeln("  height = Math.round(width * initialHeight / initialWidth) - 48;");
			// reload the images
			// rq : on utilise des caractères unicode pour éviter des warnings
			writeln("  document.getElementById('img').src = '?graph=" + graphName
					+ "\\u0026width=' + width + '\\u0026height=' + height;");
			writeln("  document.getElementById('img').style.width = '';");
			writeln("}");
			writeln("window.onload = function() {");
			writeln("  if (navigator.appName == 'Microsoft Internet Explorer') {");
			writeln("    initialWidth = document.getElementById('img').width;");
			writeln("    initialHeight = document.getElementById('img').height;");
			writeln("  } else {");
			writeln("    initialWidth = Math.round(Element.getStyle('img','width').replace('px',''));");
			writeln("    initialHeight = Math.round(Element.getStyle('img','height').replace('px',''));");
			writeln("  }");
			writeln("}");
			writeln(SCRIPT_END);
		}

		private Map<String, CounterRequest> mapAllRequestsById() {
			final Map<String, CounterRequest> result = new HashMap<String, CounterRequest>();
			for (final Counter counter : counters) {
				for (final CounterRequest request : counter.getRequests()) {
					result.put(request.getId(), request);
				}
			}
			return result;
		}

		private Counter getCounterByRequestId(CounterRequest request) {
			final String requestId = request.getId();
			for (final Counter counter : counters) {
				if (counter.isRequestIdFromThisCounter(requestId)) {
					return counter;
				}
			}
			return null;
		}

		private static String htmlEncode(String text) {
			return I18N.htmlEncode(text, false);
		}

		private void write(String html) throws IOException {
			I18N.writeTo(html, writer);
		}

		private void writeln(String html) throws IOException {
			I18N.writelnTo(html, writer);
		}
	}

	HtmlCounterReport(Counter counter, Range range, Writer writer) {
		super();
		assert counter != null;
		assert range != null;
		assert writer != null;
		this.counter = counter;
		this.range = range;
		this.writer = writer;
		this.counterRequestAggregation = new CounterRequestAggregation(counter);
		this.htmlCounterRequestGraphReport = new HtmlCounterRequestGraphReport(range, writer);
	}

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
		writeSizeAndLinks(requests, counterName, globalRequest);

		// 3. détails par requêtes (non visible par défaut)
		writeln("<div id='details" + counterName + "' style='display: none;'>");
		writeRequests(counterName, counter.getChildCounterName(), requests,
				isRequestGraphDisplayed(counter), true, false);
		writeln("</div>");

		// 4. logs (non visible par défaut)
		if (isErrorCounter()) {
			writeln("<div id='logs" + counterName + "' style='display: none;'>");
			new HtmlCounterErrorReport(counter, writer).toHtml();
			writeln("</div>");
		}
	}

	private void writeSizeAndLinks(List<CounterRequest> requests, String counterName,
			CounterRequest globalRequest) throws IOException {
		// delta ni négatif ni à 0
		final long deltaMillis = Math.max(System.currentTimeMillis()
				- counter.getStartDate().getTime(), 1);
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
		writeln(I18N.getFormattedString(nbKey, integerFormat.format(hitsParMinute),
				integerFormat.format(requests.size())));
		final String separator = "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;";
		if (counter.isBusinessFacadeCounter()) {
			writeln(separator);
			writeln("<a href='?part=counterSummaryPerClass&amp;counter=" + counterName
					+ "' class='noPrint'>#Resume_par_classe#</a>");
			if (PDF_ENABLED) {
				writeln(separator);
				writeln("<a href='?part=runtimeDependencies&amp;format=pdf&amp;counter="
						+ counterName + "' class='noPrint'>#Dependances#</a>");
			}
		}
		writeln(separator);
		writeShowHideLink("details" + counterName, "#Details#");
		if (isErrorCounter()) {
			writeln(separator);
			writeShowHideLink("logs" + counterName, "#Dernieres_erreurs#");
		}
		writeln(separator);
		if (range.getPeriod() == Period.TOUT) {
			writeln("<a href='?action=clear_counter&amp;counter=" + counterName + "' title='"
					+ I18N.getFormattedString("Vider_stats", counterName) + '\'');
			writeln("class='noPrint' onclick=\"javascript:return confirm('"
					+ I18N.javascriptEncode(I18N.getFormattedString("confirm_vider_stats",
							counterName)) + "');\">#Reinitialiser#</a>");
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

	static boolean isRequestGraphDisplayed(Counter parentCounter) {
		return !(parentCounter.isErrorCounter() && !parentCounter.isJobCounter())
				&& !parentCounter.isJspOrStrutsCounter();
	}

	void writeRequestsAggregatedOrFilteredByClassName(String requestId) throws IOException {
		final List<CounterRequest> requestsAggregatedByClassName = counterRequestAggregation
				.getRequestsAggregatedByClassName();
		final List<CounterRequest> requestList;
		if (requestId == null) {
			// on va afficher la liste des requêtes aggrégées par classe
			requestList = requestsAggregatedByClassName;
		} else {
			// on a un paramètre requestId, ie que l'utilisateur a cliqué sur un lien de détail
			// des requêtes pour une classe, et on va afficher la liste des requêtes non aggrégées
			// mais filtrées pour cette classe
			requestList = new ArrayList<CounterRequest>();
			// on recherche d'abord le nom de la classe à partir de requestId
			for (final CounterRequest requestAggregated : requestsAggregatedByClassName) {
				if (requestId.equals(requestAggregated.getId())) {
					final String className = requestAggregated.getName();
					// et on filtre les requêtes pour cette classe
					requestList.addAll(counterRequestAggregation
							.getRequestsFilteredByClassName(className));
					break;
				}
			}
		}
		final boolean includeSummaryPerClassLink = requestId == null;
		final boolean includeDetailLink = !includeSummaryPerClassLink;
		writeRequests(counter.getName(), counter.getChildCounterName(), requestList,
				includeDetailLink, includeDetailLink, includeSummaryPerClassLink);
	}

	private void writeRequests(String tableName, String childCounterName,
			List<CounterRequest> requestList, boolean includeGraph, boolean includeDetailLink,
			boolean includeSummaryPerClassLink) throws IOException {
		assert requestList != null;
		writeln("<table class='sortable' width='100%' border='1' cellspacing='0' cellpadding='2' summary='"
				+ tableName + "'>");
		writeTableHead(childCounterName);
		writeln("<tbody>");
		boolean odd = false;
		for (final CounterRequest request : requestList) {
			if (odd) {
				write("<tr class='odd' onmouseover=\"this.className='highlight'\" onmouseout=\"this.className='odd'\">");
			} else {
				write("<tr onmouseover=\"this.className='highlight'\" onmouseout=\"this.className=''\">");
			}
			odd = !odd; // NOPMD
			writeRequest(request, includeGraph, includeDetailLink, includeSummaryPerClassLink);
			writeln("</tr>");
		}
		writeln("</tbody></table>");
	}

	private void writeTableHead(String childCounterName) throws IOException {
		if (isJobCounter()) {
			write("<thead><tr><th>#Job#</th>");
		} else if (isErrorCounter()) {
			write("<thead><tr><th>#Erreur#</th>");
		} else {
			write("<thead><tr><th>#Requete#</th>");
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
		if (!isErrorAndNotJobCounter()) {
			write("<th class='sorttable_numeric'>#erreur_systeme#</th>");
		}
		if (counterRequestAggregation.isResponseSizeDisplayed()) {
			write("<th class='sorttable_numeric'>#Taille_moyenne#</th>");
		}
		if (counterRequestAggregation.isChildHitsDisplayed()) {
			write("<th class='sorttable_numeric'>"
					+ I18N.getFormattedString("hits_fils_moyens", childCounterName));
			write("</th><th class='sorttable_numeric'>"
					+ I18N.getFormattedString("temps_fils_moyen", childCounterName) + "</th>");
		}
		writeln("</tr></thead>");
	}

	private void writeRequest(CounterRequest request, boolean includeGraph,
			boolean includeDetailLink, boolean includeSummaryPerClassLink) throws IOException {
		final String nextColumn = "</td> <td align='right'>";
		write("<td>");
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
		if (!isErrorAndNotJobCounter()) {
			write(nextColumn);
			write(systemErrorFormat.format(request.getSystemErrorPercentage()));
		}
		if (counterRequestAggregation.isResponseSizeDisplayed()) {
			write(nextColumn);
			write(integerFormat.format(request.getResponseSizeMean() / 1024));
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
			// writer.write pour ne pas gérer de traductions si le nom contient '#'
			writer.write(htmlEncode(requestName));
			write("</a>");
		} else if (includeSummaryPerClassLink) {
			write("<a href='?part=counterSummaryPerClass&amp;counter=");
			write(counter.getName());
			write("&amp;graph=");
			write(requestId);
			write("'>");
			// writer.write pour ne pas gérer de traductions si le nom contient '#'
			writer.write(htmlEncode(requestName));
			write("</a> ");
		} else {
			// writer.write pour ne pas gérer de traductions si le nom contient '#'
			writer.write(htmlEncode(requestName));
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

	private static String htmlEncode(String text) {
		return I18N.htmlEncode(text, false);
	}

	private void writeShowHideLink(String idToShow, String label) throws IOException {
		writeln("<a href=\"javascript:showHide('" + idToShow + "');\" class='noPrint'><img id='"
				+ idToShow + "Img' src='?resource=bullets/plus.png' alt=''/> " + label + "</a>");
	}

	private void writePercentage(long dividende, long diviseur) throws IOException {
		if (diviseur == 0) {
			write("0");
		} else {
			write(integerFormat.format(100 * dividende / diviseur));
		}
	}

	private void write(String html) throws IOException {
		I18N.writeTo(html, writer);
	}

	private void writeln(String html) throws IOException {
		I18N.writelnTo(html, writer);
	}
}
