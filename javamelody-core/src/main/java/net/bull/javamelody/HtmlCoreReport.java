/*
 * Copyright 2008-2016 by Emeric Vernat
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

import java.io.IOException;
import java.io.Writer;
import java.net.URLEncoder;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * Rapport html principal.
 * @author Emeric Vernat
 */
class HtmlCoreReport extends HtmlAbstractReport {
	private static final int MAX_CURRENT_REQUESTS_DISPLAYED_IN_MAIN_REPORT = 500;
	private static final int MAX_THREADS_DISPLAYED_IN_MAIN_REPORT = 500;
	private static final String SEPARATOR = "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;";
	private static final String END_DIV = "</div>";
	private static final String SCRIPT_BEGIN = "<script type='text/javascript'>";
	private static final String SCRIPT_END = "</script>";

	private final Collector collector;
	private final List<JavaInformations> javaInformationsList;
	private final Range range;
	private final CollectorServer collectorServer;
	private final long start = System.currentTimeMillis();
	private final Map<String, String> menuTextsByAnchorName = new LinkedHashMap<String, String>();

	private static class HtmlForms extends HtmlAbstractReport {
		HtmlForms(Writer writer) {
			super(writer);
		}

		void writeCustomPeriodLink(Range range, String graphName, String part) throws IOException {
			writeln("<a href=\"javascript:showHide('customPeriod');document.customPeriodForm.startDate.focus();\" ");
			final String linkLabel = getString("personnalisee");
			writeln("title='" + getFormattedString("Choisir_periode", linkLabel) + "'>");
			writeln("<img src='?resource=calendar.png' alt='#personnalisee#' /> #personnalisee#</a>");
			writeln("<div id='customPeriod' style='display: none;'>");
			writeln(SCRIPT_BEGIN);
			writeln("function validateCustomPeriodForm() {");
			writeln("   periodForm = document.customPeriodForm;");
			writelnCheckMandatory("periodForm.startDate", "dates_mandatory");
			writelnCheckMandatory("periodForm.endDate", "dates_mandatory");
			writeln("   periodForm.period.value=periodForm.startDate.value + '"
					+ Range.CUSTOM_PERIOD_SEPARATOR + "' + periodForm.endDate.value;");
			writeln("   return true;");
			writeln("}");
			writeln(SCRIPT_END);
			writeln("<br/>");
			final DateFormat dateFormat = I18N.createDateFormat();
			final String dateFormatPattern;
			if (getString("dateFormatPattern").length() == 0) {
				final String pattern = ((SimpleDateFormat) dateFormat).toPattern();
				dateFormatPattern = pattern.toLowerCase(I18N.getCurrentLocale());
			} else {
				dateFormatPattern = getString("dateFormatPattern");
			}
			writeln("<form name='customPeriodForm' method='get' action='' onsubmit='return validateCustomPeriodForm();'>");
			writeln("<br/><b>#startDate#</b>&nbsp;&nbsp;<input type='text' size='10' name='startDate' ");
			if (range.getStartDate() != null) {
				writeln("value='" + dateFormat.format(range.getStartDate()) + '\'');
			}
			writeln("/>&nbsp;&nbsp;<b>#endDate#</b>&nbsp;&nbsp;<input type='text' size='10' name='endDate' ");
			if (range.getEndDate() != null) {
				writeln("value='" + dateFormat.format(range.getEndDate()) + '\'');
			}
			writeln("/>&nbsp;&nbsp;");
			writeDirectly('(' + dateFormatPattern + ')');
			writeln("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<input type='submit' value='#ok#'/><br/><br/>");
			writeln("<input type='hidden' name='period' value=''/>");
			if (graphName != null) {
				writeln("<input type='hidden' name='part' value='" + part + "'/>");
				writeln("<input type='hidden' name='graph' value='" + urlEncode(graphName) + "'/>");
			}
			writeln("</form><br/>");
			writeln(END_DIV);
		}

		void writeAddAndRemoveApplicationLinks(String currentApplication) throws IOException {
			if (currentApplication == null) {
				writeln("<div align='center'><h3>#add_application#</h3>");
				writeln("#collect_server_intro#");
			} else {
				final String separator = "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;";
				writeln(separator);
				writeln("<a href=\"javascript:showHide('addApplication');document.appForm.appName.focus();\"");
				writeln(" class='noPrint'><img src='?resource=action_add.png' alt='#add_application#'/> #add_application#</a>");
				writeln(separator);
				writeln("<a href='?action=remove_application&amp;application=" + currentApplication
						+ getCsrfTokenUrlPart() + "' class='noPrint' ");
				final String messageConfirmation = getFormattedString("confirm_remove_application",
						currentApplication);
				writeln("onclick=\"javascript:return confirm('"
						+ javascriptEncode(messageConfirmation) + "');\">");
				final String removeApplicationLabel = getFormattedString("remove_application",
						currentApplication);
				writeln("<img src='?resource=action_delete.png' alt=\"" + removeApplicationLabel
						+ "\"/> " + removeApplicationLabel + "</a>");
				writeln("<div id='addApplication' style='display: none;'>");
			}
			writeln(SCRIPT_BEGIN);
			writeln("function validateAppForm() {");
			writelnCheckMandatory("document.appForm.appName", "app_name_mandatory");
			writelnCheckMandatory("document.appForm.appUrls", "app_urls_mandatory");
			writeln("   return true;");
			writeln("}");
			writeln(SCRIPT_END);
			writeln("<br/> <br/>");
			writeln("<form name='appForm' method='post' action='' onsubmit='return validateAppForm();'>");
			writeln("<br/><b>#app_name_to_monitor# :</b>&nbsp;&nbsp;<input type='text' size='15' name='appName'/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
			writeln("<b>#app_urls# :</b>&nbsp;&nbsp;<input type='text' size='50' name='appUrls'/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
			writeln("<input type='submit' value='#add#'/><br/>");
			writeln("#urls_sample# : <i>http://myhost/myapp/</i> #or# <i>http://host1/myapp/,http://host2/myapp/</i>");
			writeln("<br/> <br/>");
			writeln("</form>");
			writeln("</div>\n");
		}

		private void writelnCheckMandatory(String fieldFullName, String msgKey) throws IOException {
			writeln("   if (" + fieldFullName + ".value.length == 0) {");
			writeln("      alert('" + getStringForJavascript(msgKey) + "');");
			writeln("      " + fieldFullName + ".focus();");
			writeln("      return false;");
			writeln("   }");
		}

		@Override
		void toHtml() {
			throw new UnsupportedOperationException();
		}
	}

	HtmlCoreReport(Collector collector, CollectorServer collectorServer,
			List<JavaInformations> javaInformationsList, Range range, Writer writer) {
		super(writer);
		assert collector != null;
		assert javaInformationsList != null && !javaInformationsList.isEmpty();
		assert range != null;

		this.collector = collector;
		this.collectorServer = collectorServer;
		this.javaInformationsList = javaInformationsList;
		this.range = range;
	}

	@Override
	void toHtml() throws IOException {
		toHtml(null, null);
	}

	void toHtml(String message, String anchorNameForRedirect) throws IOException {
		if (collectorServer != null) {
			writeApplicationsLinks();
		}

		writeln("<h3 class='chapterTitle'><img src='?resource=systemmonitor.png' alt='#Stats#'/>");
		writeAnchor("top", I18N.getString("Stats"));
		writeSummary();
		writeln("</h3>");
		write("<a href='https://github.com/javamelody/javamelody/wiki/Donate'>");
		writeln("<img class='noPrint' style='position: absolute; top: 15px; right: 10px; border: 0;' src='?resource=donate.gif' alt='Donate' /></a>");
		writeln("<div align='center'>");
		writeRefreshAndPeriodLinks(null, null);
		writeGraphs();
		writeln(END_DIV);

		final List<Counter> counters = collector.getRangeCountersToBeDisplayed(range);
		final Map<String, HtmlCounterReport> counterReportsByCounterName = writeCounters(counters);

		if (collectorServer == null) {
			writeln("<h3 class='chapterTitle'><img src='?resource=hourglass.png' alt='#Requetes_en_cours#'/>");
			writeAnchor("currentRequests", I18N.getString("Requetes_en_cours"));
			writeln("#Requetes_en_cours#</h3>");
			// si on n'est pas sur le serveur de collecte il n'y a qu'un javaInformations
			writeCurrentRequests(javaInformationsList.get(0), counters,
					counterReportsByCounterName);
		}

		writeln("<h3 class='chapterTitle'><img src='?resource=systeminfo.png' alt='#Informations_systemes#'/>");
		writeAnchor("systeminfo", I18N.getString("Informations_systemes"));
		writeln("#Informations_systemes#</h3>");
		if (collectorServer != null) {
			writeln("<div align='center' class='noPrint'><a href='?part=currentRequests'>");
			writeln("<img src='?resource=hourglass.png' width='20' height='20' alt=\"#Voir_requetes_en_cours#\" /> #Voir_requetes_en_cours#</a>");
			writeln(END_DIV);
			writeln("<br/>");
		}
		if (Parameters.isSystemActionsEnabled()) {
			writeSystemActionsLinks();
		}

		new HtmlJavaInformationsReport(javaInformationsList, getWriter()).toHtml();

		writeln("<h3 class='chapterTitle' style='clear:both;'><img src='?resource=threads.png' alt='#Threads#'/>");
		writeAnchor("threads", I18N.getString("Threads"));
		writeln("#Threads#</h3>");
		writeThreads();

		if (isJobEnabled()) {
			writeln("<h3 class='chapterTitle'><img src='?resource=jobs.png' alt='#Jobs#'/>");
			writeAnchor("jobs", I18N.getString("Jobs"));
			writeln("#Jobs#</h3>");
			final Counter rangeJobCounter = collector.getRangeCounter(range,
					Counter.JOB_COUNTER_NAME);
			writeJobs(rangeJobCounter);
			writeCounter(rangeJobCounter);
		}

		if (isCacheEnabled()) {
			writeln("<h3 class='chapterTitle'><img src='?resource=caches.png' alt='#Caches#'/>");
			writeAnchor("caches", I18N.getString("Caches"));
			writeln("#Caches#</h3>");
			writeCaches();
		}
		//		else if (JavaInformations.STACK_TRACES_ENABLED) {
		//			// pour que les tooltips des stack traces s'affichent dans le scroll
		//			writeln("<br/><br/><br/><br/>");
		//		}

		writeMenu();

		writeMessageIfNotNull(message, null, anchorNameForRedirect);
		writeDurationAndOverhead();
	}

	private void writeSummary() throws IOException {
		final String javaMelodyUrl = "<a href='https://github.com/javamelody/javamelody/wiki' target='_blank'>JavaMelody</a>";
		if (range.getPeriod() == Period.TOUT) {
			final String startDate = I18N.createDateAndTimeFormat()
					.format(collector.getCounters().get(0).getStartDate());
			writeDirectly(getFormattedString("Statistiques", javaMelodyUrl,
					I18N.getCurrentDateAndTime(), startDate, collector.getApplication()));
		} else {
			writeDirectly(getFormattedString("Statistiques_sans_depuis", javaMelodyUrl,
					I18N.getCurrentDateAndTime(), collector.getApplication()));
		}
		if (javaInformationsList.get(0).getContextDisplayName() != null) {
			writeDirectly(htmlEncodeButNotSpace(
					" (" + javaInformationsList.get(0).getContextDisplayName() + ')'));
		}
		writeln("");
	}

	private void writeAnchor(String anchorName, String menuText) throws IOException {
		write("<a name='" + anchorName + "'></a>");
		menuTextsByAnchorName.put(anchorName, menuText);
	}

	private void writeMenu() throws IOException {
		writeln(SCRIPT_BEGIN);
		writeln("function toggle(id) {");
		writeln("var el = document.getElementById(id);");
		writeln("if (el.getAttribute('class') == 'menuHide') {");
		writeln("  el.setAttribute('class', 'menuShow');");
		writeln("} else {");
		writeln("  el.setAttribute('class', 'menuHide');");
		writeln("} }");
		writeln(SCRIPT_END);

		writeln("<div class='noPrint'> ");
		writeln("<div id='menuBox' class='menuHide'>");
		writeln("  <ul id='menuTab'><li><a href='javascript:toggle(\"menuBox\");'><img id='menuToggle' src='?resource=menu.png' alt='menu' /></a></li></ul>");
		writeln("  <div id='menuLinks'><div id='menuDeco'>");
		for (final Map.Entry<String, String> entry : menuTextsByAnchorName.entrySet()) {
			final String anchorName = entry.getKey();
			final String menuText = entry.getValue();
			writeDirectly("    <div class='menuButton'><a href='#" + anchorName + "'>" + menuText
					+ "</a></div>");
			writeln("");
		}
		final String customReports = Parameters.getParameter(Parameter.CUSTOM_REPORTS);
		if (customReports != null) {
			for (final String customReport : customReports.split(",")) {
				final String customReportName = customReport.trim();
				final String customReportPath = Parameters.getParameterByName(customReportName);
				if (customReportPath == null) {
					LOG.debug("Parameter not defined in web.xml for custom report: "
							+ customReportName);
					continue;
				}
				writeDirectly("    <div class='menuButton'><a href='?report="
						+ URLEncoder.encode(customReportName, "UTF-8") + "'>"
						+ htmlEncode(customReportName) + "</a></div>");
				writeln("");
			}
		}
		if (SessionListener.getCurrentSession() != null) {
			writeDirectly("    <div class='menuButton'><a href='?action=logout"
					+ getCsrfTokenUrlPart() + "'>" + I18N.getString("logout") + "</a></div>");
			writeln("");
		}
		writeln("  </div></div>");
		writeln("</div></div>");
	}

	private Map<String, HtmlCounterReport> writeCounters(List<Counter> counters)
			throws IOException {
		final Map<String, HtmlCounterReport> counterReportsByCounterName = new HashMap<String, HtmlCounterReport>();
		for (final Counter counter : counters) {
			final HtmlCounterReport htmlCounterReport = writeCounter(counter);
			counterReportsByCounterName.put(counter.getName(), htmlCounterReport);
		}

		if (range.getPeriod() == Period.TOUT && counterReportsByCounterName.size() > 1) {
			writeln("<div align='right'>");
			writeln("<a href='?action=clear_counter&amp;counter=all" + getCsrfTokenUrlPart()
					+ "' title='#Vider_toutes_stats#'");
			writeln("class='noPrint' onclick=\"javascript:return confirm('"
					+ getStringForJavascript("confirm_vider_toutes_stats")
					+ "');\">#Reinitialiser_toutes_stats#</a>");
			writeln(END_DIV);
		}

		return counterReportsByCounterName;
	}

	private HtmlCounterReport writeCounter(Counter counter) throws IOException {
		writeCounterTitle(counter);
		final HtmlCounterReport htmlCounterReport = new HtmlCounterReport(counter, range,
				getWriter());
		htmlCounterReport.toHtml();
		return htmlCounterReport;
	}

	private void writeCounterTitle(Counter counter) throws IOException {
		writeln("<h3 class='chapterTitle'><img src='?resource=" + counter.getIconName() + "' alt='"
				+ counter.getName() + "'/>");
		writeAnchor(counter.getName(),
				I18N.getString("Stats") + ' ' + counter.getName().toLowerCase(Locale.ENGLISH));
		final String counterLabel = getString(counter.getName() + "Label");
		write(getFormattedString("Statistiques_compteur", counterLabel));
		writeln(" - " + range.getLabel() + "</h3>");
	}

	static void writeAddAndRemoveApplicationLinks(String currentApplication, Writer writer)
			throws IOException {
		new HtmlForms(writer).writeAddAndRemoveApplicationLinks(currentApplication);
	}

	void writeMessageIfNotNull(String message, String partToRedirectTo,
			String anchorNameForRedirect) throws IOException {
		if (message != null) {
			writeln(SCRIPT_BEGIN);
			// writeDirectly pour ne pas gérer de traductions si le message contient '#'
			writeDirectly("alert(\"" + javascriptEncode(message) + "\");");
			writeln("");
			// redirect vers une url évitant que F5 du navigateur ne refasse l'action au lieu de faire un refresh
			if (partToRedirectTo == null) {
				if (anchorNameForRedirect == null) {
					writeln("location.href = '?'");
				} else {
					writeln("if (location.href.indexOf('?') != -1) {");
					writeDirectly(
							"location.href = location.href.substring(0, location.href.indexOf('?')) + '#"
									+ anchorNameForRedirect + "';");
					writeln("} else {");
					writeDirectly("location.href = '#" + anchorNameForRedirect + "';");
					writeln("}");
				}
			} else {
				writeln("location.href = '?part=" + partToRedirectTo + '\'');
			}
			writeln(SCRIPT_END);
		}
	}

	private void writeGraphs() throws IOException {
		if (collector.isStopped()) {
			// pas de graphs, ils seraient en erreur sans timer
			// mais un message d'avertissement à la place
			writeln("<div align='center' class='severe'><br/><br/>");
			writeln("#collect_server_misusage#");
			writeln("</div>");
			return;
		}

		writeGraphs(collector.getDisplayedCounterJRobins());
		final Collection<JRobin> otherJRobins = collector.getDisplayedOtherJRobins();
		if (!otherJRobins.isEmpty()) {
			writeln("<div align='right'>");
			writeShowHideLink("detailsGraphs", "#Autres_courbes#");
			writeln(END_DIV);
			writeln("<div id='detailsGraphs' style='display: none;'><div>");
			writeGraphs(otherJRobins);
			writeln("</div></div>");
		}
	}

	private void writeGraphs(Collection<JRobin> jrobins) throws IOException {
		int i = 0;
		for (final JRobin jrobin : jrobins) {
			final String jrobinName = jrobin.getName();
			writeln("<a href='?part=graph&amp;graph=" + jrobinName
					+ "'><img class='synthese' src='?width=200&amp;height=" + JRobin.SMALL_HEIGHT
					+ "&amp;graph=" + jrobinName + "' alt=\"" + jrobin.getLabel() + "\" title=\""
					+ jrobin.getLabel() + "\"/></a>");
			i++;
			if (i % 3 == 0) {
				// un <br/> après httpSessions et avant activeThreads pour l'alignement
				writeln("<br/>");
			}
		}
	}

	private void writeCurrentRequests(JavaInformations javaInformations, List<Counter> counters,
			Map<String, HtmlCounterReport> counterReportsByCounterName) throws IOException {
		final List<ThreadInformations> threadInformationsList = javaInformations
				.getThreadInformationsList();
		final boolean stackTraceEnabled = javaInformations.isStackTraceEnabled();
		final List<CounterRequestContext> rootCurrentContexts = collector
				.getRootCurrentContexts(counters);
		writeCurrentRequests(threadInformationsList, rootCurrentContexts, stackTraceEnabled,
				MAX_CURRENT_REQUESTS_DISPLAYED_IN_MAIN_REPORT, false, counterReportsByCounterName);
	}

	void writeAllCurrentRequestsAsPart(
			Map<JavaInformations, List<CounterRequestContext>> currentRequests) throws IOException {
		writeln("<div  class='noPrint'>");
		writeln("<a  href='javascript:history.back()'><img src='?resource=action_back.png' alt='#Retour#'/> #Retour#</a>");
		writeln(SEPARATOR);
		writeln("<a href='?part=currentRequests'><img src='?resource=action_refresh.png' alt='#Actualiser#'/> #Actualiser#</a>");
		if (isPdfEnabled()) {
			writeln(SEPARATOR);
			write("<a href='?part=currentRequests&amp;format=pdf' title='#afficher_PDF#'>");
			write("<img src='?resource=pdf.png' alt='#PDF#'/> #PDF#</a>");
		}
		writeln("</div><br/>");
		for (final Map.Entry<JavaInformations, List<CounterRequestContext>> entry : currentRequests
				.entrySet()) {
			final JavaInformations javaInformations = entry.getKey();
			final List<CounterRequestContext> rootCurrentContexts = entry.getValue();

			final List<ThreadInformations> threadInformationsList = javaInformations
					.getThreadInformationsList();
			final boolean stackTraceEnabled = javaInformations.isStackTraceEnabled();
			writeCurrentRequests(threadInformationsList, rootCurrentContexts, stackTraceEnabled,
					Integer.MAX_VALUE, true, null);
			write("<br/><br/>");
		}
	}

	private void writeCurrentRequests(List<ThreadInformations> threadInformationsList,
			List<CounterRequestContext> rootCurrentContexts, boolean stackTraceEnabled,
			int maxContextsDisplayed, boolean onlyTitleAndDetails,
			Map<String, HtmlCounterReport> counterReportsByCounterName) throws IOException {
		// counterReportsByCounterName peut être null
		final HtmlCounterRequestContextReport htmlCounterRequestContextReport = new HtmlCounterRequestContextReport(
				rootCurrentContexts, counterReportsByCounterName, threadInformationsList,
				stackTraceEnabled, maxContextsDisplayed, getWriter());
		if (onlyTitleAndDetails) {
			htmlCounterRequestContextReport.writeTitleAndDetails();
		} else {
			htmlCounterRequestContextReport.toHtml();
		}
	}

	void writeAllThreadsAsPart() throws IOException {
		final String separator = "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;";
		writeln("<div class='noPrint'>");
		writeln("<a href='javascript:history.back()'><img src='?resource=action_back.png' alt='#Retour#'/> #Retour#</a>");
		writeln(separator);
		writeln("<a href='?part=threads'><img src='?resource=action_refresh.png' alt='#Actualiser#'/> #Actualiser#</a>");
		for (final JavaInformations javaInformations : javaInformationsList) {
			if (javaInformations.isStackTraceEnabled()) {
				writeln(separator);
				writeln("<a href='?part=threadsDump'><img src='?resource=text.png' alt='#Dump_threads_en_texte#'/>&nbsp;#Dump_threads_en_texte#</a>");
				break;
			}
		}

		writeln("</div> <br/>");
		writeTitle("threads.png", getString("Threads"));
		write(" <br/>");

		for (final JavaInformations javaInformations : javaInformationsList) {
			write(" <b>");
			writeDirectly(getFormattedString("Threads_sur", javaInformations.getHost()));
			write(": </b>");
			writeln(getFormattedString("thread_count", javaInformations.getThreadCount(),
					javaInformations.getPeakThreadCount(),
					javaInformations.getTotalStartedThreadCount()));
			final HtmlThreadInformationsReport htmlThreadInformationsReport = new HtmlThreadInformationsReport(
					javaInformations.getThreadInformationsList(),
					javaInformations.isStackTraceEnabled(), getWriter());
			htmlThreadInformationsReport.writeDeadlocks();
			writeln("<br/><br/>");
			htmlThreadInformationsReport.toHtml();
		}
	}

	void writeThreadsDump() throws IOException {
		writeDirectly(I18N.getCurrentDateAndTime());
		writeDirectly("\n\n");
		for (final JavaInformations javaInformations : javaInformationsList) {
			writeDirectly("===== " + getFormattedString("Threads_sur", javaInformations.getHost())
					+ " =====");
			writeDirectly("\n\n");
			final HtmlThreadInformationsReport htmlThreadInformationsReport = new HtmlThreadInformationsReport(
					javaInformations.getThreadInformationsList(),
					javaInformations.isStackTraceEnabled(), getWriter());
			htmlThreadInformationsReport.writeThreadsDump();
		}
	}

	private void writeThreads() throws IOException {
		int i = 0;
		for (final JavaInformations javaInformations : javaInformationsList) {
			write("<b>");
			writeDirectly(getFormattedString("Threads_sur", javaInformations.getHost()));
			write(": </b>");
			writeln(getFormattedString("thread_count", javaInformations.getThreadCount(),
					javaInformations.getPeakThreadCount(),
					javaInformations.getTotalStartedThreadCount()));
			writeln(SEPARATOR);
			final List<ThreadInformations> threadInformationsList = javaInformations
					.getThreadInformationsList();
			final HtmlThreadInformationsReport htmlThreadInformationsReport = new HtmlThreadInformationsReport(
					threadInformationsList, javaInformations.isStackTraceEnabled(), getWriter());
			if (threadInformationsList.size() <= MAX_THREADS_DISPLAYED_IN_MAIN_REPORT) {
				final String id = "threads_" + i;
				writeShowHideLink(id, "#Details#");
				htmlThreadInformationsReport.writeDeadlocks();
				writeln("<br/><br/><div id='" + id + "' style='display: none;'>");
				htmlThreadInformationsReport.toHtml();

				writeln("<div align='right' class='noPrint'><br/>");
				if (javaInformations.isStackTraceEnabled()) {
					writeln("<a href='?part=threadsDump'><img src='?resource=text.png' alt='#Dump_threads_en_texte#'/>&nbsp;#Dump_threads_en_texte#</a>");
				}
				writeln(SEPARATOR);
				writeln("<a href='?part=threads'><img src='?resource=threads.png' alt='#Threads#' width='16' height='16'/>&nbsp;#Voir_dans_une_nouvelle_page#</a>");
				writeln("</div>");

				writeln("</div><br/>");
			} else {
				// le nombre de threads dépasse le maximum pour être affiché dans le rapport
				// principal, donc on affiche seulement un lien vers la page à part
				writeln("<a href='?part=threads' class='noPrint'>#Details#</a><br/>");
			}
			i++;
		}
	}

	void writeCounterSummaryPerClass(String counterName, String requestId) throws IOException {
		final Counter counter = collector.getRangeCounter(range, counterName);
		writeln("<div class='noPrint'>");
		writeln("<a href='javascript:history.back()'><img src='?resource=action_back.png' alt='#Retour#'/> #Retour#</a>");
		writeln(SEPARATOR);
		final String hrefStart = "<a href='?part=counterSummaryPerClass&amp;counter="
				+ counter.getName()
				+ (requestId == null ? "" : "&amp;graph=" + urlEncode(requestId));
		writeln(hrefStart + "'>");
		writeln("<img src='?resource=action_refresh.png' alt='#Actualiser#'/> #Actualiser#</a>");

		if (isPdfEnabled()) {
			writeln(SEPARATOR);
			write(hrefStart);
			writeln("&amp;format=pdf' title='#afficher_PDF#'>");
			write("<img src='?resource=pdf.png' alt='#PDF#'/> #PDF#</a>");
		}
		writeln("</div>");

		writeCounterTitle(counter);
		final HtmlCounterReport htmlCounterReport = new HtmlCounterReport(counter, range,
				getWriter());
		htmlCounterReport.writeRequestsAggregatedOrFilteredByClassName(requestId);
	}

	private boolean isGcEnabled() {
		return Action.GC_ENABLED || collectorServer != null;
	}

	private boolean isHeapHistoEnabled() {
		return collectorServer != null || VirtualMachine.isEnabled();
	}

	private boolean isSamplingEnabled() {
		return collectorServer != null || collector.getSamplingProfiler() != null;
	}

	private boolean isDatabaseEnabled() {
		return !Parameters.isNoDatabase()
				&& javaInformationsList.get(0).getDataBaseVersion() != null
				&& !javaInformationsList.get(0).getDataBaseVersion().contains("Exception");
	}

	private boolean doesWebXmlExists() {
		return javaInformationsList.get(0).doesWebXmlExists();
	}

	private boolean isSessionsEnabled() {
		return javaInformationsList.get(0).getSessionCount() >= 0;
	}

	private boolean isCacheEnabled() {
		for (final JavaInformations javaInformations : javaInformationsList) {
			if (javaInformations.isCacheEnabled()) {
				return true;
			}
		}
		return false;
	}

	private boolean isJobEnabled() {
		for (final JavaInformations javaInformations : javaInformationsList) {
			if (javaInformations.isJobEnabled()) {
				return true;
			}
		}
		return false;
	}

	private void writeCaches() throws IOException {
		int i = 0;
		for (final JavaInformations javaInformations : javaInformationsList) {
			if (!javaInformations.isCacheEnabled()) {
				continue;
			}
			final List<CacheInformations> cacheInformationsList = javaInformations
					.getCacheInformationsList();
			writeln("<b>");
			writeln(getFormattedString("caches_sur", cacheInformationsList.size(),
					javaInformations.getHost()));
			writeln("</b>");
			writeln(SEPARATOR);
			final String id = "caches_" + i;
			writeShowHideLink(id, "#Details#");
			writeln("<br/><br/><div id='" + id + "' style='display: none;'><div>");
			new HtmlCacheInformationsReport(cacheInformationsList, getWriter()).toHtml();
			writeln("</div></div><br/>");
			i++;
		}
	}

	private void writeJobs(Counter rangeJobCounter) throws IOException {
		int i = 0;
		for (final JavaInformations javaInformations : javaInformationsList) {
			if (!javaInformations.isJobEnabled()) {
				continue;
			}
			final List<JobInformations> jobInformationsList = javaInformations
					.getJobInformationsList();
			writeln("<b>");
			writeln(getFormattedString("jobs_sur", jobInformationsList.size(),
					javaInformations.getHost(), javaInformations.getCurrentlyExecutingJobCount()));
			writeln("</b>");
			writeln(SEPARATOR);
			final String id = "job_" + i;
			writeShowHideLink(id, "#Details#");
			writeln("<br/><br/><div id='" + id + "' style='display: none;'><div>");
			new HtmlJobInformationsReport(jobInformationsList, rangeJobCounter, getWriter())
					.toHtml();
			writeln("</div></div><br/>");
			i++;
		}
	}

	// CHECKSTYLE:OFF
	private void writeSystemActionsLinks() throws IOException {
		// CHECKSTYLE:ON
		writeln("<div align='center' class='noPrint'>");
		final String separator = "&nbsp;&nbsp;&nbsp;&nbsp;";
		final String endOfOnClickConfirm = "');\">";
		if (isGcEnabled()) {
			write("<a href='?action=gc" + getCsrfTokenUrlPart()
					+ "' onclick=\"javascript:return confirm('"
					+ getStringForJavascript("confirm_ramasse_miette") + endOfOnClickConfirm);
			write("<img src='?resource=broom.png' width='20' height='20' alt='#ramasse_miette#' /> #ramasse_miette#</a>");
			writeln(separator);
		} else {
			write("<a href='?action=gc" + getCsrfTokenUrlPart() + "' onclick=\"javascript:alert('"
					+ getStringForJavascript("ramasse_miette_desactive") + "');return false;\">");
			write("<img src='?resource=broom.png' width='20' height='20' alt='#ramasse_miette#' /> #ramasse_miette#</a>");
			writeln(separator);
		}
		write("<a href='?action=heap_dump" + getCsrfTokenUrlPart()
				+ "' onclick=\"javascript:return confirm('"
				+ getStringForJavascript("confirm_heap_dump") + endOfOnClickConfirm);
		write("<img src='?resource=heapdump.png' width='20' height='20' alt=\"#heap_dump#\" /> #heap_dump#</a>");
		writeln(separator);
		if (isHeapHistoEnabled()) {
			write("<a href='?part=heaphisto'>");
			write("<img src='?resource=memory.png' width='20' height='20' alt=\"#heaphisto#\" /> #heaphisto#</a>");
			writeln(separator);
		}
		if (isSessionsEnabled()) {
			write("<a href='?action=invalidate_sessions" + getCsrfTokenUrlPart()
					+ "' onclick=\"javascript:return confirm('"
					+ getStringForJavascript("confirm_invalidate_sessions") + endOfOnClickConfirm);
			write("<img src='?resource=user-trash.png' width='18' height='18' alt=\"#invalidate_sessions#\" /> #invalidate_sessions#</a>");
			writeln(separator);
			write("<a href='?part=sessions'>");
			writeln("<img src='?resource=system-users.png' width='20' height='20' alt=\"#sessions#\" /> #sessions#</a>");
		}
		if (isSamplingEnabled()) {
			writeln(separator);
			write("<a href='?part=hotspots'>");
			writeln("<img src='?resource=clock.png' width='20' height='20' alt=\"#hotspots#\" /> #hotspots#</a>");
		}

		writeln("<br />");
		if (doesWebXmlExists()) {
			// on n'affiche le lien web.xml que si le fichier existe (pour api servlet 3.0 par ex)
			writeln(separator);
			write("<a href='?part=web.xml'>");
			write("<img src='?resource=xml.png' width='20' height='20' alt=\"#web.xml#\" /> #web.xml#</a>");
		}

		writeln(separator);
		write("<a href='?part=mbeans'>");
		write("<img src='?resource=mbeans.png' width='20' height='20' alt=\"#MBeans#\" /> #MBeans#</a>");

		writeln(separator);
		write("<a href='?part=processes'>");
		write("<img src='?resource=processes.png' width='20' height='20' alt=\"#processes#\" /> #processes#</a>");

		final String serverInfo = javaInformationsList.get(0).getServerInfo();
		if (serverInfo != null && !serverInfo.contains("Winstone")) {
			// on n'affiche pas le lien JNDI si serveur Winstone car cela n'a pas d'intérêt
			// pour Jenkins sous Winstone, et surtout car (Winstone)Context.listBindings
			// renvoie une liste de NameClassPair au lieu d'une liste de Binding comme il le devrait
			writeln(separator);
			write("<a href='?part=jndi'>");
			write("<img src='?resource=jndi.png' width='20' height='20' alt=\"#Arbre_JNDI#\" /> #Arbre_JNDI#</a>");
		}

		if (isDatabaseEnabled()) {
			writeln(separator);
			write("<a href='?part=connections'>");
			write("<img src='?resource=db.png' width='20' height='20' alt=\"#Connexions_jdbc_ouvertes#\" /> #Connexions_jdbc_ouvertes#</a>");

			writeln(separator);
			write("<a href='?part=database'>");
			writeln("<img src='?resource=db.png' width='20' height='20' alt=\"#database#\" /> #database#</a>");
		}

		writeln("<br/></div>");
	}

	private void writeApplicationsLinks() throws IOException {
		assert collectorServer != null;
		writeln("<div align='center'>");
		final Collection<String> applications = Parameters.getCollectorUrlsByApplications()
				.keySet();
		if (applications.size() > 1
				|| !collectorServer.getLastCollectExceptionsByApplication().isEmpty()) {
			if (applications.size() > 10) {
				writeln("<table summary='applications'><tr><td>");
				writeShowHideLink("chooseApplication", "#Choix_application#");
				if (Parameters.getCollectorApplicationsFile().canWrite()) {
					writeAddAndRemoveApplicationLinks(collector.getApplication(), getWriter());
				}
				writeln("<div id='chooseApplication' style='display: none;'><div>&nbsp;&nbsp;&nbsp;");
				writeApplicationsLinks(applications, "<br />&nbsp;&nbsp;&nbsp;");
				writeln("</div></div></td></tr></table>");
			} else {
				writeln("&nbsp;&nbsp;&nbsp;#Choix_application# :&nbsp;&nbsp;&nbsp;");
				writeApplicationsLinks(applications, "&nbsp;&nbsp;&nbsp;");
				if (Parameters.getCollectorApplicationsFile().canWrite()) {
					writeAddAndRemoveApplicationLinks(collector.getApplication(), getWriter());
				}
			}
		} else if (Parameters.getCollectorApplicationsFile().canWrite()) {
			writeAddAndRemoveApplicationLinks(collector.getApplication(), getWriter());
		}
		writeln(END_DIV);
	}

	private void writeApplicationsLinks(Collection<String> applications, String separator)
			throws IOException {
		final Map<String, Throwable> lastCollectExceptionsByApplication = collectorServer
				.getLastCollectExceptionsByApplication();
		for (final String application : applications) {
			final Throwable lastCollectException = lastCollectExceptionsByApplication
					.get(application);
			writeln("<a href='?application=" + application + "' class='tooltip'>");
			if (lastCollectException == null) {
				writeln("<img src='?resource=bullets/green.png' alt='#Application_disponible#'/>");
				writeln("<em style='text-align: left; font-size: 11px;'>");
				writeln("#Application_disponible#");
				writeln("</em>");
			} else {
				writeln("<img src='?resource=bullets/red.png' alt='#Application_indisponible#'/>");
				writeln("<em style='text-align: left; font-size: 11px;'>");
				writeln("#Application_indisponible#:<br/>");
				writeDirectly(htmlEncode(lastCollectException.toString()));
				writeDirectly("<br/>");
				for (final StackTraceElement stackTraceElement : lastCollectException
						.getStackTrace()) {
					writeDirectly(htmlEncode(stackTraceElement.toString()));
					writeDirectly("<br/>");
				}
				writeln("</em>");
			}
			writeln(application + "</a>");
			writeln(separator);
		}
	}

	void writeRefreshAndPeriodLinks(String graphName, String part) throws IOException {
		writeln("<div class='noPrint'>");
		final String separator = "&nbsp;&nbsp;&nbsp;&nbsp;";
		final String graphParameter = "&amp;graph=";
		if (graphName == null) {
			write("<a href='?' title='#Rafraichir#'>");
		} else {
			write("<a href='javascript:history.back()'><img src='?resource=action_back.png' alt='#Retour#'/> #Retour#</a>");
			writeln(separator);
			writeln("<a href='?'><img src='?resource=action_home.png' alt='#Page_principale#'/> #Page_principale#</a>");
			writeln(separator);
			write("<a href='?part=" + part + graphParameter + urlEncode(graphName)
					+ "' title='#Rafraichir#'>");
		}
		write("<img src='?resource=action_refresh.png' alt='#Actualiser#'/> #Actualiser#</a>");
		if (graphName == null && isPdfEnabled()) {
			writeln(separator);
			write("<a href='?format=pdf' title='#afficher_PDF#'>");
			write("<img src='?resource=pdf.png' alt='#PDF#'/> #PDF#</a>");
		}
		writeln(separator);
		write("<a href='?resource=#help_url#' target='_blank'");
		write(" title=\"#Afficher_aide_en_ligne#\"><img src='?resource=action_help.png' alt='#Aide_en_ligne#'/> #Aide_en_ligne#</a>");
		writeln(separator);
		write("<a href='?part=jnlp'");
		write(" title=\"#RDA#\"><img src='?resource=systemmonitor.png' width='16' height='16' alt='#RDA#'/> #Desktop#</a>");
		writeln(separator);
		writeln("#Choix_periode# :&nbsp;");
		// On affiche des liens vers les périodes.
		// Rq : il n'y a pas de période ni de graph sur la dernière heure puisque
		// si la résolution des données est de 5 min, on ne verra alors presque rien
		for (final Period myPeriod : Period.values()) {
			if (graphName == null) {
				write("<a href='?period=" + myPeriod.getCode() + "' ");
			} else {
				write("<a href='?part=" + part + graphParameter + urlEncode(graphName)
						+ "&amp;period=" + myPeriod.getCode() + "' ");
			}
			write("title='" + getFormattedString("Choisir_periode", myPeriod.getLinkLabel())
					+ "'>");
			write("<img src='?resource=" + myPeriod.getIconName() + "' alt='"
					+ myPeriod.getLinkLabel() + "' /> ");
			writeln(myPeriod.getLinkLabel() + "</a>&nbsp;");
		}
		new HtmlForms(getWriter()).writeCustomPeriodLink(range, graphName, part);

		writeln(END_DIV);
	}

	private void writeDurationAndOverhead() throws IOException {
		final long displayDuration = System.currentTimeMillis() - start;
		writeln("<a name='bottom'></a>");
		writeln("<br/><div style='font-size: 11px;'>");
		writeln("#temps_derniere_collecte#: " + collector.getLastCollectDuration() + " #ms#<br/>");
		writeln("#temps_affichage#: " + displayDuration + " #ms#<br/>");
		writeln("#Estimation_overhead_memoire#: < "
				+ (collector.getEstimatedMemorySize() / 1024 / 1024 + 1) + " #Mo#");
		writeln("<br/>#Usage_disque#: " + (collector.getDiskUsage() / 1024 / 1024 + 1) + " #Mo#");
		if (Parameters.isSystemActionsEnabled()) {
			writeln("&nbsp;&nbsp;&nbsp;<a href='?action=purge_obsolete_files"
					+ getCsrfTokenUrlPart() + "' class='noPrint'>");
			writeln("<img width='14' height='14' src='?resource=user-trash.png' alt='#Purger_les_fichiers_obsoletes#' title='#Purger_les_fichiers_obsoletes#'/></a>");
		}
		if (Parameters.JAVAMELODY_VERSION != null) {
			writeln("<br/><br/>JavaMelody " + Parameters.JAVAMELODY_VERSION);
		}
		if (collectorServer == null) {
			writeln("<br/>");
			writeShowHideLink("debuggingLogs", "Debugging logs");
			writeln("<br/><br/>");
			writeln("<div id='debuggingLogs' style='display: none;'>");
			final List<String> debuggingLogs = LOG.getDebuggingLogs();
			if (debuggingLogs.size() >= LOG.MAX_DEBUGGING_LOGS_COUNT) {
				writeln("<div class='severe'>Only the last " + LOG.MAX_DEBUGGING_LOGS_COUNT
						+ " messages are displayed</div>");
			}
			for (final String msg : debuggingLogs) {
				writeDirectly(htmlEncodeButNotSpace(msg).replaceAll("[\t]",
						"&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"));
				writeln("<br/>");
			}
			writeln(END_DIV);
		}
		writeln(END_DIV);
	}
}
