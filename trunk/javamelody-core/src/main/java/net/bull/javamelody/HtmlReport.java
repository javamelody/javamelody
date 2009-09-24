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
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import net.bull.javamelody.HtmlCounterReport.HtmlCounterRequestGraphReport;

/**
 * Rapport html.
 * @author Emeric Vernat
 */
class HtmlReport {
	private static final String SCRIPT_BEGIN = "<script type='text/javascript'>";
	private static final String SCRIPT_END = "</script>";
	private static final boolean PDF_ENABLED = isPdfEnabled();

	private final Collector collector;
	private final List<JavaInformations> javaInformationsList;
	private final Period period;
	private final Writer writer;
	private final boolean collectorServer;

	private static class HtmlAddAndRemoveApplications {
		private final Writer writer;

		HtmlAddAndRemoveApplications(Writer writer) {
			super();
			this.writer = writer;
		}

		void writeAddAndRemoveApplicationLinks(String currentApplication, Period period)
				throws IOException {
			if (currentApplication == null) {
				writeln("<div align='center'><h3>#add_application#</h3>");
			} else {
				final String separator = "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;";
				writeln(separator);
				writeln("<a href=\"javascript:showHide('addApplication');document.appForm.appName.focus();\"");
				writeln(" class='noPrint'><img src='?resource=action_add.png' alt='#add_application#'/> #add_application#</a>");
				writeln(separator);
				writeln("<a href='?action=remove_application&amp;application=" + currentApplication
						+ "&amp;period=" + period.getCode() + "' class='noPrint' ");
				final String messageConfirmation = I18N.getFormattedString(
						"confirm_remove_application", currentApplication);
				writeln("onclick=\"javascript:return confirm('"
						+ I18N.javascriptEncode(messageConfirmation) + "');\">");
				final String removeApplicationLabel = I18N.getFormattedString("remove_application",
						currentApplication);
				writeln("<img src='?resource=action_delete.png' alt=\"" + removeApplicationLabel
						+ "\"/> " + removeApplicationLabel + "</a>");
				writeln("<div id='addApplication' style='display: none;'>");
			}
			writeln(SCRIPT_BEGIN);
			writeln("function validateAppForm() {");
			writeln("   if (document.appForm.appName.value.length == 0) {");
			writeln("      alert('" + I18N.getStringForJavascript("app_name_mandatory") + "');");
			writeln("      document.appForm.appName.focus();");
			writeln("      return false;");
			writeln("   }");
			writeln("   if (document.appForm.appUrls.value.length == 0) {");
			writeln("      alert('" + I18N.getStringForJavascript("app_urls_mandatory") + "');");
			writeln("      document.appForm.appUrls.focus();");
			writeln("      return false;");
			writeln("   }");
			writeln("   return true;");
			writeln("}");
			writeln(SCRIPT_END);
			writeln("<br/> <br/>");
			writeln("<form name='appForm' method='post' action='?period=" + period.getCode()
					+ "' onsubmit='return validateAppForm();'>");
			writeln("<br/><b>#app_name_to_monitor# :</b>&nbsp;&nbsp;<input type='text' size='15' name='appName'/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
			writeln("<b>#app_urls# :</b>&nbsp;&nbsp;<input type='text' size='50' name='appUrls'/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
			writeln("<input type='submit' value='#add#'/><br/>");
			writeln("#urls_sample# : <i>http://myhost/myapp/</i> #or# <i>http://host1/myapp/,http://host2/myapp/</i>");
			writeln("<br/> <br/>");
			writeln("</form>");
			writeln("</div>\n");
		}

		private void writeln(String html) throws IOException {
			I18N.writelnTo(html, writer);
		}
	}

	HtmlReport(Collector collector, boolean collectorServer,
			List<JavaInformations> javaInformationsList, Period period, Writer writer) {
		super();
		assert collector != null;
		assert javaInformationsList != null && !javaInformationsList.isEmpty();
		assert period != null;
		assert writer != null;

		this.collector = collector;
		this.collectorServer = collectorServer;
		this.javaInformationsList = javaInformationsList;
		this.period = period;
		this.writer = writer;
	}

	private static boolean isPdfEnabled() {
		try {
			Class.forName("com.lowagie.text.Document");
			return true;
		} catch (final ClassNotFoundException e) {
			return false;
		}
	}

	void toHtml(String message) throws IOException {
		final long start = System.currentTimeMillis();
		writeHtmlHeader(false);
		if (collectorServer) {
			writeln("<div align='center'>");
			writeApplicationsLinks(buildPeriodParameter());
			writeAddAndRemoveApplicationLinks(collector.getApplication(), period, writer);
			writeln("</div>\n");
		}

		writeln("<h3><img width='24' height='24' src='?resource=systemmonitor.png' alt='#Stats#'/>");
		writeln(I18N.getFormattedString("Statistiques", I18N.getCurrentDateAndTime(), I18N
				.createDateAndTimeFormat().format(collector.getCounters().get(0).getStartDate()),
				collector.getApplication()));
		if (javaInformationsList.get(0).getContextDisplayName() != null) {
			writeln(" (" + javaInformationsList.get(0).getContextDisplayName() + ')');
		}
		writeln("</h3>");
		writeln("<div align='center'>");
		writeRefreshAndPeriodLinks(null);
		writeGraphs();
		writeln("</div>");

		final Map<String, HtmlCounterReport> counterReportsByCounterName = writeCounters();
		if (period == Period.TOUT) {
			writeln("<div align='right'>");
			writeln("<a href='?period=tout&amp;action=clear_counter&amp;counter=all' title='#Vider_toutes_stats#'");
			writeln("class='noPrint' onclick=\"javascript:return confirm('"
					+ I18N.javascriptEncode(I18N.getString("confirm_vider_toutes_stats"))
					+ "');\">#Reinitialiser_toutes_stats#</a>");
			writeln("</div>");
		}

		if (!collectorServer) {
			writeln("<h3><img width='24' height='24' src='?resource=hourglass.png' alt='#Requetes_en_cours#'/>#Requetes_en_cours#</h3>");
			// si on n'est pas sur le serveur de collecte il n'y a qu'un javaInformations
			writeCurrentRequests(javaInformationsList.get(0), counterReportsByCounterName);
		}

		writeln("<h3><img width='24' height='24' src='?resource=systeminfo.png' alt='#Informations_systemes#'/>");
		writeln("#Informations_systemes#</h3>");
		if (collectorServer) {
			writeln("<div align='center' class='noPrint'><a href='?part=currentRequests"
					+ buildPeriodParameter() + "'>");
			writeln("<img src='?resource=hourglass.png' width='20' height='20' alt=\"#Voir_requetes_en_cours#\" /> #Voir_requetes_en_cours#</a>");
			writeln("</div><br/>");
		}
		if (Boolean.parseBoolean(Parameters.getParameter(Parameter.SYSTEM_ACTIONS_ENABLED))) {
			writeSystemActionsLinks();
		}

		new HtmlJavaInformationsReport(javaInformationsList, period, writer).toHtml();

		writeln("<h3 style='clear:both;'><img width='24' height='24' src='?resource=threads.png' alt='#Threads#'/>");
		writeln("#Threads#</h3>");
		writeThreads();

		if (isCacheEnabled()) {
			writeln("<h3><img width='24' height='24' src='?resource=caches.png' alt='#Caches#'/>");
			writeln("#Caches#</h3>");
			writeCaches();
		}

		writeMessageIfNotNull(message, null);
		writePoweredBy();
		final long displayDuration = System.currentTimeMillis() - start;
		writeln("<div style='font-size:10pt;'>#temps_derniere_collecte#: "
				+ collector.getLastCollectDuration() + " #ms#<br/>#temps_affichage#: "
				+ displayDuration + " #ms#</div>");

		writeHtmlFooter();
	}

	private Map<String, HtmlCounterReport> writeCounters() throws IOException {
		final Map<String, HtmlCounterReport> counterReportsByCounterName = new HashMap<String, HtmlCounterReport>();
		for (final Counter counter : collector.getPeriodCountersToBeDisplayed(period)) {
			writeln("<h3><img width='24' height='24' src='?resource=" + counter.getIconName()
					+ "' alt='" + counter.getName() + "'/>");
			final String counterLabel = I18N.getString(counter.getName() + "Label");
			writeln(I18N.getFormattedString("Statistiques_compteur", counterLabel));
			writeln(" - " + period.getLabel() + "</h3>");
			final HtmlCounterReport htmlCounterReport = new HtmlCounterReport(counter, period,
					writer);
			htmlCounterReport.toHtml();
			counterReportsByCounterName.put(counter.getName(), htmlCounterReport);
		}
		return counterReportsByCounterName;
	}

	static void writeAddAndRemoveApplicationLinks(String currentApplication, Period period,
			Writer writer) throws IOException {
		new HtmlAddAndRemoveApplications(writer).writeAddAndRemoveApplicationLinks(
				currentApplication, period);
	}

	void writeMessageIfNotNull(String message, String partToRedirectTo) throws IOException {
		if (message != null) {
			writeln(SCRIPT_BEGIN);
			writeln("alert(\"" + I18N.javascriptEncode(message) + "\");");
			// redirect vers une url évitant que F5 du navigateur ne refasse l'action au lieu de faire un refresh
			if (partToRedirectTo == null) {
				writeln("location.href = '?period=" + period.getCode() + '\'');
			} else {
				writeln("location.href = '?part=" + partToRedirectTo + "&period="
						+ period.getCode() + '\'');
			}
			writeln(SCRIPT_END);
		}
	}

	private String buildPeriodParameter() {
		return "&amp;period=" + period.getCode();
	}

	private void writeGraphs() throws IOException {
		final String periodParameter = buildPeriodParameter();
		for (final JRobin jrobin : collector.getCounterJRobins()) {
			// les jrobin de compteurs (qui commencent par le jrobin xxxHitsRate)
			// doivent être sur une même ligne donc on met un <br/> si c'est le premier
			final String jrobinName = jrobin.getName();
			if (jrobinName.endsWith("HitsRate")) {
				writeln("<br/>");
			}
			if (isJRobinDisplayed(jrobinName)) {
				writeln("<a href='?part=graph&amp;graph=" + jrobinName + periodParameter
						+ "'><img class='synthese' src='?width=200&amp;height="
						+ JRobin.SMALL_HEIGHT + "&amp;graph=" + jrobinName + periodParameter
						+ "' alt=\"" + jrobin.getLabel() + "\" title=\"" + jrobin.getLabel()
						+ "\"/></a>");
			}
			if ("httpSessions".equals(jrobinName) || "fileDescriptors".equals(jrobinName)) {
				// un <br/> après httpSessions et avant activeThreads pour l'alignement
				writeln("<br/>");
			}
		}
	}

	private boolean isJRobinDisplayed(String jrobinName) {
		// inutile car on ne génère pas les jrobin pour le counter de ce nom là
		//		if (jrobinName.startsWith(Counter.ERROR_COUNTER_NAME)) {
		//			return false;
		//		}
		for (final Counter counter : collector.getCounters()) {
			if (jrobinName.startsWith(counter.getName())) {
				return counter.isDisplayed();
			}
		}
		return true;
	}

	private void writeCurrentRequests(JavaInformations javaInformations,
			Map<String, HtmlCounterReport> counterReportsByCounterName) throws IOException {
		final List<ThreadInformations> threadInformationsList = javaInformations
				.getThreadInformationsList();
		final boolean stackTraceEnabled = javaInformations.isStackTraceEnabled();
		writeCurrentRequests(threadInformationsList, stackTraceEnabled, counterReportsByCounterName);
	}

	void writeCurrentRequests(List<ThreadInformations> threadInformationsList,
			boolean stackTraceEnabled, Map<String, HtmlCounterReport> counterReportsByCounterName)
			throws IOException {
		final List<CounterRequestContext> rootCurrentContexts = collector.getRootCurrentContexts();
		if (rootCurrentContexts.isEmpty()) {
			writeln("#Aucune_requete_en_cours#");
		} else {
			new HtmlCounterRequestContextReport(rootCurrentContexts, counterReportsByCounterName,
					threadInformationsList, stackTraceEnabled, period, writer).toHtml();
		}
	}

	private void writeThreads() throws IOException {
		int i = 0;
		for (final JavaInformations javaInformations : javaInformationsList) {
			writeln("<b>#Threads_sur# " + javaInformations.getHost() + ": </b>");
			writeln(I18N.getFormattedString("thread_count", javaInformations.getThreadCount(),
					javaInformations.getPeakThreadCount(), javaInformations
							.getTotalStartedThreadCount()));
			writeln("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
			final String id = "threads_" + i;
			writeShowHideLink(id, "#Details#");
			writeln("<br/><br/>");
			writeln("<div id='" + id + "' style='display: none;'>");
			new HtmlThreadInformationsReport(javaInformations.getThreadInformationsList(),
					javaInformations.isStackTraceEnabled(), writer).toHtml();
			// plus nécessaire, car il y a caches et logos après:
			//			if (JavaInformations.STACK_TRACES_ENABLED) {
			//				// pour que les tooltips des stack traces s'affichent dans le scroll
			//				writeln("<br/><br/><br/><br/><br/><br/><br/><br/>");
			//			}
			writeln("</div><br/>");
			i++;
		}
	}

	private boolean isCacheEnabled() {
		for (final JavaInformations javaInformations : javaInformationsList) {
			if (javaInformations.isCacheEnabled()) {
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
			writeln("<b>" + cacheInformationsList.size() + " #caches_sur# "
					+ javaInformations.getHost() + "</b>");
			writeln("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
			final String id = "caches_" + i;
			writeShowHideLink(id, "#Details#");
			writeln("<br/><br/>");
			writeln("<div id='" + id + "' style='display: none;'>");
			new HtmlCacheInformationsReport(javaInformations.getCacheInformationsList(), writer)
					.toHtml();
			writeln("</div><br/>");
			i++;
		}
	}

	private void writeSystemActionsLinks() throws IOException {
		final String periodParameter = buildPeriodParameter();
		writeln("<div align='center' class='noPrint'>");
		final String separator = "&nbsp;&nbsp;&nbsp;&nbsp;";
		if (Action.GC_ENABLED || collectorServer) {
			writeln("<a href='?action=gc" + periodParameter
					+ "' onclick=\"javascript:return confirm('"
					+ I18N.getStringForJavascript("confirm_ramasse_miette") + "');\">");
			writeln("<img src='?resource=broom.png' width='20' height='20' alt='#ramasse_miette#' /> #ramasse_miette#</a>");
			writeln(separator);
		} else {
			writeln("<a href='_' onclick=\"javascript:alert('"
					+ I18N.getStringForJavascript("ramasse_miette_desactive")
					+ "');return false;\">");
			writeln("<img src='?resource=broom.png' width='20' height='20' alt='#ramasse_miette#' /> #ramasse_miette#</a>");
			writeln(separator);
		}
		if (Action.HEAP_DUMP_ENABLED || collectorServer) {
			// si serveur de collecte, on suppose que si la version de java est la bonne
			// sur le serveur de collecte, ce sera la bonne aussi sur les serveurs
			// des webapps monitorées
			writeln("<a href='?action=heap_dump" + periodParameter
					+ "' onclick=\"javascript:return confirm('"
					+ I18N.getStringForJavascript("confirm_heap_dump") + "');\">");
			writeln("<img src='?resource=heapdump.png' width='20' height='20' alt=\"#heap_dump#\" /> #heap_dump#</a>");
			writeln(separator);
		}
		writeln("<a href='?action=invalidate_sessions" + periodParameter
				+ "' onclick=\"javascript:return confirm('"
				+ I18N.getStringForJavascript("confirm_invalidate_sessions") + "');\">");
		writeln("<img src='?resource=user-trash.png' width='18' height='18' alt=\"#invalidate_sessions#\" /> #invalidate_sessions#</a>");
		writeln("<br />");
		writeln(separator);
		writeln("<a href='?part=sessions" + periodParameter + "'>");
		writeln("<img src='?resource=system-users.png' width='20' height='20' alt=\"#sessions#\" /> #sessions#</a>");
		if (collectorServer || VirtualMachine.isEnabled()) {
			writeln(separator);
			writeln("<a href='?part=heaphisto" + periodParameter + "'>");
			writeln("<img src='?resource=memory.png' width='20' height='20' alt=\"#heaphisto#\" /> #heaphisto#</a>");
		}
		if (!javaInformationsList.isEmpty() && javaInformationsList.get(0).doesWebXmlExists()) {
			// on n'affiche le lien web.xml que si le fichier existe (pour api servlet 3.0 par ex)
			writeln(separator);
			writeln("<a href='?part=web.xml" + periodParameter + "'>");
			writeln("<img src='?resource=xml.png' width='20' height='20' alt=\"#web.xml#\" /> #web.xml#</a>");
		}

		writeln(separator);
		writeln("<a href='?part=processes" + periodParameter + "'>");
		writeln("<img src='?resource=threads.png' width='20' height='20' alt=\"#processes#\" /> #processes#</a>");

		writeln("<br/></div>");
	}

	private void writeApplicationsLinks(String periodParameter) throws IOException {
		final Set<String> applications = Parameters.getCollectorUrlsByApplications().keySet();
		if (applications.size() > 1) {
			writeln("&nbsp;&nbsp;&nbsp;#Choix_application# :&nbsp;&nbsp;&nbsp;");
			for (final String application : applications) {
				writeln("<a href='?application=" + application + periodParameter + "'>");
				writeln(application + "</a>&nbsp;&nbsp;&nbsp;");
			}
		}
	}

	private void writeRefreshAndPeriodLinks(String graphName) throws IOException {
		writeln("<div class='noPrint'>");
		final String start;
		final String separator = "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;";
		if (graphName == null) {
			start = "<a href='?period=";
		} else {
			writeln("<a href='javascript:history.back()'><img src='?resource=action_back.png' alt='#Retour#'/> #Retour#</a>");
			writeln(separator);
			start = "<a href='?part=graph&amp;graph=" + graphName + "&amp;period=";
		}
		writeln(start + period.getCode() + "' title='#Rafraichir#'>");
		writeln("<img src='?resource=action_refresh.png' alt='#Actualiser#'/> #Actualiser#</a>");
		if (graphName == null && PDF_ENABLED) {
			writeln(separator);
			writeln(start + period.getCode() + "&amp;format=pdf' title='#afficher_PDF#'>");
			writeln("<img src='?resource=pdf.png' alt='#PDF#'/> #PDF#</a>");
		}
		writeln(separator);
		writeln("<a href='?resource=#help_url#' target='_blank'");
		writeln(" title=\"#Afficher_aide_en_ligne#\"><img src='?resource=action_help.png' alt='#Aide_en_ligne#'/> #Aide_en_ligne#</a>");
		writeln(separator);
		writeln("#Choix_periode# :&nbsp;&nbsp;&nbsp;");
		// On affiche des liens vers les périodes.
		// Rq : il n'y a pas de période ni de graph sur la dernière heure puisque
		// si la résolution des données est de 5 min, on ne verra alors presque rien
		for (final Period myPeriod : Period.values()) {
			writeln(start + myPeriod.getCode() + "' ");
			writeln("title='" + I18N.getFormattedString("Choisir_periode", myPeriod.getLinkLabel())
					+ "'>");
			writeln("<img src='?resource=" + myPeriod.getIconName() + "' alt='"
					+ myPeriod.getLinkLabel() + "' /> ");
			writeln(myPeriod.getLinkLabel() + "</a>&nbsp;&nbsp;&nbsp;");
		}
		writeln("</div>");
	}

	void writeHtmlHeader(boolean includeSlider) throws IOException {
		writeln("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">");
		writeln("<html><head><title>#Monitoring_sur# " + collector.getApplication() + "</title>");
		writeln("<link rel='stylesheet' href='?resource=monitoring.css' type='text/css'/>");
		writeln("<script type='text/javascript' src='?resource=resizable_tables.js'></script>");
		writeln("<script type='text/javascript' src='?resource=sorttable.js'></script>");
		if (includeSlider) {
			writeln("<script type='text/javascript' src='?resource=prototype.js'></script>");
			writeln("<script type='text/javascript' src='?resource=slider.js'></script>");
		}
		writeJavaScript();
		writeln("</head><body>");
	}

	void writeHtmlFooter() throws IOException {
		final String analyticsId = Parameters.getParameter(Parameter.ANALYTICS_ID);
		if (analyticsId != null) {
			writer.write(SCRIPT_BEGIN);
			writer
					.write("var gaJsHost = (('https:' == document.location.protocol) ? 'https://ssl.' : 'http://www.');\n");
			writer
					.write("document.write(unescape(\"%3Cscript src='\" + gaJsHost + \"google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E\"));\n");
			writer.write(SCRIPT_END);
			writer.write(SCRIPT_BEGIN);
			writer.write(" try{\n");
			writer.write("var pageTracker = _gat._getTracker('" + analyticsId + "');\n");
			writer.write("pageTracker._trackPageview();\n");
			writer.write("} catch(err) {}\n");
			writer.write(SCRIPT_END);
			writer.write('\n');
		}
		writeln("</body></html>");
	}

	private void writeJavaScript() throws IOException {
		writeln(SCRIPT_BEGIN);
		writeln("function showHide(id){");
		writeln("  if (document.getElementById(id).style.display=='none') {");
		writeln("    if (document.getElementById(id + 'Img') != null) {");
		writeln("      document.getElementById(id + 'Img').src='?resource=bullets/minus.png';");
		writeln("    }");
		writeln("    document.getElementById(id).style.display='inline';");
		writeln("  } else {");
		writeln("    if (document.getElementById(id + 'Img') != null) {");
		writeln("      document.getElementById(id + 'Img').src='?resource=bullets/plus.png';");
		writeln("    }");
		writeln("    document.getElementById(id).style.display='none';");
		writeln("  }");
		writeln("}");
		writeln(SCRIPT_END);
	}

	private void writePoweredBy() throws IOException {
		writeln("");
		//		writeln("<div align='center'><font size='-1' face='Helvetica'>Powered by</font>&nbsp;&nbsp;&nbsp;");
		//		writeln("<a href='http://www.bull.com/fr/'>");
		//		writeln("<img src='?resource=logobull.png' alt='Bull' title='Bull' /></a>");
		//		writeln("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
		//		writeln("<a href='http://www.bull.com/fr/services/novaforge.php'>");
		//		writeln("<img src='?resource=Novaforge2.png' alt='Novaforge' title='Novaforge' /></a></div>");
	}

	void writeRequestAndGraphDetail(String graphName) throws IOException {
		writeHtmlHeader(true);

		writeln("<div align='center'>");
		writeRefreshAndPeriodLinks(graphName);
		writeln("</div>");

		new HtmlCounterRequestGraphReport(period, writer).writeRequestAndGraphDetail(collector,
				graphName);

		writeHtmlFooter();
	}

	void writeSessions(List<SessionInformations> sessionsInformations, String message,
			String sessionsPart) throws IOException {
		assert sessionsInformations != null;
		writeHtmlHeader(false);
		writeMessageIfNotNull(message, sessionsPart);
		new HtmlSessionInformationsReport(writer).toHtml(sessionsInformations);
		writeHtmlFooter();
	}

	void writeSessionDetail(String sessionId, SessionInformations sessionInformations)
			throws IOException {
		assert sessionId != null;
		writeHtmlHeader(false);
		new HtmlSessionInformationsReport(writer).writeSessionDetails(sessionId,
				sessionInformations);
		writeHtmlFooter();
	}

	void writeHeapHistogram(HeapHistogram heapHistogram, String message, String heapHistoPart)
			throws IOException {
		assert heapHistogram != null;
		writeHtmlHeader(false);
		writeMessageIfNotNull(message, heapHistoPart);
		new HtmlHeapHistogramReport(writer).toHtml(heapHistogram);
		writeHtmlFooter();
	}

	void writeProcesses(List<ProcessInformations> processInformationsList) throws IOException {
		assert processInformationsList != null;
		writeHtmlHeader(false);
		new HtmlProcessInformationsReport(processInformationsList, writer).toHtml();
		writeHtmlFooter();
	}

	private void writeShowHideLink(String idToShow, String label) throws IOException {
		writeln("<a href=\"javascript:showHide('" + idToShow + "');\" class='noPrint'><img id='"
				+ idToShow + "Img' src='?resource=bullets/plus.png' alt=''/> " + label + "</a>");
	}

	private void writeln(String html) throws IOException {
		I18N.writelnTo(html, writer);
	}
}
