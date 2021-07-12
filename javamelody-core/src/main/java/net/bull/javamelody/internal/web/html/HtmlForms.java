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
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.Date;
import java.util.Locale;
import java.util.Map;

import net.bull.javamelody.internal.common.I18N;
import net.bull.javamelody.internal.model.Range;

/**
 * Formulaires html.
 * @author Emeric Vernat
 */
class HtmlForms extends HtmlAbstractReport {
	HtmlForms(Writer writer) {
		super(writer);
	}

	void writeCustomPeriodLinks(Map<String, Date> datesByWebappVersions, Range currentRange,
			String graphName, String part) throws IOException {
		writeln("<a class='showHide' data-show-hide-id='customPeriod' ");
		writeln("title='" + getFormattedString("Choisir_periode", getString("personnalisee"))
				+ "'>");
		writeln("<img src='?resource=calendar.png' alt='#personnalisee#' /> #personnalisee#</a>");

		if (!datesByWebappVersions.isEmpty()) {
			writeln("&nbsp;<a class='showHide' data-show-hide-id='deploymentPeriod' ");
			writeln("title='" + getFormattedString("Choisir_periode", getString("par_deploiement"))
					+ "'>");
			writeln("<img src='?resource=calendar.png' alt='#par_deploiement#' /> #par_deploiement#</a>");
		}

		writeCustomPeriodDiv(currentRange, graphName, part);
		if (!datesByWebappVersions.isEmpty()) {
			writeDeploymentPeriodDiv(datesByWebappVersions, currentRange, graphName, part);
		}
	}

	private void writeCustomPeriodDiv(Range currentRange, String graphName, String part)
			throws IOException {
		writeln("<div id='customPeriod' class='displayNone'>");
		writeln("<br/>");
		// yyyy-MM-dd is always the pattern of the input type=date
		final String pattern = "yyyy-MM-dd";
		final DateFormat dateFormat = new SimpleDateFormat(pattern, Locale.US);
		final String max = dateFormat.format(new Date());
		writeln("<form name='customPeriodForm' method='get' action=''>");
		writeln("<br/><b><label for='customPeriodStartDate'>#startDate#</label></b>&nbsp;&nbsp;");
		writeln("<input type='date' id='customPeriodStartDate' name='startDate' size='10' required max='"
				+ max + "' ");
		if (currentRange.getStartDate() != null) {
			writeln("value='" + dateFormat.format(currentRange.getStartDate()) + '\'');
		}
		writeln("/>&nbsp;&nbsp;<b><label for='customPeriodEndDate'>#endDate#</label></b>&nbsp;&nbsp;");
		writeln("<input type='date' id='customPeriodEndDate' name='endDate' size='10' required max='"
				+ max + "' ");
		if (currentRange.getEndDate() != null) {
			writeln("value='" + dateFormat.format(currentRange.getEndDate()) + '\'');
		}
		writeln("/>&nbsp;&nbsp;");
		final DateFormat localeDateFormat = I18N.createDateFormat();
		final String localeDateFormatPattern;
		if (getString("dateFormatPattern").isEmpty()) {
			localeDateFormatPattern = ((SimpleDateFormat) localeDateFormat).toPattern()
					.toLowerCase(I18N.getCurrentLocale());
		} else {
			localeDateFormatPattern = getString("dateFormatPattern");
		}
		// ce customPeriodPattern ne sera pas affiché si html5
		writeDirectly("<span id='customPeriodPattern' class='displayNone'>("
				+ localeDateFormatPattern + ")</span>");
		writeln("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<input type='submit' value='#ok#'/><br/><br/>");
		writeln("<input type='hidden' name='period' value=''/>");
		writeln("<input type='hidden' name='pattern' value='" + pattern + "'/>");
		if (graphName != null) {
			writeln("<input type='hidden' name='part' value='" + part + "'/>");
			writeln("<input type='hidden' name='graph' value='" + urlEncode(graphName) + "'/>");
		}
		writeln("</form><br/>");
		writeln("</div>");
	}

	private void writeDeploymentPeriodDiv(Map<String, Date> datesByWebappVersions,
			Range currentRange, String graphName, String part) throws IOException {
		writeln("<div id='deploymentPeriod' class='displayNone'>");
		writeln("<br/>");
		final DateFormat dateFormat = I18N.createDateFormat();
		final String currentRangeValue = currentRange.getValue();
		final String startDateLabel = I18N.getString("startDate")
				.toLowerCase(I18N.getCurrentLocale());
		final String endDateLabel = I18N.getString("endDate");
		writeln("<form name='deploymentPeriodForm' method='get' action=''>");
		writeln("<br/><b>#Version#</b>&nbsp;&nbsp;");
		writeln("<select name='period' class='selectDeploymentPeriod'>");
		writeDirectly("<option>&nbsp;</option>");
		// on doit retrier les versions ici, notamment s'il y en a une ajoutée à la fin
		String previousDate = null;
		for (final Map.Entry<String, Date> entry : datesByWebappVersions.entrySet()) {
			final String version = entry.getKey();
			final String date = dateFormat.format(entry.getValue());
			final String label;
			if (previousDate == null) {
				previousDate = dateFormat.format(new Date());
				label = version + ' ' + startDateLabel + ' ' + date;
			} else {
				label = version + ' ' + startDateLabel + ' ' + date + ' ' + endDateLabel + ' '
						+ previousDate;
			}
			final String rangeValue = date + Range.CUSTOM_PERIOD_SEPARATOR + previousDate;
			writeDirectly("<option value='" + rangeValue + "'");
			if (rangeValue.equals(currentRangeValue)) {
				writeDirectly(" selected='selected'");
			}
			writeDirectly(">");
			writeDirectly(htmlEncodeButNotSpace(label));
			writeDirectly("</option>");
			previousDate = date;
		}
		writeln("</select><br/><br/>");
		if (graphName != null) {
			writeln("<input type='hidden' name='part' value='" + part + "'/>");
			writeln("<input type='hidden' name='graph' value='" + urlEncode(graphName) + "'/>");
		}
		writeln("</form><br/>");
		writeln("</div>");
	}

	void writeAddAndRemoveApplicationLinks(String currentApplication,
			Collection<String> applications) throws IOException {
		if (currentApplication == null) {
			writeln("<div align='center'><h3>#add_application#</h3>");
			writeln("#collect_server_intro#");
		} else {
			final String separator = "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;";
			writeln(separator);
			writeln("<a href='' class='showHide noPrint' data-show-hide-id='addApplication'>");
			writeln("<img src='?resource=action_add.png' alt='#add_application#'/> #add_application#</a>");
			writeln(separator);
			if (applications.size() > 1) {
				writeln("<a href='' class='showHide noPrint' data-show-hide-id='addAggregation'>");
				writeln("<img src='?resource=action_add.png' alt='#add_aggregation#'/> #add_aggregation#</a>");
				writeln(separator);
			}
			writeln("<a href='?action=remove_application&amp;application=" + currentApplication
					+ getCsrfTokenUrlPart() + "' class='confirm noPrint' ");
			final String messageConfirmation = getFormattedString("confirm_remove_application",
					currentApplication);
			writeln("data-confirm='" + htmlEncodeButNotSpaceAndNewLine(messageConfirmation) + "'>");
			final String removeApplicationLabel = getFormattedString("remove_application",
					currentApplication);
			writeln("<img src='?resource=action_delete.png' alt=\"" + removeApplicationLabel
					+ "\"/> " + removeApplicationLabel + "</a>");
			writeln("<div id='addApplication' class='displayNone'>");
		}
		writeln("<br/> <br/>");
		writeln("<form name='appForm' method='post' action=''>");
		writeln("<br/><b><label for='appName'>#app_name_to_monitor#</label> :</b>&nbsp;&nbsp;");
		writeln("<input type='text' size='15' id='appName' name='appName' required />&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
		writeln("<b><label for='appUrls'>#app_urls#</label> :</b>&nbsp;&nbsp;");
		writeln("<input type='text' size='50' id='appUrls' name='appUrls' required />&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
		writeln("<input type='submit' value='#add#'/><br/>");
		writeln("#urls_sample# : <i>http://myhost/myapp/</i> #or# <i>http://host1/myapp/,http://host2/myapp/</i>");
		writeln("<br/> <br/>");
		writeln("</form>");
		writeln("</div>\n");

		if (applications.size() > 1) {
			writeln("<div id='addAggregation' class='displayNone'>");
			writeln("<br/> <br/>");
			writeln("<form name='aggregationForm' method='post' action=''>");
			writeln("<br/><b><label for='appName'>#aggregation_name_to_monitor#</label> :</b>&nbsp;&nbsp;");
			writeln("<input type='text' size='15' id='appName' name='appName' required />&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
			writeln("<br/><b>#aggregated_apps# :</b>");
			writeln("<table summary=''>");
			for (final String application : applications) {
				writeln("<tr><td>");
				writeln("<input type='checkbox' name='aggregatedApps' value='" + application
						+ "' /> <label for='aggregatedApps'>" + application + "</label>");
				writeln("</td></tr>");
			}
			writeln("</table>");
			writeln("<br/>");
			writeln("<input type='submit' value='#add#'/><br/>");
			writeln("<br/>");
			writeln("</form>\n");
			writeln("</div>\n");
		}
	}

	@Override
	void toHtml() {
		throw new UnsupportedOperationException();
	}
}
