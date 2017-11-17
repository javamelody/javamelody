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
package net.bull.javamelody.internal.web.html;

import java.io.IOException;
import java.io.Writer;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
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
		writeln("<a href=\"javascript:showHide('customPeriod');document.customPeriodForm.startDate.focus();\" ");
		writeln("title='" + getFormattedString("Choisir_periode", getString("personnalisee"))
				+ "'>");
		writeln("<img src='?resource=calendar.png' alt='#personnalisee#' /> #personnalisee#</a>");

		if (!datesByWebappVersions.isEmpty()) {
			writeln("&nbsp;<a href=\"javascript:showHide('deploymentPeriod');\" ");
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
		writeln("<div id='customPeriod' style='display: none;'>");
		writeln("<script type='text/javascript'>");
		writeln("function validateCustomPeriodForm() {");
		writeln("   periodForm = document.customPeriodForm;");
		writelnCheckMandatory("periodForm.startDate", "dates_mandatory");
		writelnCheckMandatory("periodForm.endDate", "dates_mandatory");
		writeln("   periodForm.period.value=periodForm.startDate.value + '"
				+ Range.CUSTOM_PERIOD_SEPARATOR + "' + periodForm.endDate.value;");
		writeln("   return true;");
		writeln("}");
		writeln("</script>");
		writeln("<br/>");
		final DateFormat dateFormat = I18N.createDateFormat();
		final String dateFormatPattern;
		if (getString("dateFormatPattern").isEmpty()) {
			final String pattern = ((SimpleDateFormat) dateFormat).toPattern();
			dateFormatPattern = pattern.toLowerCase(I18N.getCurrentLocale());
		} else {
			dateFormatPattern = getString("dateFormatPattern");
		}
		writeln("<form name='customPeriodForm' method='get' action='' onsubmit='return validateCustomPeriodForm();'>");
		writeln("<br/><b>#startDate#</b>&nbsp;&nbsp;<input type='text' size='10' name='startDate' ");
		if (currentRange.getStartDate() != null) {
			writeln("value='" + dateFormat.format(currentRange.getStartDate()) + '\'');
		}
		writeln("/>&nbsp;&nbsp;<b>#endDate#</b>&nbsp;&nbsp;<input type='text' size='10' name='endDate' ");
		if (currentRange.getEndDate() != null) {
			writeln("value='" + dateFormat.format(currentRange.getEndDate()) + '\'');
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
		writeln("</div>");
	}

	private void writeDeploymentPeriodDiv(Map<String, Date> datesByWebappVersions,
			Range currentRange, String graphName, String part) throws IOException {
		writeln("<div id='deploymentPeriod' style='display: none;'>");
		writeln("<br/>");
		final DateFormat dateFormat = I18N.createDateFormat();
		final String currentRangeValue = currentRange.getValue();
		final String startDateLabel = I18N.getString("startDate")
				.toLowerCase(I18N.getCurrentLocale());
		final String endDateLabel = I18N.getString("endDate");
		writeln("<form name='deploymentPeriodForm' method='get' action=''>");
		writeln("<br/><b>#Version#</b>&nbsp;&nbsp;");
		writeln("<select name='period' onchange='document.deploymentPeriodForm.submit();'>");
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
			writeln("onclick=\"javascript:return confirm('" + javascriptEncode(messageConfirmation)
					+ "');\">");
			final String removeApplicationLabel = getFormattedString("remove_application",
					currentApplication);
			writeln("<img src='?resource=action_delete.png' alt=\"" + removeApplicationLabel
					+ "\"/> " + removeApplicationLabel + "</a>");
			writeln("<div id='addApplication' style='display: none;'>");
		}
		writeln("<script type='text/javascript'>");
		writeln("function validateAppForm() {");
		writelnCheckMandatory("document.appForm.appName", "app_name_mandatory");
		writelnCheckMandatory("document.appForm.appUrls", "app_urls_mandatory");
		writeln("   return true;");
		writeln("}");
		writeln("</script>");
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
