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
import java.text.DateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Partie du rapport html pour les jobs.
 * @author Emeric Vernat
 */
class HtmlJobInformationsReport {
	private static final long ONE_DAY_MILLIS = 24L * 60 * 60 * 1000;
	private final List<JobInformations> jobInformationsList;
	private final Map<String, CounterRequest> counterRequestsByRequestName;
	private final Writer writer;
	private final DateFormat fireTimeFormat = I18N.createDateAndTimeFormat();
	private final DateFormat durationFormat = I18N.createDurationFormat();
	private final boolean systemActionsEnabled = Parameters.isSystemActionsEnabled();

	HtmlJobInformationsReport(List<JobInformations> jobInformationsList, Counter rangeJobCounter,
			Writer writer) {
		super();
		assert jobInformationsList != null;
		assert rangeJobCounter != null;
		assert writer != null;

		this.jobInformationsList = jobInformationsList;
		this.writer = writer;
		final List<CounterRequest> counterRequests = rangeJobCounter.getRequests();
		this.counterRequestsByRequestName = new HashMap<String, CounterRequest>(counterRequests
				.size());
		for (final CounterRequest counterRequest : counterRequests) {
			counterRequestsByRequestName.put(counterRequest.getName(), counterRequest);
		}
	}

	void toHtml() throws IOException {
		writeln("<table class='sortable' width='100%' border='1' cellspacing='0' cellpadding='2' summary='#Jobs#'>");
		write("<thead><tr><th>#JobGroup#</th>");
		write("<th>#JobName#</th>");
		write("<th>#JobClassName#</th>");
		write("<th>#JobLastException#</th>");
		write("<th class='sorttable_date'>#JobMeanTime#</th>");
		write("<th class='sorttable_date'>#JobElapsedTime#</th>");
		write("<th class='sorttable_date'>#JobPreviousFireTime#</th>");
		write("<th class='sorttable_date'>#JobNextFireTime#</th>");
		write("<th>#JobPeriodOrCronExpression#</th>");
		write("<th>#JobPaused#</th>");
		if (systemActionsEnabled) {
			write("<th class='noPrint'>#Pause_job#</th>");
			write("<th class='noPrint'>#Resume_job#</th>");
		}
		writeln("</tr></thead><tbody>");
		boolean odd = false;
		for (final JobInformations jobInformations : jobInformationsList) {
			if (odd) {
				write("<tr class='odd' onmouseover=\"this.className='highlight'\" onmouseout=\"this.className='odd'\">");
			} else {
				write("<tr onmouseover=\"this.className='highlight'\" onmouseout=\"this.className=''\">");
			}
			odd = !odd; // NOPMD
			writeJobInformations(jobInformations);
			writeln("</tr>");
		}
		writeln("</tbody></table>");
		write("<div align='right' class='noPrint'>");
		if (systemActionsEnabled) {
			final String onClickConfirm = "' onclick=\"javascript:return confirm('";
			final String endOnClickConfirm = "');\">";
			writeln("<a href='?action=pause_job&amp;jobId=all" + onClickConfirm
					+ I18N.getStringForJavascript("confirm_pause_all_jobs") + endOnClickConfirm);
			writeln("<img src='?resource=control_pause_blue.png' width='18' height='18' alt=\"#Pause_all_jobs#\" /> #Pause_all_jobs#</a>");
			writeln("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
			writeln("<a href='?action=resume_job&amp;jobId=all" + onClickConfirm
					+ I18N.getStringForJavascript("confirm_resume_all_jobs") + endOnClickConfirm);
			writeln("<img src='?resource=control_play_blue.png' width='18' height='18' alt=\"#Resume_all_jobs#\" /> #Resume_all_jobs#</a>");
			writeln("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
		}

		// writer.write pour éviter traduction car # dans l'url
		writer.write("<a href='http://www.quartz-scheduler.org/docs/index.html'");
		writeln("target='_blank'>Configuration reference</a>");

		writeln("</div>");
	}

	private void writeJobInformations(JobInformations jobInformations) throws IOException {
		write("<td>");
		final String nextColumnAlignRight = "</td> <td align='right'>";
		writer.write(htmlEncode(jobInformations.getGroup()));
		write("</td> <td>");
		writeNameWithDescription(jobInformations);
		write("</td> <td>");
		writer.write(htmlEncode(jobInformations.getJobClassName()));

		final CounterRequest counterRequest = getCounterRequest(jobInformations);
		if (counterRequest != null) {
			write("</td> <td align='center'>");
			writeStackTrace(counterRequest.getStackTrace());
			write(nextColumnAlignRight);
			write(durationFormat.format(new Date(counterRequest.getMean())));
			// rq: on n'affiche pas le maximum, l'écart-type ou le pourcentage d'erreurs,
			// uniquement car cela ferait trop de colonnes dans la page
		} else {
			write("</td><td>&nbsp;</td><td>&nbsp;");
		}

		writeJobTimes(jobInformations, counterRequest);

		write("</td> <td align='center'>");
		if (jobInformations.isPaused()) {
			write("#oui#");
		} else {
			write("#non#");
		}
		if (systemActionsEnabled) {
			writePauseJobAndResumeJobLinks(jobInformations);
		}
		write("</td>");
	}

	private void writeNameWithDescription(JobInformations jobInformations) throws IOException {
		if (jobInformations.getDescription() == null) {
			writer.write(htmlEncode(jobInformations.getName()));
		} else {
			write("<a class='tooltip'><em>");
			writer.write(htmlEncode(jobInformations.getDescription()));
			writeln("</em>");
			writer.write(htmlEncode(jobInformations.getName()));
			writeln("</a>");
		}
	}

	private void writeStackTrace(String stackTrace) throws IOException {
		if (stackTrace == null) {
			write("<img src='?resource=bullets/green.png' alt='#JobWithoutLastException#' title='#JobWithoutLastException#'/>");
		} else {
			write("<a class='tooltip'>");
			write("<em>");
			writeln(I18N.htmlEncode(stackTrace.replace("[See nested", "\n[See nested"), true));
			writeln("</em>");
			write("<img src='?resource=bullets/red.png' alt=''/>");
			writeln("</a>");
		}
	}

	private void writeJobTimes(JobInformations jobInformations, CounterRequest counterRequest)
			throws IOException {
		final String nextColumnAlignRight = "</td> <td align='right'>";
		final String nbsp = "&nbsp;";
		write(nextColumnAlignRight);
		if (jobInformations.getElapsedTime() >= 0) {
			write(durationFormat.format(new Date(jobInformations.getElapsedTime())));
			if (counterRequest != null) {
				write("<br/>");
				writeln(toBar(counterRequest.getMean(), jobInformations.getElapsedTime()));
			}
		} else {
			write(nbsp);
		}
		write(nextColumnAlignRight);
		if (jobInformations.getPreviousFireTime() != null) {
			write(fireTimeFormat.format(jobInformations.getPreviousFireTime()));
		} else {
			write(nbsp);
		}
		write(nextColumnAlignRight);
		if (jobInformations.getNextFireTime() != null) {
			write(fireTimeFormat.format(jobInformations.getNextFireTime()));
		} else {
			write(nbsp);
		}
		write(nextColumnAlignRight);
		// on n'affiche pas la période si >= 1 jour car ce formateur ne saurait pas l'afficher
		if (jobInformations.getRepeatInterval() > 0
				&& jobInformations.getRepeatInterval() < ONE_DAY_MILLIS) {
			write(durationFormat.format(new Date(jobInformations.getRepeatInterval())));
		} else if (jobInformations.getCronExpression() != null) {
			write(jobInformations.getCronExpression());
		} else {
			write(nbsp);
		}
	}

	private void writePauseJobAndResumeJobLinks(JobInformations jobInformations) throws IOException {
		write("</td> <td align='center' class='noPrint'>");
		final String onClickConfirm = "' onclick=\"javascript:return confirm('";
		final String endOnClickConfirm = "');\">";
		writeln("<a href='?action=pause_job&amp;jobId=" + jobInformations.getGlobalJobId()
				+ onClickConfirm + I18N.getStringForJavascript("confirm_pause_job")
				+ endOnClickConfirm);
		writeln("<img src='?resource=control_pause_blue.png' width='18' height='18' alt=\"#Pause_job#\" title=\"#Pause_job#\" /></a>");
		write("</td> <td align='center' class='noPrint'>");
		writeln("<a href='?action=resume_job&amp;jobId=" + jobInformations.getGlobalJobId()
				+ onClickConfirm + I18N.getStringForJavascript("confirm_resume_job")
				+ endOnClickConfirm);
		writeln("<img src='?resource=control_play_blue.png' width='18' height='18' alt=\"#Resume_job#\" title=\"#Resume_job#\" /></a>");
	}

	private CounterRequest getCounterRequest(JobInformations jobInformations) {
		final String jobFullName = jobInformations.getGroup() + '.' + jobInformations.getName();
		return counterRequestsByRequestName.get(jobFullName);
	}

	private static String toBar(int mean, long elapsedTime) {
		return HtmlJavaInformationsReport.toBar(100d * elapsedTime / mean);
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
