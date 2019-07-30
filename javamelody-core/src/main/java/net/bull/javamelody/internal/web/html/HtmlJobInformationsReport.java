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
import java.util.Date;
import java.util.List;

import net.bull.javamelody.internal.common.I18N;
import net.bull.javamelody.internal.common.Parameters;
import net.bull.javamelody.internal.model.Counter;
import net.bull.javamelody.internal.model.CounterRequest;
import net.bull.javamelody.internal.model.JobInformations;

/**
 * Partie du rapport html pour les jobs.
 * @author Emeric Vernat
 */
class HtmlJobInformationsReport extends HtmlAbstractReport {
	private static final long ONE_DAY_MILLIS = 24L * 60 * 60 * 1000;
	private final List<JobInformations> jobInformationsList;
	private final Counter jobCounter;
	private final DateFormat fireTimeFormat = I18N.createDateAndTimeFormat();
	private final DateFormat durationFormat = I18N.createDurationFormat();
	private final boolean systemActionsEnabled = Parameters.isSystemActionsEnabled();

	HtmlJobInformationsReport(List<JobInformations> jobInformationsList, Counter rangeJobCounter,
			Writer writer) {
		super(writer);
		assert jobInformationsList != null;
		assert rangeJobCounter != null;

		this.jobInformationsList = jobInformationsList;
		this.jobCounter = rangeJobCounter;
	}

	@Override
	void toHtml() throws IOException {
		final HtmlTable table = new HtmlTable();
		table.beginTable(getString("Jobs"));
		write("<th>#JobGroup#</th>");
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
		for (final JobInformations jobInformations : jobInformationsList) {
			table.nextRow();
			writeJobInformations(jobInformations);
		}
		table.endTable();
		write("<div align='right' class='noPrint'>");
		if (systemActionsEnabled) {
			final String onClickConfirm = "' onclick=\"javascript:return confirm('";
			final String endOnClickConfirm = "');\">";
			writeln("<a href='?action=pause_job&amp;jobId=all" + getCsrfTokenUrlPart()
					+ onClickConfirm + getStringForJavascript("confirm_pause_all_jobs")
					+ endOnClickConfirm);
			writeln("<img src='?resource=control_pause_blue.png' width='18' height='18' alt=\"#Pause_all_jobs#\" /> #Pause_all_jobs#</a>");
			writeln("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
			writeln("<a href='?action=resume_job&amp;jobId=all" + getCsrfTokenUrlPart()
					+ onClickConfirm + getStringForJavascript("confirm_resume_all_jobs")
					+ endOnClickConfirm);
			writeln("<img src='?resource=control_play_blue.png' width='18' height='18' alt=\"#Resume_all_jobs#\" /> #Resume_all_jobs#</a>");
			writeln("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
		}

		// writeDirectly pour éviter traduction car # dans l'url
		writeDirectly("<a href='http://www.quartz-scheduler.org/docs/index.html'");
		writeln("target='_blank'>Configuration reference</a>");

		writeln("</div>");
	}

	private void writeJobInformations(JobInformations jobInformations) throws IOException {
		write("<td>");
		final String nextColumnAlignRight = "</td> <td align='right'>";
		writeDirectly(htmlEncodeButNotSpace(jobInformations.getGroup()));
		write("</td> <td>");
		writeNameWithDescription(jobInformations);
		write("</td> <td>");
		writeDirectly(htmlEncodeButNotSpace(jobInformations.getJobClassName()));

		final CounterRequest counterRequest = getCounterRequest(jobInformations);
		// counterRequest ne peut pas être null ici
		write("</td> <td align='center'>");
		writeStackTrace(counterRequest.getStackTrace());
		if (counterRequest.getMean() >= 0) {
			write(nextColumnAlignRight);
			write(formatDuration(counterRequest.getMean()));
		} else {
			write("</td><td>&nbsp;");
		}
		// rq: on n'affiche pas le maximum, l'écart-type ou le pourcentage d'erreurs,
		// uniquement car cela ferait trop de colonnes dans la page

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

	private String formatDuration(int durationAsMillis) {
		// int to long sans cast pour findbugs
		final long duration = 1L * durationAsMillis;
		return durationFormat.format(new Date(duration));
	}

	private void writeNameWithDescription(JobInformations jobInformations) throws IOException {
		if (jobInformations.getDescription() == null) {
			writeDirectly(htmlEncodeButNotSpace(jobInformations.getName()));
		} else {
			write("<a class='tooltip'><em>");
			writeDirectly(htmlEncodeButNotSpace(jobInformations.getDescription()));
			writeln("</em>");
			writeDirectly(htmlEncodeButNotSpace(jobInformations.getName()));
			writeln("</a>");
		}
	}

	private void writeStackTrace(String stackTrace) throws IOException {
		if (stackTrace == null) {
			write("<img src='?resource=bullets/green.png' alt='#JobWithoutLastException#' title='#JobWithoutLastException#'/>");
		} else {
			write("<a class='tooltip'>");
			write("<em>");
			// writeDirectly pour ne pas gérer de traductions si la stack-trace contient '#'
			writeDirectly(htmlEncode(stackTrace.replace("[See nested", "\n[See nested")));
			writeln("");
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
			write("<br/>");
			writeln(toBar(counterRequest.getMean(), jobInformations.getElapsedTime()));
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
			// writeDirectly pour ne pas gérer de traductions si l'expression contient '#'
			writeDirectly(htmlEncodeButNotSpace(jobInformations.getCronExpression()));
		} else {
			write(nbsp);
		}
	}

	private void writePauseJobAndResumeJobLinks(JobInformations jobInformations)
			throws IOException {
		write("</td> <td align='center' class='noPrint'>");
		final String onClickConfirm = "' onclick=\"javascript:return confirm('";
		final String endOnClickConfirm = "');\">";
		writeln("<a href='?action=pause_job&amp;jobId=" + jobInformations.getGlobalJobId()
				+ getCsrfTokenUrlPart() + onClickConfirm
				+ getStringForJavascript("confirm_pause_job") + endOnClickConfirm);
		writeln("<img src='?resource=control_pause_blue.png' width='18' height='18' alt=\"#Pause_job#\" title=\"#Pause_job#\" /></a>");
		write("</td> <td align='center' class='noPrint'>");
		writeln("<a href='?action=resume_job&amp;jobId=" + jobInformations.getGlobalJobId()
				+ getCsrfTokenUrlPart() + onClickConfirm
				+ getStringForJavascript("confirm_resume_job") + endOnClickConfirm);
		writeln("<img src='?resource=control_play_blue.png' width='18' height='18' alt=\"#Resume_job#\" title=\"#Resume_job#\" /></a>");
	}

	private CounterRequest getCounterRequest(JobInformations jobInformations) {
		final String jobFullName = jobInformations.getGroup() + '.' + jobInformations.getName();
		// rq: la méthode getCounterRequestByName prend en compte l'éventuelle utilisation du paramètre
		// job-transform-pattern qui peut faire que jobFullName != counterRequest.getName()
		final CounterRequest result = jobCounter.getCounterRequestByName(jobFullName, true);
		// getCounterRequestByName ne peut pas retourner null actuellement
		assert result != null;
		return result;
	}

	private static String toBar(int mean, long elapsedTime) {
		return HtmlJavaInformationsReport.toBar(100d * elapsedTime / mean);
	}
}
