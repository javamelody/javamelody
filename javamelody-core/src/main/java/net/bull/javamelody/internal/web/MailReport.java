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
package net.bull.javamelody.internal.web;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;

import net.bull.javamelody.Parameter;
import net.bull.javamelody.internal.common.I18N;
import net.bull.javamelody.internal.common.LOG;
import net.bull.javamelody.internal.common.Mailer;
import net.bull.javamelody.internal.common.Parameters;
import net.bull.javamelody.internal.model.Collector;
import net.bull.javamelody.internal.model.JavaInformations;
import net.bull.javamelody.internal.model.Period;
import net.bull.javamelody.internal.web.pdf.PdfReport;

/**
 * Génération de rapport pdf hebdomadaire et envoi par email aux administrateurs paramétrés.
 * @author Emeric Vernat
 */
public class MailReport {
	public static void scheduleReportMailForLocalServer(Collector collector, Timer timer) {
		assert collector != null;
		assert timer != null;
		for (final Period period : getMailPeriods()) {
			scheduleReportMailForLocalServer(collector, timer, period);
		}
	}

	static void scheduleReportMailForLocalServer(final Collector collector, final Timer timer,
			final Period period) {
		assert collector != null;
		assert timer != null;
		assert period != null;
		final TimerTask task = new TimerTask() {
			/** {@inheritDoc} */
			@Override
			public void run() {
				try {
					// envoi du rapport
					new MailReport().sendReportMailForLocalServer(collector, period);
				} catch (final Throwable t) { // NOPMD
					// pas d'erreur dans cette task
					LOG.warn("sending mail report failed", t);
				}
				// si rapport à la semaine, on reschedule à la même heure la semaine suivante
				// sans utiliser de période de 24h*7 car certains jours font 23h ou 25h
				// et on ne veut pas introduire de décalage,
				// et idem pour jour suivant au lieu de 24h ou pour mois suivant
				scheduleReportMailForLocalServer(collector, timer, period);
			}
		};

		// schedule 1 fois la tâche
		final Date nextExecutionDate = getNextExecutionDate(period);
		timer.schedule(task, nextExecutionDate);
		LOG.debug("mail report for the " + period.getMailCode()
				+ " period scheduled with next execution date at " + nextExecutionDate);
	}

	public static List<Period> getMailPeriods() {
		final List<Period> mailPeriods;
		if (Parameter.MAIL_PERIODS.getValue() == null) {
			mailPeriods = Collections.singletonList(Period.SEMAINE);
		} else {
			final String mailPeriodsParameter = Parameter.MAIL_PERIODS.getValue();
			mailPeriods = new ArrayList<Period>();
			for (final String mailPeriod : mailPeriodsParameter.split(",")) {
				mailPeriods.add(Period.valueOfByMailCode(mailPeriod));
			}
		}
		return mailPeriods;
	}

	public static Date getNextExecutionDate(Period period) {
		// calcule de la date de prochaine exécution (le dimanche à minuit)
		final Calendar calendar = Calendar.getInstance();
		calendar.set(Calendar.HOUR_OF_DAY, 0);
		calendar.set(Calendar.MINUTE, 0);
		calendar.set(Calendar.SECOND, 0);
		calendar.set(Calendar.MILLISECOND, 0);
		switch (period) {
		case JOUR:
			calendar.add(Calendar.DAY_OF_YEAR, 1);
			break;
		case SEMAINE:
			calendar.set(Calendar.DAY_OF_WEEK, Calendar.SUNDAY);
			// pour le cas où on est déjà dimanche, alors on prend dimanche prochain
			if (calendar.getTimeInMillis() < System.currentTimeMillis()) {
				// on utilise add et non roll pour ne pas tourner en boucle le 31/12
				calendar.add(Calendar.DAY_OF_YEAR, 7);
			}
			break;
		case MOIS:
			calendar.set(Calendar.DAY_OF_MONTH, 1);
			// pour le cas où est on déjà le premier du mois, alors on prend le mois prochain
			if (calendar.getTimeInMillis() < System.currentTimeMillis()) {
				// on utilise add et non roll pour ne pas tourner en boucle le 31/12
				calendar.add(Calendar.MONTH, 1);
			}
			break;
		case ANNEE:
			throw new IllegalArgumentException(String.valueOf(period));
		case TOUT:
			throw new IllegalArgumentException(String.valueOf(period));
		default:
			throw new IllegalArgumentException(String.valueOf(period));
		}
		return calendar.getTime();
	}

	public void sendReportMailForLocalServer(Collector collector, Period period) throws Exception { // NOPMD
		final JavaInformations javaInformations = new JavaInformations(
				Parameters.getServletContext(), true);
		sendReportMail(collector, false, Collections.singletonList(javaInformations), period);
	}

	public void sendReportMail(Collector collector, boolean collectorServer,
			List<JavaInformations> javaInformationsList, Period period) throws Exception { // NOPMD
		final File tmpFile = new File(Parameters.TEMPORARY_DIRECTORY,
				PdfReport.getFileName(collector.getApplication()));
		try {
			final OutputStream output = new BufferedOutputStream(new FileOutputStream(tmpFile));
			try {
				final PdfReport pdfReport = new PdfReport(collector, collectorServer,
						javaInformationsList, period, output);
				pdfReport.toPdf();
			} finally {
				output.close();
			}

			final String subject;
			final String subjectPrefix = Parameter.MAIL_SUBJECT_PREFIX.getValue();
			if (subjectPrefix != null) {
				// échappement des quotes qui sont des caractères spéciaux pour MessageFormat
				subject = MessageFormat.format(subjectPrefix.replace("'", "''"),
						collector.getApplication()) + " - " + period.getLabel();
			} else {
				subject = I18N.getFormattedString("Monitoring_sur", collector.getApplication())
						+ " - " + period.getLabel();
			}

			final Mailer mailer = new Mailer(Parameter.MAIL_SESSION.getValue());
			final String adminEmails = Parameter.ADMIN_EMAILS.getValue();
			mailer.send(adminEmails, subject, "", Collections.singletonList(tmpFile), false);
		} finally {
			if (!tmpFile.delete()) {
				tmpFile.deleteOnExit();
			}
		}
	}
}
