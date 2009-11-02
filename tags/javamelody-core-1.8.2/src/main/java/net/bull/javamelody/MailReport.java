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

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;

/**
 * Génération de rapport pdf hebdomadaire et envoi par email aux administrateurs paramétrés.
 * @author Emeric Vernat
 */
class MailReport {
	static void scheduleReportMailForLocalServer(final Collector collector, final Timer timer) {
		assert collector != null;
		assert timer != null;
		final MailReport mailReport = new MailReport();
		final TimerTask task = new TimerTask() {
			/** {@inheritDoc} */
			@Override
			public void run() {
				try {
					// envoi du rapport
					mailReport.sendReportMailForLocalServer(collector);
				} catch (final Throwable t) { // NOPMD
					// pas d'erreur dans cette task
					Collector.printStackTrace(t);
				}
				// on reschedule à la même heure la semaine suivante sans utiliser de période de 24h*7
				// car certains jours font 23h ou 25h et on ne veut pas introduire de décalage
				scheduleReportMailForLocalServer(collector, timer);
			}
		};

		// schedule 1 fois la tâche
		timer.schedule(task, getNextExecutionDate());
	}

	static Date getNextExecutionDate() {
		// calcule de la date de prochaine exécution (le dimanche à minuit)
		final Calendar calendar = Calendar.getInstance();
		calendar.set(Calendar.DAY_OF_WEEK, Calendar.SUNDAY);
		calendar.set(Calendar.HOUR_OF_DAY, 0);
		calendar.set(Calendar.MINUTE, 0);
		calendar.set(Calendar.SECOND, 0);
		calendar.set(Calendar.MILLISECOND, 0);
		if (calendar.getTimeInMillis() < System.currentTimeMillis()) {
			// on utilise add et non roll pour ne pas tourner en boucle le 31/12
			calendar.add(Calendar.DAY_OF_YEAR, 7);
		}
		return calendar.getTime();
	}

	void sendReportMailForLocalServer(Collector collector) throws Exception { // NOPMD
		final JavaInformations javaInformations = new JavaInformations(Parameters
				.getServletContext(), true);
		sendReportMail(collector, false, Collections.singletonList(javaInformations));
	}

	void sendReportMail(Collector collector, boolean collectorServer,
			List<JavaInformations> javaInformationsList) throws Exception { // NOPMD
		final File tmpFile = new File(Parameters.TEMPORARY_DIRECTORY, PdfReport
				.getFileName(collector.getApplication()));
		try {
			final OutputStream output = new BufferedOutputStream(new FileOutputStream(tmpFile));
			try {
				final PdfReport pdfReport = new PdfReport(collector, collectorServer,
						javaInformationsList, Period.SEMAINE, output);
				pdfReport.toPdf();
			} finally {
				output.close();
			}

			final String subject = I18N.getString("Monitoring_sur") + ' '
					+ collector.getApplication();
			final Mailer mailer = new Mailer("java:comp/env/"
					+ Parameters.getParameter(Parameter.MAIL_SESSION));
			final String adminEmails = Parameters.getParameter(Parameter.ADMIN_EMAILS);
			mailer.send(adminEmails, subject, "", Collections.singletonList(tmpFile), false);
		} finally {
			if (!tmpFile.delete()) {
				tmpFile.deleteOnExit();
			}
		}
	}
}
