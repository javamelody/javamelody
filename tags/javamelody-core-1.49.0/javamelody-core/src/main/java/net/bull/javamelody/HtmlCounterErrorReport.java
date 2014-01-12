/*
 * Copyright 2008-2012 by Emeric Vernat
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
import java.util.List;

/**
 * Partie du rapport html pour les erreurs http et dans les logs.
 * @author Emeric Vernat
 */
class HtmlCounterErrorReport extends HtmlAbstractReport {
	private final Counter counter;
	private final DateFormat dateTimeFormat = DateFormat.getDateTimeInstance(DateFormat.SHORT,
			DateFormat.MEDIUM, I18N.getCurrentLocale());

	HtmlCounterErrorReport(Counter counter, Writer writer) {
		super(writer);
		assert counter != null;
		assert counter.isErrorCounter();

		this.counter = counter;
	}

	@Override
	void toHtml() throws IOException {
		final List<CounterError> errors = counter.getErrors();
		if (errors.isEmpty()) {
			writeln("#Aucune_erreur#");
		} else {
			writeErrors(errors);
		}
	}

	private void writeErrors(List<CounterError> errors) throws IOException {
		assert errors != null;
		final boolean displayUser = shouldDisplayUser(errors);
		final boolean displayHttpRequest = shouldDisplayHttpRequest(errors);
		if (errors.size() >= Counter.MAX_ERRORS_COUNT) {
			write("<div class='severe' align='left'>");
			writeln(getFormattedString("Dernieres_erreurs_seulement", Counter.MAX_ERRORS_COUNT));
			write("</div>");
		}
		final String tableName = counter.getName();
		final HtmlTable table = new HtmlTable();
		table.beginTable(tableName);
		write("<th class='sorttable_date'>#Date#</th>");
		if (displayHttpRequest) {
			write("<th>#Requete#</th>");
		}
		if (displayUser) {
			write("<th>#Utilisateur#</th>");
		}
		write("<th>#Erreur#</th>");
		for (final CounterError error : errors) {
			table.nextRow();
			writeError(error, displayUser, displayHttpRequest);
		}
		table.endTable();
	}

	private void writeError(CounterError error, boolean displayUser, boolean displayHttpRequest)
			throws IOException {
		write("<td align='right'>");
		write(dateTimeFormat.format(error.getDate()));
		write("</td><td>");
		if (displayHttpRequest) {
			if (error.getHttpRequest() == null) {
				write("&nbsp;");
			} else {
				writeDirectly(htmlEncode(error.getHttpRequest()));
			}
			write("</td><td>");
		}
		if (displayUser) {
			if (error.getRemoteUser() == null) {
				write("&nbsp;");
			} else {
				writeDirectly(htmlEncode(error.getRemoteUser()));
			}
			write("</td><td>");
		}
		if (error.getStackTrace() != null) {
			writeln("<a class='tooltip'>");
			writeln("<em>");
			// writeDirectly pour ne pas gérer de traductions si la stack-trace contient '#'
			writeDirectly(htmlEncode(error.getStackTrace()));
			writeln("</em>");
			// writeDirectly pour ne pas gérer de traductions si le message contient '#'
			writeDirectly(htmlEncode(error.getMessage()));
			writeln("</a>");
		} else {
			// writeDirectly pour ne pas gérer de traductions si le message contient '#'
			writeDirectly(htmlEncode(error.getMessage()));
		}
		write("</td>");
	}

	static boolean shouldDisplayUser(List<CounterError> errors) {
		for (final CounterError error : errors) {
			if (error.getRemoteUser() != null) {
				return true;
			}
		}
		return false;
	}

	static boolean shouldDisplayHttpRequest(List<CounterError> errors) {
		for (final CounterError error : errors) {
			if (error.getHttpRequest() != null) {
				return true;
			}
		}
		return false;
	}
}
