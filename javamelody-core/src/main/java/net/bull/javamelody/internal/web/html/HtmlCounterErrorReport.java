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
import java.util.List;

import net.bull.javamelody.internal.common.I18N;
import net.bull.javamelody.internal.model.Counter;
import net.bull.javamelody.internal.model.CounterError;

/**
 * Partie du rapport html pour les erreurs http et dans les logs.
 * @author Emeric Vernat
 */
public class HtmlCounterErrorReport extends HtmlAbstractReport {
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
		if (displayHttpRequest) {
			write("</td><td class='wrappedText'>");
			if (error.getHttpRequest() == null) {
				write("&nbsp;");
			} else {
				writeDirectly(htmlEncode(error.getHttpRequest()));
			}
		}
		if (displayUser) {
			write("</td><td class='wrappedText'>");
			if (error.getRemoteUser() == null) {
				write("&nbsp;");
			} else {
				writeDirectly(htmlEncode(error.getRemoteUser()));
			}
		}
		if (error.getStackTrace() != null) {
			write("</td><td>"); // pas wrappedText ici, sinon bug de largeur du tooltip sous IE11 en résolution réduite
			writeln("<div class='tooltip'>");
			writeln("<em>");
			writeStackTrace(error);
			writeln("</em>");
			// writeDirectly pour ne pas gérer de traductions si le message contient '#'
			writeDirectly(htmlEncode(error.getMessage()));
			writeln("</div>");
		} else {
			write("</td><td class='wrappedText'>");
			// writeDirectly pour ne pas gérer de traductions si le message contient '#'
			writeDirectly(htmlEncode(error.getMessage()));
		}
		write("</td>");
	}

	private void writeStackTrace(CounterError error) throws IOException {
		for (final String element : error.getStackTrace().split("[\n\r]")) {
			if (!element.isEmpty()) {
				// writeDirectly pour ne pas gérer de traductions car les liens contiennent '#'
				writeDirectly(HtmlSourceReport.htmlEncodeStackTraceElementAndTabs(element));
				writeDirectly("<br/>\n");
			}
		}
	}

	public static boolean shouldDisplayUser(List<CounterError> errors) {
		for (final CounterError error : errors) {
			if (error.getRemoteUser() != null) {
				return true;
			}
		}
		return false;
	}

	public static boolean shouldDisplayHttpRequest(List<CounterError> errors) {
		for (final CounterError error : errors) {
			if (error.getHttpRequest() != null) {
				return true;
			}
		}
		return false;
	}
}
