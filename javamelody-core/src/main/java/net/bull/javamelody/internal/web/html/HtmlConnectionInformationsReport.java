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
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import net.bull.javamelody.internal.common.I18N;
import net.bull.javamelody.internal.model.ConnectionInformations;

/**
 * Partie du rapport html pour les connections jdbc ouvertes.
 * @author Emeric Vernat
 */
class HtmlConnectionInformationsReport extends HtmlAbstractReport {
	private final List<ConnectionInformations> connectionsInformations;
	private final DateFormat dateTimeFormat = I18N.createDateAndTimeFormat();
	private final Map<Long, Thread> threadsById;
	private final Map<Thread, StackTraceElement[]> stackTracesByThread;

	HtmlConnectionInformationsReport(List<ConnectionInformations> connectionsInformations,
			Writer writer) {
		super(writer);
		assert connectionsInformations != null;
		this.connectionsInformations = connectionsInformations;
		// rq: cette partie du rapport n'est pas exécutée sur le serveur de collecte
		// donc les threads sont ok
		this.stackTracesByThread = Thread.getAllStackTraces();
		this.threadsById = new HashMap<Long, Thread>(stackTracesByThread.size());
		for (final Thread thread : stackTracesByThread.keySet()) {
			this.threadsById.put(thread.getId(), thread);
		}
		// avant, si java < 1.6.0_01 :
		//		{
		//			this.stackTracesByThread = Collections.emptyMap();
		//			final List<Thread> threads = JavaInformations.getThreadsFromThreadGroups();
		//			this.threadsById = new HashMap<Long, Thread>(threads.size());
		//			for (final Thread thread : threads) {
		//				this.threadsById.put(thread.getId(), thread);
		//			}
		//		}
	}

	@Override
	void toHtml() throws IOException {
		writeBackAndRefreshLinks();
		writeln("<br/>");

		writeTitle("db.png", getString("Connexions_jdbc_ouvertes"));
		writeln("<br/>#connexions_intro#<br/><br/>");
		writeConnections();
	}

	void writeConnections() throws IOException {
		if (connectionsInformations.isEmpty()) {
			writeln("#Aucune_connexion_jdbc_ouverte#");
			return;
		}
		final HtmlTable table = new HtmlTable();
		table.beginTable(getString("Connexions_jdbc_ouvertes"));
		write("<th class='sorttable_date'>#Date_et_stack_trace_ouverture#</th>");
		write("<th>#Thread_et_stack_trace_actuelle#</th>");
		for (final ConnectionInformations connection : connectionsInformations) {
			table.nextRow();
			writeConnection(connection);
		}
		table.endTable();
		final int nbConnections = connectionsInformations.size();
		writeln("<div align='right'>" + getFormattedString("nb_connexions_ouvertes", nbConnections)
				+ "</div>");
	}

	private void writeBackAndRefreshLinks() throws IOException {
		writeln("<div class='noPrint'>");
		writeln("<a href='javascript:history.back()'>");
		writeln("<img src='?resource=action_back.png' alt='#Retour#'/> #Retour#</a>");
		writeln("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
		writeln("<a href='?part=connections'>");
		writeln("<img src='?resource=action_refresh.png' alt='#Actualiser#'/> #Actualiser#</a>");
		writeln("</div>");
	}

	private void writeConnection(ConnectionInformations connection) throws IOException {
		write("<td align='right'>");
		writeTextWithStackTrace(dateTimeFormat.format(connection.getOpeningDate()),
				connection.getOpeningStackTrace());
		write("</td><td>");
		final Thread thread = threadsById.get(connection.getThreadId());
		if (thread == null) {
			write("&nbsp;");
		} else {
			final StackTraceElement[] stackTrace = stackTracesByThread.get(thread);
			writeTextWithStackTrace(thread.getName(),
					stackTrace != null ? Arrays.asList(stackTrace) : null);
		}
		write("</td>");
	}

	private void writeTextWithStackTrace(String text, List<StackTraceElement> stackTrace)
			throws IOException {
		final String encodedText = htmlEncode(text);
		if (stackTrace != null && !stackTrace.isEmpty()) {
			// même si stackTraceEnabled, ce thread n'a pas forcément de stack-trace
			writeln("<div class='tooltip'>");
			writeln("<em>");
			// writeDirectly pour ne pas gérer de traductions si le texte contient '#'
			writeDirectly(encodedText);
			writeln("<br/>");
			for (final StackTraceElement stackTraceElement : stackTrace) {
				writeDirectly(
						HtmlSourceReport.htmlEncodeStackTraceElement(stackTraceElement.toString()));
				writeDirectly("<br/>");
			}
			writeln("</em>");
			writeDirectly(encodedText);
			writeln("</div>");
		} else {
			// writeDirectly pour ne pas gérer de traductions si le texte contient '#'
			writeDirectly(encodedText);
		}
	}
}
