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
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.List;

import net.bull.javamelody.internal.common.I18N;
import net.bull.javamelody.internal.common.Parameters;
import net.bull.javamelody.internal.model.ThreadInformations;

/**
 * Partie du rapport html pour les threads sur le serveur.
 * @author Emeric Vernat
 */
public class HtmlThreadInformationsReport extends HtmlAbstractReport {
	private final List<ThreadInformations> threadInformationsList;
	private final DecimalFormat integerFormat = I18N.createIntegerFormat();
	private final boolean stackTraceEnabled;
	private final boolean cpuTimeEnabled;
	private final boolean systemActionsEnabled = Parameters.isSystemActionsEnabled();

	public HtmlThreadInformationsReport(List<ThreadInformations> threadInformationsList,
			boolean stackTraceEnabled, Writer writer) {
		super(writer);
		assert threadInformationsList != null;

		this.threadInformationsList = threadInformationsList;
		this.stackTraceEnabled = stackTraceEnabled;
		this.cpuTimeEnabled = !threadInformationsList.isEmpty()
				&& threadInformationsList.get(0).getCpuTimeMillis() != -1;
	}

	@Override
	void toHtml() throws IOException {
		final HtmlTable table = new HtmlTable();
		table.beginTable(getString("Threads"));
		write("<th>#Thread#</th>");
		write("<th>#Demon#</th><th class='sorttable_numeric'>#Priorite#</th><th>#Etat#</th>");
		if (stackTraceEnabled) {
			write("<th>#Methode_executee#</th>");
		}
		if (cpuTimeEnabled) {
			write("<th class='sorttable_numeric'>#Temps_cpu#</th><th class='sorttable_numeric'>#Temps_user#</th>");
		}
		if (systemActionsEnabled) {
			writeln("<th class='noPrint'>#Interrupt#</th>");
			writeln("<th class='noPrint'>#Tuer#</th>");
		}
		for (final ThreadInformations threadInformations : threadInformationsList) {
			table.nextRow();
			writeThreadInformations(threadInformations);
		}
		table.endTable();
		writeln("<div align='right'>");
		writeln("#Temps_threads#");
		writeln("</div>");
	}

	void writeDeadlocks() throws IOException {
		final List<ThreadInformations> deadlockedThreads = getDeadLockedThreads();
		if (!deadlockedThreads.isEmpty()) {
			write("<div class='severe'>#Threads_deadlocks#");
			String separator = " ";
			for (final ThreadInformations thread : deadlockedThreads) {
				writeDirectly(separator);
				writeDirectly(htmlEncode(thread.getName()));
				separator = ", ";
			}
			write("</div>");
		}
	}

	public void writeThreadsDump() throws IOException {
		final List<ThreadInformations> deadlockedThreads = getDeadLockedThreads();
		if (!deadlockedThreads.isEmpty()) {
			write("#Threads_deadlocks#");
			String separator = " ";
			for (final ThreadInformations thread : deadlockedThreads) {
				writeDirectly(separator);
				writeDirectly(thread.getName());
				separator = ", ";
			}
			writeDirectly("\n\n");
		}
		if (stackTraceEnabled) {
			for (final ThreadInformations threadInformations : threadInformationsList) {
				writeDirectly("\"");
				writeDirectly(threadInformations.getName());
				writeDirectly("\"");
				if (threadInformations.isDaemon()) {
					writeDirectly(" daemon");
				}
				writeDirectly(" prio=");
				writeDirectly(String.valueOf(threadInformations.getPriority()));
				writeDirectly(" ");
				writeDirectly(String.valueOf(threadInformations.getState()));
				final List<StackTraceElement> stackTrace = threadInformations.getStackTrace();
				if (stackTrace != null && !stackTrace.isEmpty()) {
					for (final StackTraceElement element : stackTrace) {
						writeDirectly("\n\t");
						writeDirectly(element.toString());
					}
				}
				writeDirectly("\n\n");
			}
			writeDirectly("\n");
		}
	}

	private List<ThreadInformations> getDeadLockedThreads() {
		final List<ThreadInformations> deadlockedThreads = new ArrayList<ThreadInformations>();
		for (final ThreadInformations thread : threadInformationsList) {
			if (thread.isDeadlocked()) {
				deadlockedThreads.add(thread);
			}
		}
		return deadlockedThreads;
	}

	private void writeThreadInformations(ThreadInformations threadInformations) throws IOException {
		write("<td>");
		writeThreadWithStackTrace(threadInformations);
		write("</td> <td align='center'>");
		if (threadInformations.isDaemon()) {
			write("#oui#");
		} else {
			write("#non#");
		}
		write("</td> <td align='right'>");
		write(integerFormat.format(threadInformations.getPriority()));
		write("</td> <td>");
		write("<img src='?resource=bullets/");
		write(getStateIcon(threadInformations));
		write("' alt='");
		write(String.valueOf(threadInformations.getState()));
		write("'/>");
		write(String.valueOf(threadInformations.getState()));
		if (stackTraceEnabled) {
			write("</td> <td>");
			writeExecutedMethod(threadInformations);
		}
		if (cpuTimeEnabled) {
			write("</td> <td align='right'>");
			write(integerFormat.format(threadInformations.getCpuTimeMillis()));
			write("</td> <td align='right'>");
			write(integerFormat.format(threadInformations.getUserTimeMillis()));
		}
		writeSendThreadInterrupt(threadInformations);
		writeKillThread(threadInformations);
		write("</td>");
	}

	public static String getStateIcon(ThreadInformations threadInformations) {
		switch (threadInformations.getState()) {
		case RUNNABLE:
			return "green.png";
		case WAITING:
			return "yellow.png";
		case TIMED_WAITING:
			if (isSleeping(threadInformations)) {
				return "blue.png";
			}
			return "yellow.png";
		case BLOCKED:
			return "red.png";
		case NEW:
		case TERMINATED:
			return "gray.png";
		default:
			throw new IllegalArgumentException("state inconnu" + threadInformations.getState());
		}
	}

	private static boolean isSleeping(ThreadInformations threadInformations) {
		final List<StackTraceElement> stackTrace = threadInformations.getStackTrace();
		return stackTrace != null && !stackTrace.isEmpty()
				&& "sleep".equals(stackTrace.get(0).getMethodName())
				&& "java.lang.Thread".equals(stackTrace.get(0).getClassName());
	}

	void writeThreadWithStackTrace(ThreadInformations threadInformations) throws IOException {
		final List<StackTraceElement> stackTrace = threadInformations.getStackTrace();
		final String encodedName = htmlEncode(threadInformations.getName());
		if (stackTrace != null && !stackTrace.isEmpty()) {
			// même si stackTraceEnabled, ce thread n'a pas forcément de stack-trace
			writeln("<div class='tooltip'>");
			writeln("<em>");
			// writeDirectly pour ne pas gérer de traductions si le nom contient '#'
			writeDirectly(encodedName);
			writeln("<br/>");
			for (final StackTraceElement stackTraceElement : stackTrace) {
				writeDirectly(
						HtmlSourceReport.htmlEncodeStackTraceElement(stackTraceElement.toString()));
				writeDirectly("<br/>");
			}
			writeln("</em>");
			writeDirectly(encodedName);
			writeln("</div>");
		} else {
			// writeDirectly pour ne pas gérer de traductions si le nom contient '#'
			writeDirectly(encodedName);
		}
	}

	void writeExecutedMethod(ThreadInformations threadInformations) throws IOException {
		final String executedMethod = threadInformations.getExecutedMethod();
		if (executedMethod != null && !executedMethod.isEmpty()) {
			writeDirectly(HtmlSourceReport
					.htmlEncodeStackTraceElement(threadInformations.getExecutedMethod()));
		} else {
			writeDirectly("&nbsp;");
		}
	}

	void writeSendThreadInterrupt(ThreadInformations threadInformations) throws IOException {
		if (systemActionsEnabled) {
			write("</td> <td align='center' class='noPrint'>");
			write("<a href='?action=send_thread_interrupt&amp;threadId=");
			write(threadInformations.getGlobalThreadId());
			write(getCsrfTokenUrlPart());
			final String confirmSendThreadInterrupt = javascriptEncode(getFormattedString(
					"confirm_send_thread_interrupt", threadInformations.getName()));
			// writeDirectly pour ne pas gérer de traductions si le nom contient '#'
			writeDirectly("' onclick=\"javascript:return confirm('" + confirmSendThreadInterrupt
					+ "');\">");
			final String title = javascriptEncode(
					getFormattedString("send_thread_interrupt", threadInformations.getName()));
			writeDirectly("<img width='16' height='16' src='?resource=action_interrupt.png' alt='"
					+ title + "' title='" + title + "' />");
			write("</a>");
		}
	}

	void writeKillThread(ThreadInformations threadInformations) throws IOException {
		if (systemActionsEnabled) {
			write("</td> <td align='center' class='noPrint'>");
			write("<a href='?action=kill_thread&amp;threadId=");
			write(threadInformations.getGlobalThreadId());
			write(getCsrfTokenUrlPart());
			final String confirmKillThread = javascriptEncode(
					getFormattedString("confirm_kill_thread", threadInformations.getName()));
			// writeDirectly pour ne pas gérer de traductions si le nom contient '#'
			writeDirectly("' onclick=\"javascript:return confirm('" + confirmKillThread + "');\">");
			final String title = htmlEncode(
					getFormattedString("kill_thread", threadInformations.getName()));
			writeDirectly("<img width='16' height='16' src='?resource=stop.png' alt='" + title
					+ "' title='" + title + "' />");
			write("</a>");
		}
	}
}
