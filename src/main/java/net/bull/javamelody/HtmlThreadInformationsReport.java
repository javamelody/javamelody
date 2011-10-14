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
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.List;

/**
 * Partie du rapport html pour les threads sur le serveur.
 * @author Emeric Vernat
 */
class HtmlThreadInformationsReport {
	private final List<ThreadInformations> threadInformationsList;
	private final Writer writer;
	private final DecimalFormat integerFormat = I18N.createIntegerFormat();
	private final boolean stackTraceEnabled;
	private final boolean cpuTimeEnabled;
	private final boolean systemActionsEnabled = Parameters.isSystemActionsEnabled();

	HtmlThreadInformationsReport(List<ThreadInformations> threadInformationsList,
			boolean stackTraceEnabled, Writer writer) {
		super();
		assert threadInformationsList != null;
		assert writer != null;

		this.threadInformationsList = threadInformationsList;
		this.writer = writer;
		this.stackTraceEnabled = stackTraceEnabled;
		this.cpuTimeEnabled = !threadInformationsList.isEmpty()
				&& threadInformationsList.get(0).getCpuTimeMillis() != -1;
	}

	void toHtml() throws IOException {
		writeln("<table class='sortable' width='100%' border='1' cellspacing='0' cellpadding='2' summary='#Threads#'>");
		write("<thead><tr><th>#Thread#</th>");
		write("<th>#Demon#</th><th class='sorttable_numeric'>#Priorite#</th><th>#Etat#</th>");
		if (stackTraceEnabled) {
			write("<th>#Methode_executee#</th>");
		}
		if (cpuTimeEnabled) {
			write("<th class='sorttable_numeric'>#Temps_cpu#</th><th class='sorttable_numeric'>#Temps_user#</th>");
		}
		if (systemActionsEnabled) {
			writeln("<th class='noPrint'>#Tuer#</th>");
		}
		writeln("</tr></thead><tbody>");
		boolean odd = false;
		for (final ThreadInformations threadInformations : threadInformationsList) {
			if (odd) {
				write("<tr class='odd' onmouseover=\"this.className='highlight'\" onmouseout=\"this.className='odd'\">");
			} else {
				write("<tr onmouseover=\"this.className='highlight'\" onmouseout=\"this.className=''\">");
			}
			odd = !odd; // NOPMD
			writeThreadInformations(threadInformations);
			writeln("</tr>");
		}
		writeln("</tbody></table>");
		writeln("<div align='right'>");
		writeln("#Temps_threads#");
		if (stackTraceEnabled) {
			writeln("<br/><a href='?part=threadsDump'><img src='?resource=text.png' alt='#Dump_threads_en_texte#'/>&nbsp;#Dump_threads_en_texte#</a>");
		}
		writeln("</div>");
	}

	void writeDeadlocks() throws IOException {
		final List<ThreadInformations> deadlockedThreads = new ArrayList<ThreadInformations>();
		for (final ThreadInformations thread : threadInformationsList) {
			if (thread.isDeadlocked()) {
				deadlockedThreads.add(thread);
			}
		}
		if (!deadlockedThreads.isEmpty()) {
			write("<div class='severe'>#Threads_deadlocks#");
			String separator = " ";
			for (final ThreadInformations thread : deadlockedThreads) {
				writer.write(separator);
				writer.write(htmlEncode(thread.getName()));
				separator = ", ";
			}
			write("</div>");
		}
	}

	void writeThreadsDump() throws IOException {
		if (stackTraceEnabled) {
			for (final ThreadInformations threadInformations : threadInformationsList) {
				writer.write('\"');
				writer.write(threadInformations.getName());
				writer.write('\"');
				if (threadInformations.isDaemon()) {
					writer.write(" daemon");
				}
				writer.write(" prio=");
				writer.write(String.valueOf(threadInformations.getPriority()));
				writer.write(' ');
				writer.write(String.valueOf(threadInformations.getState()));
				final List<StackTraceElement> stackTrace = threadInformations.getStackTrace();
				if (stackTrace != null && !stackTrace.isEmpty()) {
					for (final StackTraceElement element : stackTrace) {
						writer.write("\n\t");
						writer.write(element.toString());
					}
				}
				writer.write("\n\n");
			}
			writer.write("\n");
		}
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
		if (systemActionsEnabled) {
			write("</td> <td align='center' class='noPrint'>");
			write("<a href='?action=kill_thread&amp;threadId=");
			write(threadInformations.getGlobalThreadId());
			final String confirmKillThread = I18N.javascriptEncode(I18N.getFormattedString(
					"confirm_kill_thread", threadInformations.getName()));
			// writer.write pour ne pas gérer de traductions si le nom contient '#'
			writer.write("' onclick=\"javascript:return confirm('" + confirmKillThread + "');\">");
			final String title = htmlEncode(I18N.getFormattedString("kill_thread",
					threadInformations.getName()));
			writer.write("<img width='16' height='16' src='?resource=stop.png' alt='" + title
					+ "' title='" + title + "' />");
			write("</a>");
		}
		write("</td>");
	}

	static String getStateIcon(ThreadInformations threadInformations) {
		switch (threadInformations.getState()) { // NOPMD
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
			writeln("<a class='tooltip'>");
			writeln("<em>");
			// writer.write pour ne pas gérer de traductions si le nom contient '#'
			writer.write(encodedName);
			writeln("<br/>");
			for (final StackTraceElement stackTraceElement : stackTrace) {
				write(htmlEncode(stackTraceElement.toString()));
				writeln("<br/>");
			}
			writeln("</em>");
			writer.write(encodedName);
			writeln("</a>");
		} else {
			// writer.write pour ne pas gérer de traductions si le nom contient '#'
			writer.write(encodedName);
		}
	}

	void writeExecutedMethod(ThreadInformations threadInformations) throws IOException {
		final String executedMethod = threadInformations.getExecutedMethod();
		if (executedMethod != null && executedMethod.length() != 0) {
			write(htmlEncode(executedMethod));
		} else {
			write("&nbsp;");
		}
	}

	private static String htmlEncode(String text) {
		return I18N.htmlEncode(text, true);
	}

	private void write(String html) throws IOException {
		I18N.writeTo(html, writer);
	}

	private void writeln(String html) throws IOException {
		I18N.writelnTo(html, writer);
	}
}
