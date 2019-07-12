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
import java.util.List;

import net.bull.javamelody.internal.common.I18N;
import net.bull.javamelody.internal.model.ProcessInformations;

/**
 * Partie du rapport html pour les processus du système d'exploitation.
 * @author Emeric Vernat
 */
public class HtmlProcessInformationsReport extends HtmlAbstractReport {
	private final List<ProcessInformations> processInformationsList;
	private final boolean windows;
	private final DecimalFormat percentFormat = I18N.createPercentFormat();
	private final DecimalFormat integerFormat = I18N.createIntegerFormat();

	HtmlProcessInformationsReport(List<ProcessInformations> processInformationsList,
			Writer writer) {
		super(writer);
		assert processInformationsList != null;

		this.processInformationsList = processInformationsList;
		this.windows = isWindowsProcessList(processInformationsList);
	}

	@Override
	void toHtml() throws IOException {
		writeLinks();
		writeln("<br/>");

		writeTitle("processes.png", getString("Processus"));
		writeTable();
	}

	void writeTable() throws IOException {
		final HtmlTable table = new HtmlTable();
		table.beginTable(getString("Processus"));
		write("<th>#Utilisateur#</th>");
		write("<th class='sorttable_numeric'>#PID#</th>");
		if (!windows) {
			write("<th class='sorttable_numeric'>#cpu#</th><th class='sorttable_numeric'>#mem#</th>");
		}
		write("<th class='sorttable_numeric'>#vsz#</th>");
		if (!windows) {
			write("<th class='sorttable_numeric'>#rss#</th><th>#tty#</th>");
			write("<th>#stat#</th><th>#start#</th>");
		}
		write("<th>#cpuTime#</th><th>#command#</th>");
		for (final ProcessInformations processInformations : processInformationsList) {
			table.nextRow();
			writeProcessInformations(processInformations);
		}
		table.endTable();
		if (!windows) {
			write("<div align='right'>");
			write("<a href='http://en.wikipedia.org/wiki/Ps_(Unix)' target='_blank'>ps command reference</a></div>");
		}
	}

	private void writeProcessInformations(ProcessInformations processInformations)
			throws IOException {
		write("<td>");
		write(htmlEncode(processInformations.getUser()));
		final String newColumnRight = "</td><td align='right'>";
		final String newColumn = "</td><td>";
		write(newColumnRight);
		write(integerFormat.format(processInformations.getPid()));
		if (!windows) {
			write(newColumnRight);
			write(percentFormat.format(processInformations.getCpuPercentage()));
			write(newColumnRight);
			write(percentFormat.format(processInformations.getMemPercentage()));
		}
		write(newColumnRight);
		write(integerFormat.format(processInformations.getVsz()));
		if (!windows) {
			write(newColumnRight);
			write(integerFormat.format(processInformations.getRss()));
			write(newColumn);
			write(htmlEncode(processInformations.getTty()));
			write(newColumn);
			write(htmlEncode(processInformations.getStat()));
			write(newColumnRight);
			write(htmlEncode(processInformations.getStart()));
		}
		write(newColumnRight);
		write(htmlEncode(processInformations.getCpuTime()));
		write(newColumn);
		writeDirectly(htmlEncode(processInformations.getCommand()));
		write("</td>");
	}

	void writeLinks() throws IOException {
		writeln("<div class='noPrint'>");
		writeln("<a href='javascript:history.back()'><img src='?resource=action_back.png' alt='#Retour#'/> #Retour#</a>");
		writeln("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
		writeln("<a href='?part=processes'><img src='?resource=action_refresh.png' alt='#Actualiser#'/> #Actualiser#</a>");
		if (isPdfEnabled()) {
			writeln("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
			write("<a href='?part=processes&amp;format=pdf' title='#afficher_PDF#'>");
			write("<img src='?resource=pdf.png' alt='#PDF#'/> #PDF#</a>");
		}
		writeln("</div>");
	}

	public static boolean isWindowsProcessList(List<ProcessInformations> processInformationsList) {
		// une liste de process est issue de windows et non linux si toutes les valeurs de tty sont nulles
		for (final ProcessInformations processInformations : processInformationsList) {
			if (processInformations.getTty() != null) {
				return false;
			}
		}
		return true;
	}
}
