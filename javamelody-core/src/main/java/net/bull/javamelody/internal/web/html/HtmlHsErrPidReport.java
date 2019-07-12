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
import net.bull.javamelody.internal.model.HsErrPid;

/**
 * Partie du rapport html pour les crashs de JVM.
 * @author Emeric Vernat
 */
public class HtmlHsErrPidReport extends HtmlAbstractReport {
	private final List<HsErrPid> hsErrPidList;
	private final DateFormat dateTimeFormat = I18N.createDateAndTimeFormat();

	HtmlHsErrPidReport(List<HsErrPid> hsErrPidList, Writer writer) {
		super(writer);
		assert hsErrPidList != null;

		this.hsErrPidList = hsErrPidList;
	}

	@Override
	void toHtml() throws IOException {
		writeLinks();
		writeln("<br/>");

		writeTitle("alert.png", getString("Crashes") + " (" + hsErrPidList.size() + ')');
		writeTable();
	}

	void writeTable() throws IOException {
		final HtmlTable table = new HtmlTable();
		table.beginTable(getString("Crashes"));
		write("<th>#File#</th>");
		write("<th class='sorttable_date'>#Date#</th>");
		for (final HsErrPid hsErrPid : hsErrPidList) {
			table.nextRow();
			writeHsErrPid(hsErrPid);
		}
		table.endTable();
	}

	private void writeHsErrPid(HsErrPid hsErrPid) throws IOException {
		write("<td><a href='?part=crashes&path=");
		// encode file in url but avoid encoding \
		write(urlEncode(hsErrPid.getFile().replace('\\', '/')));
		write("'>");
		write(htmlEncode(hsErrPid.getFile()));
		write("</a>");
		final String newColumnRight = "</td><td align='right'>";
		write(newColumnRight);
		write(dateTimeFormat.format(hsErrPid.getDate()));
		write("</td>");
	}

	void writeLinks() throws IOException {
		writeln("<div class='noPrint'>");
		writeln("<a href='javascript:history.back()'><img src='?resource=action_back.png' alt='#Retour#'/> #Retour#</a>");
		writeln("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
		writeln("<a href='?part=crashes'><img src='?resource=action_refresh.png' alt='#Actualiser#'/> #Actualiser#</a>");
		writeln("</div>");
	}
}
