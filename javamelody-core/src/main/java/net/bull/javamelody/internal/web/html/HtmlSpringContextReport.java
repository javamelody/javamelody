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
import java.util.List;

import net.bull.javamelody.SpringContext;

/**
 * Rapport html pour lister les beans du contexte Spring.
 * @author Emeric Vernat
 */
public class HtmlSpringContextReport extends HtmlAbstractReport {
	private final SpringContext springContext;

	HtmlSpringContextReport(SpringContext springContext, Writer writer) {
		super(writer);
		assert springContext != null;
		this.springContext = springContext;
	}

	@Override
	void toHtml() throws IOException {
		writeBackLink();
		writeln("<br/>");

		final List<String> beanDefinitionNames = springContext.getBeanDefinitionNames();
		writeTitle("beans.png", getString("Spring_beans"));
		final HtmlTable table = new HtmlTable();
		table.beginTable(getString("Spring_beans"));
		write("<th>#Nom#</th><th>#Classe#</th><th>Bean</th>");
		for (final String beanName : beanDefinitionNames) {
			table.nextRow();
			final Object bean = springContext.getBean(beanName);
			final Class<?> beanClass = bean.getClass();
			String beanToString;
			try {
				beanToString = bean.toString();
			} catch (final Exception e) {
				beanToString = e.toString();
			}
			writeBean(beanName, beanClass, beanToString);
		}
		table.endTable();
		writeln("<div align='right'>" + getFormattedString("nb_beans", beanDefinitionNames.size())
				+ "</div>");
	}

	private void writeBackLink() throws IOException {
		writeln("<div class='noPrint'>");
		writeln("<a href='javascript:history.back()'>");
		writeln("<img src='?resource=action_back.png' alt='#Retour#'/> #Retour#</a>");
		writeln("</div>");
	}

	private void writeBean(String beanName, Class<?> beanClass, String beanToString)
			throws IOException {
		write("<td>");
		writeDirectly(htmlEncodeButNotSpace(beanName));
		write("</td><td>");
		writeDirectly(HtmlSourceReport.addLinkToClassName(beanClass.getName()));
		write("</td><td>");
		writeDirectly(htmlEncodeButNotSpace(beanToString));
		write("</td>");
	}
}
