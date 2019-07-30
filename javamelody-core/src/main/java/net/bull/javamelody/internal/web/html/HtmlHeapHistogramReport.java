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
import net.bull.javamelody.internal.model.Action;
import net.bull.javamelody.internal.model.HeapHistogram;
import net.bull.javamelody.internal.model.HeapHistogram.ClassInfo;

/**
 * Partie du rapport html pour l'histogramme mémoire.
 * @author Emeric Vernat
 */
class HtmlHeapHistogramReport extends HtmlAbstractReport {
	private final HeapHistogram heapHistogram;
	private final DecimalFormat integerFormat = I18N.createIntegerFormat();

	HtmlHeapHistogramReport(HeapHistogram heapHistogram, Writer writer) {
		super(writer);
		assert heapHistogram != null;
		this.heapHistogram = heapHistogram;
	}

	@Override
	void toHtml() throws IOException {
		writeLinks();
		writeln("<br/>");

		final String title = getFormattedString("heap_histo_du",
				I18N.createDateAndTimeFormat().format(heapHistogram.getTime()));
		writeTitle("memory.png", title);
		writeln("<br/><b>#Heap#</b>");
		final String separator = "&nbsp;&nbsp;&nbsp;";
		writeln(separator);
		final List<ClassInfo> heap = heapHistogram.getHeapHistogram();
		final long totalHeapInstances = heapHistogram.getTotalHeapInstances();
		final long totalHeapBytes = heapHistogram.getTotalHeapBytes();
		writeln("#Classes#: " + integerFormat.format(heap.size()) + ',');
		writeln(separator);
		writeln("#Instances#: " + integerFormat.format(totalHeapInstances) + ',');
		writeln(separator);
		writeln("#Kilo-Octets#: " + integerFormat.format(totalHeapBytes / 1024));
		writeClassInfoSummaryAndDetails(heap, totalHeapInstances, totalHeapBytes, true,
				heapHistogram.isSourceDisplayed());
		final List<ClassInfo> permGen = heapHistogram.getPermGenHistogram();
		if (!permGen.isEmpty()) {
			// avec jrockit ou java 8, permGen est vide
			writeln("<br/><br/><b>#PermGen#</b>");
			writeln(separator);
			final long totalPermGenInstances = heapHistogram.getTotalPermGenInstances();
			final long totalPermGenBytes = heapHistogram.getTotalPermGenBytes();
			writeln("#Classes#: " + integerFormat.format(permGen.size()) + ',');
			writeln(separator);
			writeln("#Instances#: " + integerFormat.format(totalPermGenInstances) + ',');
			writeln(separator);
			writeln("#Kilo-Octets#: " + integerFormat.format(totalPermGenBytes / 1024));
			writeClassInfoSummaryAndDetails(permGen, totalPermGenInstances, totalPermGenBytes,
					false, false);
		}
	}

	private void writeClassInfoSummaryAndDetails(List<ClassInfo> classHistogram,
			long totalInstances, long totalBytes, boolean heap, boolean sourceDisplayed)
			throws IOException {
		final List<ClassInfo> summaryClassHistogram = new ArrayList<ClassInfo>();
		for (final ClassInfo classInfo : classHistogram) {
			if (classInfo.getBytes() * 100 / totalBytes == 0) {
				break;
			}
			summaryClassHistogram.add(classInfo);
		}
		writeClassInfo(summaryClassHistogram, totalInstances, totalBytes, heap, sourceDisplayed);

		writeln("<div align='right'>");
		final String id;
		if (heap) {
			id = "detailsHeap";
		} else {
			id = "detailsPermGen";
		}
		writeShowHideLink(id, "#Details#");
		writeln("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
		writeln("</div><div id='" + id + "' style='display:none;'>");
		final List<ClassInfo> detailsClassHistogram = new ArrayList<ClassInfo>();
		for (final ClassInfo classInfo : classHistogram) {
			if (classInfo.getBytes() * 100 / totalBytes == 0) {
				detailsClassHistogram.add(classInfo);
			}
		}
		writeClassInfo(detailsClassHistogram, totalInstances, totalBytes, heap, sourceDisplayed);
		writeln("</div>");
	}

	private void writeClassInfo(List<ClassInfo> classHistogram, long totalInstances,
			long totalBytes, boolean heap, boolean sourceDisplayed) throws IOException {
		final HtmlTable table = new HtmlTable();
		table.beginTable(getString("histogramme"));
		write("<th>#Classe#</th><th>#Taille#</th><th>#pct_taille#</th>"
				+ "<th>#Instances#</th><th>#pct_instances#</th>"
				+ (sourceDisplayed ? "<th>#Source#</th>" : ""));
		for (final ClassInfo classInfo : classHistogram) {
			table.nextRow();
			writeClassInfoRow(classInfo, totalInstances, totalBytes, heap, sourceDisplayed);
		}
		table.endTable();
	}

	private void writeClassInfoRow(ClassInfo classInfo, long totalInstances, long totalBytes,
			boolean heap, boolean sourceDisplayed) throws IOException {
		writeDirectly("<td>");
		final String classInfoName = classInfo.getName();
		if (heap) {
			writeDirectly(HtmlSourceReport.addLinkToClassName(classInfoName));
		} else {
			// encodage nécessaire dans PermGen pour "<methodKlass>" par exemple
			writeDirectly(classInfoName.replaceAll("[<]", "&lt;").replaceAll("[>]", "&gt;"));
		}
		final String nextColumnAlignRight = "</td><td align='right'>";
		writeDirectly(nextColumnAlignRight);
		final long bytes = classInfo.getBytes();
		final long instancesCount = classInfo.getInstancesCount();
		writeDirectly(integerFormat.format(bytes / 1024));
		writeDirectly(nextColumnAlignRight);
		writeDirectly(integerFormat.format(bytes * 100 / totalBytes));
		writeDirectly(nextColumnAlignRight);
		writeDirectly(integerFormat.format(instancesCount));
		writeDirectly(nextColumnAlignRight);
		writeDirectly(integerFormat.format(instancesCount * 100 / totalInstances));
		writeDirectly("</td>");
		if (sourceDisplayed) {
			writeDirectly("<td>");
			final String source = classInfo.getSource();
			if (source == null) {
				writeDirectly("&nbsp;");
			} else {
				writeDirectly(source);
			}
			writeDirectly("</td>");
		}
	}

	private void writeLinks() throws IOException {
		writeln("<div class='noPrint'>");
		writeln("<a href='javascript:history.back()'><img src='?resource=action_back.png' alt='#Retour#'/> #Retour#</a>");
		final String separator = "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;";
		writeln(separator);
		writeln("<a href='?part=heaphisto'><img src='?resource=action_refresh.png' alt='#Actualiser#'/> #Actualiser#</a>");
		if (isPdfEnabled()) {
			writeln(separator);
			write("<a href='?part=heaphisto&amp;format=pdf' title='#afficher_PDF#'>");
			write("<img src='?resource=pdf.png' alt='#PDF#'/> #PDF#</a>");
		}
		writeln(separator);
		if (Action.GC_ENABLED) {
			writeln("<a href='?part=heaphisto&amp;action=gc" + getCsrfTokenUrlPart()
					+ "' onclick=\"javascript:return confirm('"
					+ getStringForJavascript("confirm_ramasse_miette") + "');\">");
			writeln("<img src='?resource=broom.png' width='16' height='16' alt='#ramasse_miette#' /> #ramasse_miette#</a>");
			writeln(separator);
		} else {
			writeln("<a href='?part=heaphisto&amp;action=gc" + getCsrfTokenUrlPart()
					+ "' onclick=\"javascript:alert('"
					+ getStringForJavascript("ramasse_miette_desactive") + "');return false;\">");
			writeln("<img src='?resource=broom.png' width='16' height='16' alt='#ramasse_miette#' /> #ramasse_miette#</a>");
			writeln(separator);
		}
		writeln("<a href='?part=heaphisto&amp;action=heap_dump" + getCsrfTokenUrlPart()
				+ "' onclick=\"javascript:return confirm('"
				+ getStringForJavascript("confirm_heap_dump") + "');\">");
		writeln("<img src='?resource=heapdump.png' width='16' height='16' alt='#heap_dump#' /> #heap_dump#</a>");
		writeln(separator);
		writeln("</div>");
	}
}
