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
package net.bull.javamelody.internal.web.pdf;

import java.io.IOException;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.lowagie.text.Chunk;
import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.Font;
import com.lowagie.text.Image;
import com.lowagie.text.Paragraph;
import com.lowagie.text.Phrase;
import com.lowagie.text.pdf.PdfPCell;

import net.bull.javamelody.internal.common.I18N;
import net.bull.javamelody.internal.model.JavaInformations;
import net.bull.javamelody.internal.model.ThreadInformations;
import net.bull.javamelody.internal.web.html.HtmlThreadInformationsReport;

/**
 * Partie du rapport pdf pour les threads sur le serveur.
 * @author Emeric Vernat
 */
class PdfThreadInformationsReport extends PdfAbstractTableReport {
	private final List<ThreadInformations> threadInformationsList;
	private final DecimalFormat integerFormat = I18N.createIntegerFormat();
	private final boolean stackTraceEnabled;
	private final boolean cpuTimeEnabled;
	private final Font cellFont = PdfFonts.TABLE_CELL.getFont();
	private final PdfDocumentFactory pdfDocumentFactory;

	PdfThreadInformationsReport(List<ThreadInformations> threadInformationsList,
			boolean stackTraceEnabled, PdfDocumentFactory pdfDocumentFactory, Document document) {
		super(document);
		assert threadInformationsList != null;
		assert pdfDocumentFactory != null;

		this.threadInformationsList = threadInformationsList;
		this.pdfDocumentFactory = pdfDocumentFactory;
		this.stackTraceEnabled = stackTraceEnabled;
		this.cpuTimeEnabled = !threadInformationsList.isEmpty()
				&& threadInformationsList.get(0).getCpuTimeMillis() != -1;
	}

	@Override
	void toPdf() throws DocumentException, IOException {
		writeHeader();

		for (final ThreadInformations threadInformations : threadInformationsList) {
			nextRow();
			writeThreadInformations(threadInformations);
		}
		addTableToDocument();

		final Paragraph tempsThreads = new Paragraph(getString("Temps_threads") + '\n', cellFont);
		tempsThreads.setAlignment(Element.ALIGN_RIGHT);
		addToDocument(tempsThreads);

		// rq stack-trace: on n'inclue pas dans le pdf les stack-traces des threads
		// car c'est très verbeux et cela remplirait des pages pour pas grand chose
		// d'autant que si le pdf est généré de nuit pour être envoyé par mail
		// alors ces stack-traces n'ont pas beaucoup d'intérêt
		//		if (stackTrace != null && !stackTrace.isEmpty()) {
		//			// même si stackTraceEnabled, ce thread n'a pas forcément de stack-trace
		//			writeln(threadInformations.getName());
		//			for (final StackTraceElement stackTraceElement : stackTrace) {
		//				writeln(stackTraceElement.toString());
		//			}
		//		}
	}

	void writeIntro(JavaInformations javaInformations) throws DocumentException {
		final Font boldFont = PdfFonts.BOLD.getFont();
		final Font normalFont = PdfFonts.NORMAL.getFont();
		addToDocument(new Phrase(
				getFormattedString("Threads_sur", javaInformations.getHost()) + ": ", boldFont));
		addToDocument(new Phrase(getFormattedString("thread_count",
				javaInformations.getThreadCount(), javaInformations.getPeakThreadCount(),
				javaInformations.getTotalStartedThreadCount()), normalFont));
	}

	void writeDeadlocks() throws DocumentException {
		final List<ThreadInformations> deadlockedThreads = new ArrayList<ThreadInformations>();
		for (final ThreadInformations thread : threadInformationsList) {
			if (thread.isDeadlocked()) {
				deadlockedThreads.add(thread);
			}
		}
		if (!deadlockedThreads.isEmpty()) {
			final StringBuilder sb = new StringBuilder();
			sb.append('\n');
			sb.append(getString("Threads_deadlocks"));
			String separator = " ";
			for (final ThreadInformations thread : deadlockedThreads) {
				sb.append(separator);
				sb.append(thread.getName());
				separator = ", ";
			}
			sb.append('\n');
			addToDocument(new Phrase(sb.toString(), PdfFonts.SEVERE_CELL.getFont()));
		}
	}

	private void writeHeader() throws DocumentException {
		final List<String> headers = createHeaders();
		final int[] relativeWidths = new int[headers.size()];
		Arrays.fill(relativeWidths, 0, headers.size(), 1);
		relativeWidths[0] = 3; // thread
		relativeWidths[3] = 2; // état
		if (stackTraceEnabled) {
			relativeWidths[4] = 6; // méthode exécutée
		}

		initTable(headers, relativeWidths);
	}

	private List<String> createHeaders() {
		final List<String> headers = new ArrayList<String>();
		headers.add(getString("Thread"));
		headers.add(getString("Demon"));
		headers.add(getString("Priorite"));
		headers.add(getString("Etat"));
		if (stackTraceEnabled) {
			headers.add(getString("Methode_executee"));
		}
		if (cpuTimeEnabled) {
			headers.add(getString("Temps_cpu"));
			headers.add(getString("Temps_user"));
		}
		return headers;
	}

	private void writeThreadInformations(ThreadInformations threadInformations)
			throws DocumentException, IOException {
		final PdfPCell defaultCell = getDefaultCell();
		defaultCell.setHorizontalAlignment(Element.ALIGN_LEFT);
		addCell(threadInformations.getName());
		defaultCell.setHorizontalAlignment(Element.ALIGN_CENTER);
		if (threadInformations.isDaemon()) {
			addCell(getString("oui"));
		} else {
			addCell(getString("non"));
		}
		defaultCell.setHorizontalAlignment(Element.ALIGN_RIGHT);
		addCell(integerFormat.format(threadInformations.getPriority()));
		defaultCell.setHorizontalAlignment(Element.ALIGN_LEFT);
		final PdfPCell cell = new PdfPCell();
		final Paragraph paragraph = new Paragraph(
				getDefaultCell().getLeading() + cellFont.getSize());
		paragraph.add(new Chunk(
				getImage(
						"bullets/" + HtmlThreadInformationsReport.getStateIcon(threadInformations)),
				0, -1));
		paragraph.add(new Phrase(String.valueOf(threadInformations.getState()), cellFont));
		cell.addElement(paragraph);
		addCell(cell);
		if (stackTraceEnabled) {
			addCell(threadInformations.getExecutedMethod());
		}
		if (cpuTimeEnabled) {
			defaultCell.setHorizontalAlignment(Element.ALIGN_RIGHT);
			addCell(integerFormat.format(threadInformations.getCpuTimeMillis()));
			addCell(integerFormat.format(threadInformations.getUserTimeMillis()));
		}
	}

	private Image getImage(String resourceFileName) throws DocumentException, IOException {
		return pdfDocumentFactory.getSmallImage(resourceFileName);
	}
}
