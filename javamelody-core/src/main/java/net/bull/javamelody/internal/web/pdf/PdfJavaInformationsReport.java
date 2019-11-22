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
import java.util.List;

import com.lowagie.text.Anchor;
import com.lowagie.text.BadElementException;
import com.lowagie.text.Chunk;
import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.Font;
import com.lowagie.text.Image;
import com.lowagie.text.Phrase;
import com.lowagie.text.pdf.PdfPTable;

import net.bull.javamelody.internal.common.I18N;
import net.bull.javamelody.internal.common.Parameters;
import net.bull.javamelody.internal.model.JavaInformations;
import net.bull.javamelody.internal.model.MemoryInformations;
import net.bull.javamelody.internal.model.TomcatInformations;
import net.bull.javamelody.internal.web.html.HtmlJavaInformationsReport;

/**
 * Partie du rapport pdf pour les informations systèmes sur le serveur.
 * @author Emeric Vernat
 */
class PdfJavaInformationsReport extends PdfAbstractReport {
	private static final String DIVIDE = " / ";
	private static final String BAR_SEPARATOR = "   ";
	private final boolean noDatabase = Parameters.isNoDatabase();
	private final DecimalFormat decimalFormat = I18N.createPercentFormat();
	private final DecimalFormat integerFormat = I18N.createIntegerFormat();
	private final List<JavaInformations> javaInformationsList;
	private final Font cellFont = PdfFonts.TABLE_CELL.getFont();
	private final Font boldCellFont = PdfFonts.BOLD_CELL.getFont();
	private PdfPTable currentTable;

	PdfJavaInformationsReport(List<JavaInformations> javaInformationsList, Document document) {
		super(document);
		assert javaInformationsList != null;

		this.javaInformationsList = javaInformationsList;
	}

	@Override
	void toPdf() throws DocumentException, IOException {
		for (final JavaInformations javaInformations : javaInformationsList) {
			currentTable = createJavaInformationsTable();
			writeSummary(javaInformations);
			addCell("");
			addCell("");
			addToDocument(currentTable);
		}
	}

	private static PdfPTable createJavaInformationsTable() throws DocumentException {
		final PdfPTable table = new PdfPTable(2);
		table.setHorizontalAlignment(Element.ALIGN_LEFT);
		table.setWidthPercentage(100);
		table.setWidths(new int[] { 2, 8 });
		table.getDefaultCell().setBorder(0);
		return table;
	}

	private void writeSummary(JavaInformations javaInformations)
			throws BadElementException, IOException {
		addCell(getString("Host") + ':');
		currentTable.addCell(new Phrase(javaInformations.getHost(), boldCellFont));
		addCell(getString("memoire_utilisee") + ':');
		final MemoryInformations memoryInformations = javaInformations.getMemoryInformations();
		final long usedMemory = memoryInformations.getUsedMemory();
		final long maxMemory = memoryInformations.getMaxMemory();
		final Phrase memoryPhrase = new Phrase(integerFormat.format(usedMemory / 1024 / 1024) + ' '
				+ getString("Mo") + DIVIDE + integerFormat.format(maxMemory / 1024 / 1024) + ' '
				+ getString("Mo") + BAR_SEPARATOR, cellFont);
		final Image memoryImage = Image.getInstance(
				Bar.toBarWithAlert(memoryInformations.getUsedMemoryPercentage()), null);
		memoryImage.scalePercent(50);
		memoryPhrase.add(new Chunk(memoryImage, 0, 0));
		currentTable.addCell(memoryPhrase);
		if (javaInformations.getSessionCount() >= 0) {
			addCell(getString("nb_sessions_http") + ':');
			addCell(integerFormat.format(javaInformations.getSessionCount()));
		}
		addCell(getString("nb_threads_actifs") + "\n(" + getString("Requetes_http_en_cours")
				+ "):");
		addCell(integerFormat.format(javaInformations.getActiveThreadCount()));
		if (!noDatabase) {
			addCell(getString("nb_connexions_actives") + ':');
			addCell(integerFormat.format(javaInformations.getActiveConnectionCount()));
			addCell(getString("nb_connexions_utilisees") + "\n(" + getString("ouvertes") + "):");
			final int usedConnectionCount = javaInformations.getUsedConnectionCount();
			final int maxConnectionCount = javaInformations.getMaxConnectionCount();
			if (maxConnectionCount <= 0) {
				addCell(integerFormat.format(usedConnectionCount));
			} else {
				final Phrase usedConnectionCountPhrase = new Phrase(
						integerFormat.format(usedConnectionCount) + DIVIDE
								+ integerFormat.format(maxConnectionCount) + BAR_SEPARATOR,
						cellFont);
				final Image usedConnectionCountImage = Image.getInstance(
						Bar.toBarWithAlert(javaInformations.getUsedConnectionPercentage()), null);
				usedConnectionCountImage.scalePercent(50);
				usedConnectionCountPhrase.add(new Chunk(usedConnectionCountImage, 0, 0));
				currentTable.addCell(usedConnectionCountPhrase);
			}
		}
		if (javaInformations.getSystemLoadAverage() >= 0) {
			addCell(getString("Charge_systeme") + ':');
			addCell(decimalFormat.format(javaInformations.getSystemLoadAverage()));
		}
		if (javaInformations.getSystemCpuLoad() >= 0) {
			addCell(getString("systemCpuLoad") + ':');
			final Phrase systemCpuLoadPhrase = new Phrase(
					decimalFormat.format(javaInformations.getSystemCpuLoad()) + BAR_SEPARATOR,
					cellFont);
			final Image systemCpuLoadImage = Image
					.getInstance(Bar.toBarWithAlert(javaInformations.getSystemCpuLoad()), null);
			systemCpuLoadImage.scalePercent(50);
			systemCpuLoadPhrase.add(new Chunk(systemCpuLoadImage, 0, 0));
			currentTable.addCell(systemCpuLoadPhrase);
		}
	}

	void writeInformationsDetails() throws DocumentException, IOException {
		for (final JavaInformations javaInformations : javaInformationsList) {
			currentTable = createJavaInformationsTable();
			writeSummary(javaInformations);
			writeDetails(javaInformations);
			addToDocument(currentTable);
		}
	}

	private void writeDetails(JavaInformations javaInformations)
			throws BadElementException, IOException {
		addCell(getString("OS") + ':');
		final Phrase osPhrase = new Phrase("", cellFont);
		final String osIconName = HtmlJavaInformationsReport
				.getOSIconName(javaInformations.getOS());
		final String separator = "   ";
		if (osIconName != null) {
			final Image osImage = PdfDocumentFactory.getImage("servers/" + osIconName);
			osImage.scalePercent(40);
			osPhrase.add(new Chunk(osImage, 0, 0));
			osPhrase.add(new Chunk(separator));
		}
		osPhrase.add(new Chunk(javaInformations.getOS() + " ("
				+ javaInformations.getAvailableProcessors() + ' ' + getString("coeurs") + ')'));
		currentTable.addCell(osPhrase);
		addCell(getString("Java") + ':');
		addCell(javaInformations.getJavaVersion());
		addCell(getString("JVM") + ':');
		final Phrase jvmVersionPhrase = new Phrase(javaInformations.getJvmVersion(), cellFont);
		if (javaInformations.getJvmVersion().contains("Client")) {
			jvmVersionPhrase.add(new Chunk(separator));
			final Image alertImage = PdfDocumentFactory.getImage("alert.png");
			alertImage.scalePercent(50);
			jvmVersionPhrase.add(new Chunk(alertImage, 0, -2));
		}
		currentTable.addCell(jvmVersionPhrase);
		addCell(getString("PID") + ':');
		addCell(javaInformations.getPID());
		if (javaInformations.getUnixOpenFileDescriptorCount() >= 0) {
			writeFileDescriptorCounts(javaInformations);
		}
		final String serverInfo = javaInformations.getServerInfo();
		if (serverInfo != null) {
			writeServerInfo(serverInfo);
			addCell(getString("Contexte_webapp") + ':');
			addCell(javaInformations.getContextPath());
		}
		addCell(getString("Demarrage") + ':');
		addCell(I18N.createDateAndTimeFormat().format(javaInformations.getStartDate()));
		addCell(getString("Arguments_JVM") + ':');
		addCell(javaInformations.getJvmArguments());
		if (javaInformations.getSessionCount() >= 0) {
			addCell(getString("httpSessionsMeanAge") + ':');
			addCell(integerFormat.format(javaInformations.getSessionMeanAgeInMinutes()));
		}
		writeTomcatInformations(javaInformations.getTomcatInformationsList());
		addCell(getString("Gestion_memoire") + ':');
		writeMemoryInformations(javaInformations.getMemoryInformations());
		// on considère que l'espace libre sur le disque dur est celui sur la partition du répertoire temporaire
		addCell(getString("Free_disk_space") + ':');
		addCell(integerFormat.format(javaInformations.getFreeDiskSpaceInTemp() / 1024 / 1024) + ' '
				+ getString("Mo"));
		addCell(getString("Usable_disk_space") + ':');
		addCell(integerFormat.format(javaInformations.getUsableDiskSpaceInTemp() / 1024 / 1024)
				+ ' ' + getString("Mo"));
		writeDatabaseVersionAndDataSourceDetails(javaInformations);
		addCell("");
		addCell("");
	}

	private void writeServerInfo(String serverInfo) throws BadElementException, IOException {
		addCell(getString("Serveur") + ':');
		final Phrase serverInfoPhrase = new Phrase("", cellFont);
		final String applicationServerIconName = HtmlJavaInformationsReport
				.getApplicationServerIconName(serverInfo);
		if (applicationServerIconName != null) {
			final Image applicationServerImage = PdfDocumentFactory
					.getImage("servers/" + applicationServerIconName);
			applicationServerImage.scalePercent(40);
			serverInfoPhrase.add(new Chunk(applicationServerImage, 0, 0));
			serverInfoPhrase.add(new Chunk("   "));
		}
		serverInfoPhrase.add(new Chunk(serverInfo));
		currentTable.addCell(serverInfoPhrase);
	}

	private void writeFileDescriptorCounts(JavaInformations javaInformations)
			throws BadElementException, IOException {
		final long unixOpenFileDescriptorCount = javaInformations.getUnixOpenFileDescriptorCount();
		final long unixMaxFileDescriptorCount = javaInformations.getUnixMaxFileDescriptorCount();
		addCell(getString("nb_fichiers") + ':');
		final Phrase fileDescriptorCountPhrase = new Phrase(
				integerFormat.format(unixOpenFileDescriptorCount) + DIVIDE
						+ integerFormat.format(unixMaxFileDescriptorCount) + BAR_SEPARATOR,
				cellFont);
		final Image fileDescriptorCountImage = Image.getInstance(
				Bar.toBarWithAlert(javaInformations.getUnixOpenFileDescriptorPercentage()), null);
		fileDescriptorCountImage.scalePercent(50);
		fileDescriptorCountPhrase.add(new Chunk(fileDescriptorCountImage, 0, 0));
		currentTable.addCell(fileDescriptorCountPhrase);
	}

	private void writeDatabaseVersionAndDataSourceDetails(JavaInformations javaInformations) {
		if (!noDatabase && javaInformations.getDataBaseVersion() != null) {
			addCell(getString("Base_de_donnees") + ':');
			addCell(javaInformations.getDataBaseVersion());
		}
		if (javaInformations.getDataSourceDetails() != null) {
			addCell(getString("DataSource_jdbc") + ':');
			addCell(javaInformations.getDataSourceDetails());
			addCell("");
			final Anchor anchor = new Anchor("DataSource reference", PdfFonts.BLUE.getFont());
			anchor.setName("DataSource reference");
			anchor.setReference(
					"http://commons.apache.org/dbcp/apidocs/org/apache/commons/dbcp/BasicDataSource.html");
			currentTable.addCell(anchor);
		}
	}

	private void writeTomcatInformations(List<TomcatInformations> tomcatInformationsList)
			throws BadElementException, IOException {
		for (final TomcatInformations tomcatInformations : tomcatInformationsList) {
			if (tomcatInformations.getRequestCount() <= 0) {
				continue;
			}
			addCell("Tomcat " + tomcatInformations.getName() + ':');
			// rq: on n'affiche pas pour l'instant getCurrentThreadCount
			final int currentThreadsBusy = tomcatInformations.getCurrentThreadsBusy();
			final String equal = " = ";
			final Phrase phrase = new Phrase(getString("busyThreads") + equal
					+ integerFormat.format(currentThreadsBusy) + DIVIDE
					+ integerFormat.format(tomcatInformations.getMaxThreads()) + BAR_SEPARATOR,
					cellFont);
			final Image threadsImage = Image.getInstance(Bar.toBarWithAlert(
					100d * currentThreadsBusy / tomcatInformations.getMaxThreads()), null);
			threadsImage.scalePercent(50);
			phrase.add(new Chunk(threadsImage, 0, 0));

			phrase.add(new Chunk('\n' + getString("bytesReceived") + equal
					+ integerFormat.format(tomcatInformations.getBytesReceived()) + '\n'
					+ getString("bytesSent") + equal
					+ integerFormat.format(tomcatInformations.getBytesSent()) + '\n'
					+ getString("requestCount") + equal
					+ integerFormat.format(tomcatInformations.getRequestCount()) + '\n'
					+ getString("errorCount") + equal
					+ integerFormat.format(tomcatInformations.getErrorCount()) + '\n'
					+ getString("processingTime") + equal
					+ integerFormat.format(tomcatInformations.getProcessingTime()) + '\n'
					+ getString("maxProcessingTime") + equal
					+ integerFormat.format(tomcatInformations.getMaxTime())));
			currentTable.addCell(phrase);
		}
	}

	private void writeMemoryInformations(MemoryInformations memoryInformations)
			throws BadElementException, IOException {
		addCell(memoryInformations.getMemoryDetails().replace(" Mo", ' ' + getString("Mo")));
		final long usedPermGen = memoryInformations.getUsedPermGen();
		if (usedPermGen > 0) {
			// perm gen est à 0 sous jrockit
			final long maxPermGen = memoryInformations.getMaxPermGen();
			addCell(getString("Memoire_Perm_Gen") + ':');
			if (maxPermGen > 0) {
				final Phrase permGenPhrase = new Phrase(
						integerFormat.format(usedPermGen / 1024 / 1024) + ' ' + getString("Mo")
								+ DIVIDE + integerFormat.format(maxPermGen / 1024 / 1024) + ' '
								+ getString("Mo") + BAR_SEPARATOR,
						cellFont);
				final Image permGenImage = Image.getInstance(
						Bar.toBarWithAlert(memoryInformations.getUsedPermGenPercentage()), null);
				permGenImage.scalePercent(50);
				permGenPhrase.add(new Chunk(permGenImage, 0, 0));
				currentTable.addCell(permGenPhrase);
			} else {
				addCell(integerFormat.format(usedPermGen / 1024 / 1024) + ' ' + getString("Mo"));
			}
		}
	}

	private void addCell(String string) {
		currentTable.addCell(new Phrase(string, cellFont));
	}
}
