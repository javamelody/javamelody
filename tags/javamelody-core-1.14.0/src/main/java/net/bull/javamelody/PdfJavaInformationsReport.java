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

import java.text.DecimalFormat;
import java.util.List;

import com.lowagie.text.Anchor;
import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.Font;
import com.lowagie.text.Phrase;
import com.lowagie.text.pdf.PdfPTable;

/**
 * Partie du rapport pdf pour les informations systèmes sur le serveur.
 * @author Emeric Vernat
 */
class PdfJavaInformationsReport {
	private final DecimalFormat decimalFormat = I18N.createPercentFormat();
	private final DecimalFormat integerFormat = I18N.createIntegerFormat();
	private final List<JavaInformations> javaInformationsList;
	private final Document document;
	private final Font cellFont = PdfDocumentFactory.TABLE_CELL_FONT;
	private final Font boldCellFont = PdfDocumentFactory.BOLD_CELL_FONT;
	private PdfPTable currentTable;

	PdfJavaInformationsReport(List<JavaInformations> javaInformationsList, Document document) {
		super();
		assert javaInformationsList != null;
		assert document != null;

		this.javaInformationsList = javaInformationsList;
		this.document = document;
	}

	void toPdf() throws DocumentException {
		for (final JavaInformations javaInformations : javaInformationsList) {
			currentTable = createJavaInformationsTable();
			writeSummary(javaInformations);
			addCell("");
			addCell("");
			document.add(currentTable);
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

	private void writeSummary(JavaInformations javaInformations) {
		addCell(getI18nString("Host") + ':');
		currentTable.addCell(new Phrase(javaInformations.getHost(), boldCellFont));
		addCell(getI18nString("memoire_utilisee") + ':');
		final String divide = " / ";
		final MemoryInformations memoryInformations = javaInformations.getMemoryInformations();
		addCell(integerFormat.format(memoryInformations.getUsedMemory() / 1024 / 1024) + ' '
				+ getI18nString("Mo") + divide
				+ integerFormat.format(memoryInformations.getMaxMemory() / 1024 / 1024) + ' '
				+ getI18nString("Mo"));
		addCell(getI18nString("nb_sessions_http") + ':');
		addCell(integerFormat.format(javaInformations.getSessionCount()));
		addCell(getI18nString("nb_threads_actifs") + "\n("
				+ getI18nString("Requetes_http_en_cours") + "):");
		addCell(integerFormat.format(javaInformations.getActiveThreadCount()));
		addCell(getI18nString("nb_connexions_actives") + ':');
		addCell(integerFormat.format(javaInformations.getActiveConnectionCount()));
		addCell(getI18nString("nb_connexions_utilisees") + "\n(" + getI18nString("ouvertes") + "):");
		final String usedConnectionCount;
		if (javaInformations.getMaxConnectionCount() < 0) {
			usedConnectionCount = integerFormat.format(javaInformations.getUsedConnectionCount());
		} else {
			usedConnectionCount = integerFormat.format(javaInformations.getUsedConnectionCount())
					+ divide + integerFormat.format(javaInformations.getMaxConnectionCount());
		}
		addCell(usedConnectionCount);
		if (javaInformations.getSystemLoadAverage() >= 0) {
			addCell(getI18nString("Charge_systeme") + ':');
			addCell(decimalFormat.format(javaInformations.getSystemLoadAverage()));
		}
	}

	void writeInformationsDetails() throws DocumentException {
		for (final JavaInformations javaInformations : javaInformationsList) {
			currentTable = createJavaInformationsTable();
			writeSummary(javaInformations);
			writeDetails(javaInformations);
			document.add(currentTable);
		}
	}

	private void writeDetails(JavaInformations javaInformations) {
		addCell(getI18nString("OS") + ':');
		addCell(javaInformations.getOS() + " (" + javaInformations.getAvailableProcessors() + ' '
				+ getI18nString("coeurs") + ')');
		addCell(getI18nString("Java") + ':');
		addCell(javaInformations.getJavaVersion());
		addCell(getI18nString("JVM") + ':');
		addCell(javaInformations.getJvmVersion());
		addCell(getI18nString("PID") + ':');
		addCell(javaInformations.getPID());
		if (javaInformations.getUnixOpenFileDescriptorCount() >= 0) {
			addCell(getI18nString("nb_fichiers") + ':');
			addCell(integerFormat.format(javaInformations.getUnixOpenFileDescriptorCount()) + " / "
					+ integerFormat.format(javaInformations.getUnixMaxFileDescriptorCount()));
		}
		if (javaInformations.getServerInfo() != null) {
			addCell(getI18nString("Serveur") + ':');
			addCell(javaInformations.getServerInfo());
			addCell(getI18nString("Contexte_webapp") + ':');
			addCell(javaInformations.getContextPath());
		}
		addCell(getI18nString("Demarrage") + ':');
		addCell(I18N.createDateAndTimeFormat().format(javaInformations.getStartDate()));
		addCell(getI18nString("Arguments_JVM") + ':');
		addCell(javaInformations.getJvmArguments());
		addCell(getI18nString("Gestion_memoire") + ':');
		writeMemoryInformations(javaInformations.getMemoryInformations());
		if (javaInformations.getFreeDiskSpaceInTemp() >= 0) {
			// on considère que l'espace libre sur le disque dur est celui sur la partition du répertoire temporaire
			addCell(getI18nString("Free_disk_space") + ':');
			addCell(integerFormat.format(javaInformations.getFreeDiskSpaceInTemp() / 1024 / 1024)
					+ ' ' + getI18nString("Mo"));
		}
		addCell(getI18nString("Base_de_donnees") + ':');
		addCell(javaInformations.getDataBaseVersion());
		if (javaInformations.getDataSourceDetails() != null) {
			addCell(getI18nString("DataSource_jdbc") + ':');
			addCell(javaInformations.getDataSourceDetails());
			addCell("");
			final Anchor anchor = new Anchor("DataSource reference", PdfDocumentFactory.BLUE_FONT);
			anchor.setName("DataSource reference");
			anchor
					.setReference("http://commons.apache.org/dbcp/apidocs/org/apache/commons/dbcp/BasicDataSource.html");
			currentTable.addCell(anchor);
		}
		if (javaInformations.isDependenciesEnabled()) {
			addCell(getI18nString("Dependencies") + ':');
			addCell(I18N.getFormattedString("nb_dependencies", javaInformations
					.getDependenciesList().size())
					+ " ;\n" + javaInformations.getDependencies());
		}
		addCell("");
		addCell("");
	}

	private void writeMemoryInformations(MemoryInformations memoryInformations) {
		addCell(memoryInformations.getMemoryDetails().replace(" Mo", ' ' + getI18nString("Mo")));
		final long usedPermGen = memoryInformations.getUsedPermGen();
		final long maxPermGen = memoryInformations.getMaxPermGen();
		addCell(getI18nString("Memoire_Perm_Gen") + ':');
		if (maxPermGen >= 0) {
			addCell(integerFormat.format(usedPermGen / 1024 / 1024) + ' ' + getI18nString("Mo")
					+ " / " + integerFormat.format(maxPermGen / 1024 / 1024) + ' '
					+ getI18nString("Mo"));
		} else {
			addCell(integerFormat.format(usedPermGen / 1024 / 1024) + ' ' + getI18nString("Mo"));
		}
	}

	private static String getI18nString(String key) {
		return I18N.getString(key);
	}

	private void addCell(String string) {
		currentTable.addCell(new Phrase(string, cellFont));
	}
}
