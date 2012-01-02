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

import java.awt.Graphics;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.text.DecimalFormat;
import java.util.List;

import javax.imageio.ImageIO;

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

/**
 * Partie du rapport pdf pour les informations systèmes sur le serveur.
 * @author Emeric Vernat
 */
class PdfJavaInformationsReport {
	private static final String DIVIDE = " / ";
	private static final String BAR_SEPARATOR = "   ";
	private final boolean noDatabase = Parameters.isNoDatabase();
	private final DecimalFormat decimalFormat = I18N.createPercentFormat();
	private final DecimalFormat integerFormat = I18N.createIntegerFormat();
	private final List<JavaInformations> javaInformationsList;
	private final Document document;
	private final Font cellFont = PdfFonts.TABLE_CELL.getFont();
	private final Font boldCellFont = PdfFonts.BOLD_CELL.getFont();
	private PdfPTable currentTable;

	static final class Bar {
		// constantes pour l'affichage d'une barre avec pourcentage
		private static final double MIN_VALUE = 0;
		private static final double MAX_VALUE = 100;
		private static final int PARTIAL_BLOCKS = 5;
		private static final int FULL_BLOCKS = 10;
		private static final double UNIT_SIZE = (MAX_VALUE - MIN_VALUE)
				/ (FULL_BLOCKS * PARTIAL_BLOCKS);

		private final BufferedImage image = new BufferedImage(106, 10, BufferedImage.TYPE_INT_ARGB);
		private final Graphics graphics = image.getGraphics();
		private int x; // initialisé à 0

		private Bar() {
			super();
		}

		static BufferedImage toBar(double percentValue) throws IOException {
			final Bar bar = new Bar();
			bar.draw(percentValue);
			bar.graphics.dispose();
			return bar.image;
		}

		// méthode inspirée de VisualScoreTag dans LambdaProbe/JStripe (Licence GPL)
		private void draw(double percentValue) throws IOException { // NOPMD
			assert x == 0;
			final double myPercent = Math.max(Math.min(percentValue, 100d), 0d);
			final int fullBlockCount = (int) Math.floor(myPercent / (UNIT_SIZE * PARTIAL_BLOCKS));
			final int partialBlockIndex = (int) Math.floor((myPercent - fullBlockCount * UNIT_SIZE
					* PARTIAL_BLOCKS)
					/ UNIT_SIZE);

			addImage(getBarImage(fullBlockCount > 0 || partialBlockIndex > 0 ? "a" : "a0"));

			final BufferedImage fullBody = getBarImage(String.valueOf(PARTIAL_BLOCKS));
			for (int i = 0; i < fullBlockCount; i++) {
				addImage(fullBody);
			}

			if (partialBlockIndex > 0) {
				final BufferedImage partialBody = getBarImage(String.valueOf(partialBlockIndex));
				addImage(partialBody);
			}

			final int emptyBlocks = FULL_BLOCKS - fullBlockCount - (partialBlockIndex > 0 ? 1 : 0);

			if (emptyBlocks > 0) {
				final BufferedImage emptyBody = getBarImage(String.valueOf(0));
				for (int i = 0; i < emptyBlocks; i++) {
					addImage(emptyBody);
				}
			}

			addImage(getBarImage(fullBlockCount == FULL_BLOCKS ? "b" : "b0"));
		}

		private void addImage(BufferedImage img) {
			graphics.drawImage(img, x, 0, null);
			x += img.getWidth();
		}

		private static BufferedImage getBarImage(String key) throws IOException {
			// ici, ne pas utiliser Toolkit.createImage et surtout pas ImageIcon (sur un serveur)
			return ImageIO.read(Bar.class.getResource(Parameters.getResourcePath("bar/rb_" + key
					+ ".gif")));
		}
	}

	PdfJavaInformationsReport(List<JavaInformations> javaInformationsList, Document document) {
		super();
		assert javaInformationsList != null;
		assert document != null;

		this.javaInformationsList = javaInformationsList;
		this.document = document;
	}

	void toPdf() throws DocumentException, IOException {
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

	private void writeSummary(JavaInformations javaInformations) throws BadElementException,
			IOException {
		addCell(getI18nString("Host") + ':');
		currentTable.addCell(new Phrase(javaInformations.getHost(), boldCellFont));
		addCell(getI18nString("memoire_utilisee") + ':');
		final MemoryInformations memoryInformations = javaInformations.getMemoryInformations();
		final long usedMemory = memoryInformations.getUsedMemory();
		final long maxMemory = memoryInformations.getMaxMemory();
		final Phrase memoryPhrase = new Phrase(integerFormat.format(usedMemory / 1024 / 1024) + ' '
				+ getI18nString("Mo") + DIVIDE + integerFormat.format(maxMemory / 1024 / 1024)
				+ ' ' + getI18nString("Mo") + BAR_SEPARATOR, cellFont);
		final Image memoryImage = Image.getInstance(
				Bar.toBar(memoryInformations.getUsedMemoryPercentage()), null);
		memoryImage.scalePercent(50);
		memoryPhrase.add(new Chunk(memoryImage, 0, 0));
		currentTable.addCell(memoryPhrase);
		if (javaInformations.getSessionCount() >= 0) {
			addCell(getI18nString("nb_sessions_http") + ':');
			addCell(integerFormat.format(javaInformations.getSessionCount()));
		}
		addCell(getI18nString("nb_threads_actifs") + "\n("
				+ getI18nString("Requetes_http_en_cours") + "):");
		addCell(integerFormat.format(javaInformations.getActiveThreadCount()));
		if (!noDatabase) {
			addCell(getI18nString("nb_connexions_actives") + ':');
			addCell(integerFormat.format(javaInformations.getActiveConnectionCount()));
			addCell(getI18nString("nb_connexions_utilisees") + "\n(" + getI18nString("ouvertes")
					+ "):");
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
						Bar.toBar(javaInformations.getUsedConnectionPercentage()), null);
				usedConnectionCountImage.scalePercent(50);
				usedConnectionCountPhrase.add(new Chunk(usedConnectionCountImage, 0, 0));
				currentTable.addCell(usedConnectionCountPhrase);
			}
		}
		if (javaInformations.getSystemLoadAverage() >= 0) {
			addCell(getI18nString("Charge_systeme") + ':');
			addCell(decimalFormat.format(javaInformations.getSystemLoadAverage()));
		}
	}

	void writeInformationsDetails() throws DocumentException, IOException {
		for (final JavaInformations javaInformations : javaInformationsList) {
			currentTable = createJavaInformationsTable();
			writeSummary(javaInformations);
			writeDetails(javaInformations);
			document.add(currentTable);
		}
	}

	private void writeDetails(JavaInformations javaInformations) throws BadElementException,
			IOException {
		addCell(getI18nString("OS") + ':');
		final Phrase osPhrase = new Phrase("", cellFont);
		final String osIconName = HtmlJavaInformationsReport
				.getOSIconName(javaInformations.getOS());
		final String separator = "   ";
		if (osIconName != null) {
			final Image osImage = PdfDocumentFactory.getImage("servers/" + osIconName);
			osImage.scalePercent(40);
			osPhrase.add(new Chunk(osImage, 0, 0));
			osPhrase.add(separator);
		}
		osPhrase.add(javaInformations.getOS() + " (" + javaInformations.getAvailableProcessors()
				+ ' ' + getI18nString("coeurs") + ')');
		currentTable.addCell(osPhrase);
		addCell(getI18nString("Java") + ':');
		addCell(javaInformations.getJavaVersion());
		addCell(getI18nString("JVM") + ':');
		final Phrase jvmVersionPhrase = new Phrase(javaInformations.getJvmVersion(), cellFont);
		if (javaInformations.getJvmVersion().contains("Client")) {
			jvmVersionPhrase.add(separator);
			final Image alertImage = PdfDocumentFactory.getImage("alert.png");
			alertImage.scalePercent(50);
			jvmVersionPhrase.add(new Chunk(alertImage, 0, -2));
		}
		currentTable.addCell(jvmVersionPhrase);
		addCell(getI18nString("PID") + ':');
		addCell(javaInformations.getPID());
		if (javaInformations.getUnixOpenFileDescriptorCount() >= 0) {
			writeFileDescriptorCounts(javaInformations);
		}
		final String serverInfo = javaInformations.getServerInfo();
		if (serverInfo != null) {
			writeServerInfo(serverInfo);
			addCell(getI18nString("Contexte_webapp") + ':');
			addCell(javaInformations.getContextPath());
		}
		addCell(getI18nString("Demarrage") + ':');
		addCell(I18N.createDateAndTimeFormat().format(javaInformations.getStartDate()));
		addCell(getI18nString("Arguments_JVM") + ':');
		addCell(javaInformations.getJvmArguments());
		if (javaInformations.getSessionCount() >= 0) {
			addCell(getI18nString("httpSessionsMeanAge") + ':');
			addCell(integerFormat.format(javaInformations.getSessionMeanAgeInMinutes()));
		}
		writeTomcatInformations(javaInformations.getTomcatInformationsList());
		addCell(getI18nString("Gestion_memoire") + ':');
		writeMemoryInformations(javaInformations.getMemoryInformations());
		if (javaInformations.getFreeDiskSpaceInTemp() >= 0) {
			// on considère que l'espace libre sur le disque dur est celui sur la partition du répertoire temporaire
			addCell(getI18nString("Free_disk_space") + ':');
			addCell(integerFormat.format(javaInformations.getFreeDiskSpaceInTemp() / 1024 / 1024)
					+ ' ' + getI18nString("Mo"));
		}
		writeDatabaseVersionAndDataSourceDetails(javaInformations);
		if (javaInformations.isDependenciesEnabled()) {
			addCell(getI18nString("Dependencies") + ':');
			addCell(I18N.getFormattedString("nb_dependencies", javaInformations
					.getDependenciesList().size())
					+ " ;\n" + javaInformations.getDependencies());
		}
		addCell("");
		addCell("");
	}

	private void writeServerInfo(String serverInfo) throws BadElementException, IOException {
		addCell(getI18nString("Serveur") + ':');
		final Phrase serverInfoPhrase = new Phrase("", cellFont);
		final String applicationServerIconName = HtmlJavaInformationsReport
				.getApplicationServerIconName(serverInfo);
		if (applicationServerIconName != null) {
			final Image applicationServerImage = PdfDocumentFactory.getImage("servers/"
					+ applicationServerIconName);
			applicationServerImage.scalePercent(40);
			serverInfoPhrase.add(new Chunk(applicationServerImage, 0, 0));
			serverInfoPhrase.add("   ");
		}
		serverInfoPhrase.add(serverInfo);
		currentTable.addCell(serverInfoPhrase);
	}

	private void writeFileDescriptorCounts(JavaInformations javaInformations)
			throws BadElementException, IOException {
		final long unixOpenFileDescriptorCount = javaInformations.getUnixOpenFileDescriptorCount();
		final long unixMaxFileDescriptorCount = javaInformations.getUnixMaxFileDescriptorCount();
		addCell(getI18nString("nb_fichiers") + ':');
		final Phrase fileDescriptorCountPhrase = new Phrase(
				integerFormat.format(unixOpenFileDescriptorCount) + DIVIDE
						+ integerFormat.format(unixMaxFileDescriptorCount) + BAR_SEPARATOR,
				cellFont);
		final Image fileDescriptorCountImage = Image.getInstance(
				Bar.toBar(javaInformations.getUnixOpenFileDescriptorPercentage()), null);
		fileDescriptorCountImage.scalePercent(50);
		fileDescriptorCountPhrase.add(new Chunk(fileDescriptorCountImage, 0, 0));
		currentTable.addCell(fileDescriptorCountPhrase);
	}

	private void writeDatabaseVersionAndDataSourceDetails(JavaInformations javaInformations) {
		if (!noDatabase && javaInformations.getDataBaseVersion() != null) {
			addCell(getI18nString("Base_de_donnees") + ':');
			addCell(javaInformations.getDataBaseVersion());
		}
		if (javaInformations.getDataSourceDetails() != null) {
			addCell(getI18nString("DataSource_jdbc") + ':');
			addCell(javaInformations.getDataSourceDetails());
			addCell("");
			final Anchor anchor = new Anchor("DataSource reference", PdfFonts.BLUE.getFont());
			anchor.setName("DataSource reference");
			anchor.setReference("http://commons.apache.org/dbcp/apidocs/org/apache/commons/dbcp/BasicDataSource.html");
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
			final Phrase phrase = new Phrase(getI18nString("busyThreads") + equal
					+ integerFormat.format(currentThreadsBusy) + DIVIDE
					+ integerFormat.format(tomcatInformations.getMaxThreads()) + BAR_SEPARATOR,
					cellFont);
			final Image threadsImage = Image
					.getInstance(
							Bar.toBar(100d * currentThreadsBusy
									/ tomcatInformations.getMaxThreads()), null);
			threadsImage.scalePercent(50);
			phrase.add(new Chunk(threadsImage, 0, 0));

			phrase.add(new Chunk('\n' + getI18nString("bytesReceived") + equal
					+ integerFormat.format(tomcatInformations.getBytesReceived()) + '\n'
					+ getI18nString("bytesSent") + equal
					+ integerFormat.format(tomcatInformations.getBytesSent()) + '\n'
					+ getI18nString("requestCount") + equal
					+ integerFormat.format(tomcatInformations.getRequestCount()) + '\n'
					+ getI18nString("errorCount") + equal
					+ integerFormat.format(tomcatInformations.getErrorCount()) + '\n'
					+ getI18nString("processingTime") + equal
					+ integerFormat.format(tomcatInformations.getProcessingTime()) + '\n'
					+ getI18nString("maxProcessingTime") + equal
					+ integerFormat.format(tomcatInformations.getMaxTime())));
			currentTable.addCell(phrase);
		}
	}

	private void writeMemoryInformations(MemoryInformations memoryInformations)
			throws BadElementException, IOException {
		addCell(memoryInformations.getMemoryDetails().replace(" Mo", ' ' + getI18nString("Mo")));
		final long usedPermGen = memoryInformations.getUsedPermGen();
		if (usedPermGen > 0) {
			// perm gen est à 0 sous jrockit
			final long maxPermGen = memoryInformations.getMaxPermGen();
			addCell(getI18nString("Memoire_Perm_Gen") + ':');
			if (maxPermGen > 0) {
				final Phrase permGenPhrase = new Phrase(
						integerFormat.format(usedPermGen / 1024 / 1024) + ' ' + getI18nString("Mo")
								+ DIVIDE + integerFormat.format(maxPermGen / 1024 / 1024) + ' '
								+ getI18nString("Mo") + BAR_SEPARATOR, cellFont);
				final Image permGenImage = Image.getInstance(
						Bar.toBar(memoryInformations.getUsedPermGenPercentage()), null);
				permGenImage.scalePercent(50);
				permGenPhrase.add(new Chunk(permGenImage, 0, 0));
				currentTable.addCell(permGenPhrase);
			} else {
				addCell(integerFormat.format(usedPermGen / 1024 / 1024) + ' ' + getI18nString("Mo"));
			}
		}
	}

	private static String getI18nString(String key) {
		return I18N.getString(key);
	}

	private void addCell(String string) {
		currentTable.addCell(new Phrase(string, cellFont));
	}
}
