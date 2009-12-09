/*
 * Copyright 2008-2009 by Emeric Vernat, Bull
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
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import com.lowagie.text.BadElementException;
import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.Font;
import com.lowagie.text.Image;
import com.lowagie.text.Paragraph;
import com.lowagie.text.Phrase;
import com.lowagie.text.pdf.PdfPCell;
import com.lowagie.text.pdf.PdfPTable;

/**
 * Partie du rapport pdf pour un compteur.
 * @author Emeric Vernat
 */
class PdfCounterReport {
	private final Collector collector;
	private final Counter counter;
	private final Period period;
	private final boolean includeGraph;
	private final Document document;
	private final CounterRequestAggregation counterRequestAggregation;
	private final DecimalFormat systemErrorFormat = I18N.createPercentFormat();
	private final DecimalFormat integerFormat = I18N.createIntegerFormat();
	private final Font cellFont = PdfDocumentFactory.TABLE_CELL_FONT;
	private final Font infoCellFont = PdfDocumentFactory.INFO_CELL_FONT;
	private final Font warningCellFont = PdfDocumentFactory.WARNING_CELL_FONT;
	private final Font severeCellFont = PdfDocumentFactory.SEVERE_CELL_FONT;
	private final Font normalFont = PdfDocumentFactory.NORMAL_FONT;
	private PdfPTable currentTable;

	PdfCounterReport(Collector collector, Counter counter, Period period, boolean includeGraph,
			Document document) {
		super();
		assert collector != null;
		assert counter != null;
		assert period != null;
		assert document != null;
		this.collector = collector;
		this.counter = counter;
		this.period = period;
		this.includeGraph = includeGraph;
		this.document = document;
		this.counterRequestAggregation = new CounterRequestAggregation(counter);
	}

	void toPdf() throws DocumentException, IOException {
		final List<CounterRequest> requests = counterRequestAggregation.getRequests();
		if (requests.isEmpty()) {
			writeNoRequests();
		} else if (isErrorCounter()) {
			// il y a au moins une "request" d'erreur puisque la liste n'est pas vide
			assert !requests.isEmpty();
			final List<CounterRequest> summaryRequest = Collections.singletonList(requests.get(0));
			writeRequests(counter.getChildCounterName(), summaryRequest);
		} else {
			// 1. synthèse
			final CounterRequest globalRequest = counterRequestAggregation.getGlobalRequest();
			final List<CounterRequest> summaryRequests = Arrays.asList(globalRequest,
					counterRequestAggregation.getWarningRequest(), counterRequestAggregation
							.getSevereRequest());
			writeRequests(counter.getChildCounterName(), summaryRequests);
		}
	}

	String getCounterName() {
		return counter.getName();
	}

	String getCounterIconName() {
		return counter.getIconName();
	}

	boolean isErrorCounter() {
		return counter.isErrorCounter();
	}

	void writeRequestDetails() throws DocumentException, IOException {
		// détails des requêtes
		final List<CounterRequest> requests = counterRequestAggregation.getRequests();
		if (requests.isEmpty()) {
			writeNoRequests();
		} else {
			// on n'inclue pas pour l'instant les graphs d'évolution des requêtes
			// pour des raisons de place et de volume
			writeRequests(counter.getChildCounterName(), requests);
		}
	}

	private void writeNoRequests() throws DocumentException {
		final String msg;
		if (isErrorCounter()) {
			msg = "Aucune_erreur";
		} else {
			msg = "Aucune_requete";
		}
		document.add(new Phrase(getI18nString(msg), normalFont));
	}

	void writeErrorDetails() throws DocumentException {
		// détails des erreurs
		new PdfCounterErrorReport(counter, document).toPdf();
	}

	private void writeRequests(String childCounterName, List<CounterRequest> requestList)
			throws DocumentException, IOException {
		assert requestList != null;
		writeHeader(childCounterName);

		final PdfPCell defaultCell = getDefaultCell();
		boolean odd = false;
		for (final CounterRequest request : requestList) {
			if (odd) {
				defaultCell.setGrayFill(0.97f);
			} else {
				defaultCell.setGrayFill(1);
			}
			odd = !odd; // NOPMD
			writeRequest(request);
		}
		document.add(currentTable);

		// débit et liens
		writeFooter();
	}

	private void writeHeader(String childCounterName) throws DocumentException {
		final List<String> headers = createHeaders(childCounterName);
		final int[] relativeWidths = new int[headers.size()];
		Arrays.fill(relativeWidths, 0, headers.size(), 1);
		relativeWidths[0] = 10; // requête
		if (includeGraph) {
			relativeWidths[0] = 8;
			relativeWidths[1] = 2; // graph d'évolution
		}

		currentTable = PdfDocumentFactory.createPdfPTable(headers, relativeWidths);
	}

	private List<String> createHeaders(String childCounterName) {
		final List<String> headers = new ArrayList<String>();
		if (isErrorCounter()) {
			headers.add(getI18nString("Erreur"));
		} else {
			headers.add(getI18nString("Requete"));
		}
		if (includeGraph) {
			headers.add(getI18nString("Evolution"));
		}
		if (counterRequestAggregation.isTimesDisplayed()) {
			headers.add(getI18nString("temps_cumule"));
			headers.add(getI18nString("Hits"));
			headers.add(getI18nString("Temps_moyen"));
			headers.add(getI18nString("Temps_max"));
			headers.add(getI18nString("Ecart_type"));
		} else {
			headers.add(getI18nString("Hits"));
		}
		if (counterRequestAggregation.isCpuTimesDisplayed()) {
			headers.add(getI18nString("temps_cpu_cumule"));
			headers.add(getI18nString("Temps_cpu_moyen"));
		}
		if (!isErrorCounter()) {
			headers.add(getI18nString("erreur_systeme"));
		}
		if (counterRequestAggregation.isResponseSizeDisplayed()) {
			headers.add(getI18nString("Taille_moyenne"));
		}
		if (counterRequestAggregation.isChildHitsDisplayed()) {
			headers.add(I18N.getFormattedString("hits_fils_moyens", childCounterName));
			headers.add(I18N.getFormattedString("temps_fils_moyen", childCounterName));
		}
		return headers;
	}

	private void writeFooter() throws DocumentException {
		final List<CounterRequest> requests = counterRequestAggregation.getRequests();
		final CounterRequest globalRequest = counterRequestAggregation.getGlobalRequest();
		// delta ni négatif ni à 0
		final long deltaMillis = Math.max(System.currentTimeMillis()
				- counter.getStartDate().getTime(), 1);
		final long hitsParMinute = 60 * 1000 * globalRequest.getHits() / deltaMillis;
		final String key;
		if (isErrorCounter()) {
			key = "nb_erreurs";
		} else {
			key = "nb_requetes";
		}
		final Paragraph footer = new Paragraph(I18N.getFormattedString(key, integerFormat
				.format(hitsParMinute), integerFormat.format(requests.size())), normalFont);
		footer.setAlignment(Element.ALIGN_RIGHT);
		document.add(footer);
	}

	private void writeRequest(CounterRequest request) throws BadElementException, IOException {
		getDefaultCell().setHorizontalAlignment(Element.ALIGN_LEFT);
		final String name = request.getName();
		if (name.length() > 1000) {
			// si la requête fait plus de 1000 caractères, on la coupe pour y voir quelque chose
			addCell(name.substring(0, 1000) + "...");
		} else {
			addCell(name);
		}
		if (includeGraph) {
			writeRequestGraph(request);
		}
		getDefaultCell().setHorizontalAlignment(Element.ALIGN_RIGHT);
		final CounterRequest globalRequest = counterRequestAggregation.getGlobalRequest();
		if (counterRequestAggregation.isTimesDisplayed()) {
			addPercentageCell(request.getDurationsSum(), globalRequest.getDurationsSum());
			addCell(integerFormat.format(request.getHits()));
			final int mean = request.getMean();
			currentTable.addCell(new Phrase(integerFormat.format(mean), getSlaFont(mean)));
			addCell(integerFormat.format(request.getMaximum()));
			addCell(integerFormat.format(request.getStandardDeviation()));
		} else {
			addCell(integerFormat.format(request.getHits()));
		}
		if (counterRequestAggregation.isCpuTimesDisplayed()) {
			addPercentageCell(request.getCpuTimeSum(), globalRequest.getCpuTimeSum());
			final int cpuTimeMean = request.getCpuTimeMean();
			currentTable.addCell(new Phrase(integerFormat.format(cpuTimeMean),
					getSlaFont(cpuTimeMean)));
		}
		if (!isErrorCounter()) {
			addCell(systemErrorFormat.format(request.getSystemErrorPercentage()));
		}
		if (counterRequestAggregation.isResponseSizeDisplayed()) {
			addCell(integerFormat.format(request.getResponseSizeMean() / 1024));
		}
		if (counterRequestAggregation.isChildHitsDisplayed()) {
			addCell(integerFormat.format(request.getChildHitsMean()));
			addCell(integerFormat.format(request.getChildDurationsMean()));
		}
	}

	private void writeRequestGraph(CounterRequest request) throws BadElementException, IOException {
		final JRobin jrobin = collector.getJRobin(request.getId());
		if (jrobin == null) {
			addCell("");
		} else {
			final byte[] img = jrobin.graph(period, 100, 50);
			final Image image = Image.getInstance(img);
			image.scalePercent(50);
			currentTable.addCell(image);
		}
	}

	Font getSlaFont(int mean) {
		final Font font;
		if (mean < counterRequestAggregation.getWarningThreshold()) {
			// si cette moyenne est < à la moyenne globale + 1 écart-type (paramétrable), c'est bien
			font = infoCellFont;
		} else if (mean < counterRequestAggregation.getSevereThreshold()) {
			// sinon, si cette moyenne est < à la moyenne globale + 2 écart-types (paramétrable),
			// attention à cette requête qui est plus longue que les autres
			font = warningCellFont;
		} else {
			// sinon, (cette moyenne est > à la moyenne globale + 2 écart-types),
			// cette requête est très longue par rapport aux autres ;
			// il peut être opportun de l'optimiser si possible
			font = severeCellFont;
		}
		return font;
	}

	private static String getI18nString(String key) {
		return I18N.getString(key);
	}

	private PdfPCell getDefaultCell() {
		return currentTable.getDefaultCell();
	}

	private void addPercentageCell(long dividende, long diviseur) {
		if (diviseur == 0) {
			addCell("0");
		} else {
			addCell(integerFormat.format(100 * dividende / diviseur));
		}
	}

	private void addCell(String string) {
		currentTable.addCell(new Phrase(string, cellFont));
	}
}
