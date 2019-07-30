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

import net.bull.javamelody.internal.common.I18N;
import net.bull.javamelody.internal.model.Collector;
import net.bull.javamelody.internal.model.Counter;
import net.bull.javamelody.internal.model.CounterRequest;
import net.bull.javamelody.internal.model.CounterRequestAggregation;
import net.bull.javamelody.internal.model.JRobin;
import net.bull.javamelody.internal.model.Range;

/**
 * Partie du rapport pdf pour un compteur.
 * @author Emeric Vernat
 */
class PdfCounterReport extends PdfAbstractTableReport {
	private final Collector collector;
	private final Counter counter;
	private final Range range;
	private final boolean includeGraph;
	private final CounterRequestAggregation counterRequestAggregation;
	private final DecimalFormat systemErrorFormat = I18N.createPercentFormat();
	private final DecimalFormat integerFormat = I18N.createIntegerFormat();
	private final Font infoCellFont = PdfFonts.INFO_CELL.getFont();
	private final Font warningCellFont = PdfFonts.WARNING_CELL.getFont();
	private final Font severeCellFont = PdfFonts.SEVERE_CELL.getFont();
	private final Font normalFont = PdfFonts.NORMAL.getFont();

	PdfCounterReport(Collector collector, Counter counter, Range range, boolean includeGraph,
			Document document) {
		super(document);
		assert collector != null;
		assert counter != null;
		assert range != null;
		this.collector = collector;
		this.counter = counter;
		this.range = range;
		this.includeGraph = includeGraph;
		this.counterRequestAggregation = new CounterRequestAggregation(counter);
	}

	@Override
	void toPdf() throws DocumentException, IOException {
		final List<CounterRequest> requests = counterRequestAggregation.getRequests();
		if (requests.isEmpty()) {
			writeNoRequests();
		} else if (isErrorAndNotJobCounter()) {
			// il y a au moins une "request" d'erreur puisque la liste n'est pas vide
			assert !requests.isEmpty();
			final List<CounterRequest> summaryRequest = Collections.singletonList(requests.get(0));
			writeRequests(counter.getChildCounterName(), summaryRequest);
		} else {
			// 1. synthèse
			final CounterRequest globalRequest = counterRequestAggregation.getGlobalRequest();
			final List<CounterRequest> summaryRequests = Arrays.asList(globalRequest,
					counterRequestAggregation.getWarningRequest(),
					counterRequestAggregation.getSevereRequest());
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

	private boolean isJobCounter() {
		return counter.isJobCounter();
	}

	private boolean isErrorAndNotJobCounter() {
		return isErrorCounter() && !isJobCounter();
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
		if (isJobCounter()) {
			msg = "Aucun_job";
		} else if (isErrorCounter()) {
			msg = "Aucune_erreur";
		} else {
			msg = "Aucune_requete";
		}
		addToDocument(new Phrase(getString(msg), normalFont));
	}

	void writeErrorDetails() throws DocumentException {
		// détails des erreurs
		new PdfCounterErrorReport(counter, getDocument()).toPdf();
	}

	void writeRequests(String childCounterName, List<CounterRequest> requestList)
			throws DocumentException, IOException {
		assert requestList != null;
		writeHeader(childCounterName);

		for (final CounterRequest request : requestList) {
			nextRow();
			writeRequest(request);
		}
		addTableToDocument();

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

		initTable(headers, relativeWidths);
	}

	private List<String> createHeaders(String childCounterName) {
		final List<String> headers = new ArrayList<String>();
		headers.add(getRequestHeader());
		if (includeGraph) {
			headers.add(getString("Evolution"));
		}
		if (counterRequestAggregation.isTimesDisplayed()) {
			headers.add(getString("temps_cumule"));
			headers.add(getString("Hits"));
			headers.add(getString("Temps_moyen"));
			headers.add(getString("Temps_max"));
			headers.add(getString("Ecart_type"));
		} else {
			headers.add(getString("Hits"));
		}
		if (counterRequestAggregation.isCpuTimesDisplayed()) {
			headers.add(getString("temps_cpu_cumule"));
			headers.add(getString("Temps_cpu_moyen"));
		}
		if (counterRequestAggregation.isAllocatedKBytesDisplayed()) {
			headers.add(getString("Ko_alloues_moyens"));
		}
		if (!isErrorAndNotJobCounter()) {
			headers.add(getString("erreur_systeme"));
		}
		if (counterRequestAggregation.isResponseSizeDisplayed()) {
			headers.add(getString("Taille_moyenne"));
		}
		if (counterRequestAggregation.isChildHitsDisplayed()) {
			headers.add(getFormattedString("hits_fils_moyens", childCounterName));
			headers.add(getFormattedString("temps_fils_moyen", childCounterName));
		}
		return headers;
	}

	private String getRequestHeader() {
		if (isJobCounter()) {
			return getString("Job");
		} else if (isErrorCounter()) {
			return getString("Erreur");
		} else {
			return getString("Requete");
		}
	}

	private void writeFooter() throws DocumentException {
		final List<CounterRequest> requests = counterRequestAggregation.getRequests();
		final CounterRequest globalRequest = counterRequestAggregation.getGlobalRequest();
		// delta ni négatif ni à 0
		final long deltaMillis = Math
				.max(System.currentTimeMillis() - counter.getStartDate().getTime(), 1);
		final long hitsParMinute = 60 * 1000 * globalRequest.getHits() / deltaMillis;
		final String key;
		if (isJobCounter()) {
			key = "nb_jobs";
		} else if (isErrorCounter()) {
			key = "nb_erreurs";
		} else {
			key = "nb_requetes";
		}
		final Paragraph footer = new Paragraph(getFormattedString(key,
				integerFormat.format(hitsParMinute), integerFormat.format(requests.size())),
				normalFont);
		footer.setAlignment(Element.ALIGN_RIGHT);
		addToDocument(footer);
	}

	private void writeRequest(CounterRequest request) throws BadElementException, IOException {
		getDefaultCell().setHorizontalAlignment(Element.ALIGN_LEFT);
		addCell(getShortRequestName(request));
		if (includeGraph) {
			writeRequestGraph(request);
		}
		getDefaultCell().setHorizontalAlignment(Element.ALIGN_RIGHT);
		final CounterRequest globalRequest = counterRequestAggregation.getGlobalRequest();
		if (counterRequestAggregation.isTimesDisplayed()) {
			addPercentageCell(request.getDurationsSum(), globalRequest.getDurationsSum());
			addCell(integerFormat.format(request.getHits()));
			final int mean = request.getMean();
			addCell(new Phrase(integerFormat.format(mean), getSlaFont(mean)));
			addCell(integerFormat.format(request.getMaximum()));
			addCell(integerFormat.format(request.getStandardDeviation()));
		} else {
			addCell(integerFormat.format(request.getHits()));
		}
		if (counterRequestAggregation.isCpuTimesDisplayed()) {
			addPercentageCell(request.getCpuTimeSum(), globalRequest.getCpuTimeSum());
			final int cpuTimeMean = request.getCpuTimeMean();
			addCell(new Phrase(integerFormat.format(cpuTimeMean), getSlaFont(cpuTimeMean)));
		}
		if (counterRequestAggregation.isAllocatedKBytesDisplayed()) {
			final long allocatedKBytesMean = request.getAllocatedKBytesMean();
			addCell(integerFormat.format(allocatedKBytesMean));
		}
		if (!isErrorAndNotJobCounter()) {
			addCell(systemErrorFormat.format(request.getSystemErrorPercentage()));
		}
		if (counterRequestAggregation.isResponseSizeDisplayed()) {
			addCell(integerFormat.format(request.getResponseSizeMean() / 1024L));
		}
		if (counterRequestAggregation.isChildHitsDisplayed()) {
			addCell(integerFormat.format(request.getChildHitsMean()));
			addCell(integerFormat.format(request.getChildDurationsMean()));
		}
	}

	private String getShortRequestName(CounterRequest request) {
		final String name = request.getName();
		if (name.length() > 1000) {
			// si la requête fait plus de 1000 caractères, on la coupe pour y voir quelque chose
			return name.substring(0, 1000) + "...";
		}
		return name;
	}

	private void writeRequestGraph(CounterRequest request) throws BadElementException, IOException {
		final JRobin jrobin = collector.getJRobin(request.getId());
		if (jrobin == null) {
			addCell("");
		} else {
			final byte[] img = jrobin.graph(range, 100, 50);
			final Image image = Image.getInstance(img);
			image.scalePercent(50);
			addCell(image);
		}
	}

	Font getSlaFont(int mean) {
		final Font font;
		if (mean < counterRequestAggregation.getWarningThreshold() || mean == 0) {
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

	private void addPercentageCell(long dividende, long diviseur) {
		if (diviseur == 0) {
			addCell("0");
		} else {
			addCell(integerFormat.format(100 * dividende / diviseur));
		}
	}
}
