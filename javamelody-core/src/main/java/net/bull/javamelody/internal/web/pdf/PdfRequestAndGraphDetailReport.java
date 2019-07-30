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
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import com.lowagie.text.Chunk;
import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.Font;
import com.lowagie.text.FontFactory;
import com.lowagie.text.Image;
import com.lowagie.text.Paragraph;
import com.lowagie.text.Phrase;
import com.lowagie.text.pdf.PdfPCell;
import com.lowagie.text.pdf.PdfPTable;

import net.bull.javamelody.JdbcWrapper;
import net.bull.javamelody.internal.common.I18N;
import net.bull.javamelody.internal.model.Collector;
import net.bull.javamelody.internal.model.CollectorServer;
import net.bull.javamelody.internal.model.Counter;
import net.bull.javamelody.internal.model.CounterRequest;
import net.bull.javamelody.internal.model.CounterRequestRumData;
import net.bull.javamelody.internal.model.DatabaseInformations;
import net.bull.javamelody.internal.model.JRobin;
import net.bull.javamelody.internal.model.Range;
import net.bull.javamelody.internal.web.html.HtmlCounterReport;

/**
 * Rapport pdf pour le détail d'une requête.
 * @author Emeric Vernat
 */
public class PdfRequestAndGraphDetailReport extends PdfAbstractTableReport {
	private final Collector collector;
	private final CollectorServer collectorServer;
	private final Range range;
	private final List<Counter> counters;
	private final String graphName;
	private final CounterRequest request;
	private final Map<String, CounterRequest> requestsById;
	private final PdfDocumentFactory pdfDocumentFactory;
	private final DecimalFormat systemErrorFormat = I18N.createPercentFormat();
	private final DecimalFormat nbExecutionsFormat = I18N.createPercentFormat();
	private final DecimalFormat integerFormat = I18N.createIntegerFormat();
	private final Font cellFont = PdfFonts.TABLE_CELL.getFont();
	private final Font boldFont = PdfFonts.BOLD.getFont();
	private final Font courierFont = FontFactory.getFont(FontFactory.COURIER, 5.5f, Font.NORMAL);

	PdfRequestAndGraphDetailReport(Collector collector, CollectorServer collectorServer,
			Range range, String graphName, PdfDocumentFactory pdfDocumentFactory, Document document)
			throws IOException {
		super(document);
		assert collector != null;
		assert range != null;
		assert graphName != null;
		assert pdfDocumentFactory != null;
		this.collector = collector;
		this.collectorServer = collectorServer;
		this.range = range;
		this.graphName = graphName;
		this.counters = collector.getRangeCounters(range);
		this.requestsById = mapAllRequestsById();
		this.request = requestsById.get(graphName);
		this.pdfDocumentFactory = pdfDocumentFactory;
	}

	private Map<String, CounterRequest> mapAllRequestsById() {
		final Map<String, CounterRequest> result = new HashMap<String, CounterRequest>();
		for (final Counter counter : counters) {
			for (final CounterRequest aRequest : counter.getRequests()) {
				result.put(aRequest.getId(), aRequest);
			}
		}
		return result;
	}

	@Override
	void toPdf() throws DocumentException, IOException {
		if (request != null) {
			if (request.getRumData() != null && request.getRumData().getHits() != 0) {
				writeRequestRumData();
			}

			writeHeader();

			writeRequests();

			addTableToDocument();

			if (JdbcWrapper.SINGLETON.getSqlCounter().isRequestIdFromThisCounter(request.getId())
					&& !request.getName().toLowerCase(Locale.ENGLISH).startsWith("alter ")) {
				// inutile d'essayer d'avoir le plan d'exécution des requêtes sql
				// telles que "alter session set ..." (cf issue 152)
				writeSqlRequestExplainPlan();
			}
		}

		if (isGraphDisplayed()) {
			writeGraph();
		}

		if (request != null && request.getStackTrace() != null) {
			final Paragraph paragraph = new Paragraph("\n", cellFont);
			paragraph.setIndentationLeft(20);
			paragraph.setIndentationRight(20);
			paragraph.add(new Phrase("Stack-trace\n", boldFont));
			paragraph.add(new Phrase(request.getStackTrace().replace("\t", "        "), cellFont));
			addToDocument(paragraph);
		}
	}

	private void writeRequestRumData() throws DocumentException {
		final CounterRequestRumData rumData = request.getRumData();
		final DecimalFormat percentFormat = I18N.createPercentFormat();
		final int networkTimeMean = rumData.getNetworkTimeMean();
		final int serverMean = request.getMean();
		final int domProcessingMean = rumData.getDomProcessingMean();
		final int pageRenderingMean = rumData.getPageRenderingMean();
		final int totalTime = networkTimeMean + serverMean + domProcessingMean + pageRenderingMean;
		final double networkPercent = 100d * networkTimeMean / totalTime;
		final double serverPercent = 100d * serverMean / totalTime;
		final double domProcessingPercent = 100d * domProcessingMean / totalTime;
		final double pageRenderingPercent = 100d * pageRenderingMean / totalTime;

		final PdfPTable table = new PdfPTable(2);
		table.setHorizontalAlignment(Element.ALIGN_LEFT);
		table.setWidthPercentage(25);
		table.getDefaultCell().setBorderWidth(0);
		table.addCell(new Phrase(I18N.getString("Network"), cellFont));
		table.addCell(new Phrase(integerFormat.format(networkTimeMean) + " ms ("
				+ percentFormat.format(networkPercent) + "%)", cellFont));
		table.addCell(new Phrase(I18N.getString("Server"), cellFont));
		table.addCell(new Phrase(integerFormat.format(serverMean) + " ms ("
				+ percentFormat.format(serverPercent) + "%)", cellFont));
		table.addCell(new Phrase(I18N.getString("DOM_processing"), cellFont));
		table.addCell(new Phrase(integerFormat.format(domProcessingMean) + " ms ("
				+ percentFormat.format(domProcessingPercent) + "%)", cellFont));
		table.addCell(new Phrase(I18N.getString("Page_rendering"), cellFont));
		table.addCell(new Phrase(integerFormat.format(pageRenderingMean) + " ms ("
				+ percentFormat.format(pageRenderingPercent) + "%)", cellFont));
		addToDocument(table);
		addToDocument(new Phrase("\n", cellFont));
	}

	private void writeHeader() throws DocumentException {
		final List<String> headers = createHeaders();
		final int[] relativeWidths = new int[headers.size()];
		Arrays.fill(relativeWidths, 0, headers.size(), 1);
		relativeWidths[0] = 8; // requête

		initTable(headers, relativeWidths);
	}

	private List<String> createHeaders() {
		final List<String> headers = new ArrayList<String>();
		headers.add(getString("Requete"));
		final boolean hasChildren = !request.getChildRequestsExecutionsByRequestId().isEmpty();
		if (hasChildren) {
			headers.add(getString("Hits_par_requete"));
		}
		headers.add(getString("Temps_moyen"));
		headers.add(getString("Temps_max"));
		headers.add(getString("Ecart_type"));
		headers.add(getString("Temps_cpu_moyen"));
		if (isAllocatedKBytesDisplayed()) {
			headers.add(getString("Ko_alloues_moyens"));
		}
		headers.add(getString("erreur_systeme"));
		final Counter parentCounter = getCounterByRequestId(request);
		final boolean allChildHitsDisplayed = parentCounter != null
				&& parentCounter.getChildCounterName() != null && request.hasChildHits();
		if (allChildHitsDisplayed) {
			final String childCounterName = parentCounter.getChildCounterName();
			headers.add(getFormattedString("hits_fils_moyens", childCounterName));
			headers.add(getFormattedString("temps_fils_moyen", childCounterName));
		}
		return headers;
	}

	private void writeRequests() throws IOException, DocumentException {
		final Map<String, Long> childRequests = request.getChildRequestsExecutionsByRequestId();
		final boolean hasChildren = !childRequests.isEmpty();
		final Counter parentCounter = getCounterByRequestId(request);
		final boolean allChildHitsDisplayed = parentCounter != null
				&& parentCounter.getChildCounterName() != null && request.hasChildHits();

		final PdfPCell defaultCell = getDefaultCell();
		defaultCell.setLeading(2, 1);
		defaultCell.setPaddingTop(0);

		nextRow();
		writeRequest(request, -1, allChildHitsDisplayed);

		if (hasChildren) {
			writeChildRequests(childRequests, allChildHitsDisplayed);
		}
	}

	private void writeChildRequests(Map<String, Long> childRequests, boolean allChildHitsDisplayed)
			throws IOException, DocumentException {
		for (final Map.Entry<String, Long> entry : childRequests.entrySet()) {
			final CounterRequest childRequest = requestsById.get(entry.getKey());
			if (childRequest != null) {
				nextRow();
				final Long nbExecutions = entry.getValue();
				final float executionsByRequest = (float) nbExecutions / request.getHits();
				writeRequest(childRequest, executionsByRequest, allChildHitsDisplayed);
			}
		}
	}

	private void writeRequest(CounterRequest childRequest, float executionsByRequest,
			boolean allChildHitsDisplayed) throws IOException, DocumentException {
		final PdfPCell defaultCell = getDefaultCell();
		defaultCell.setHorizontalAlignment(Element.ALIGN_LEFT);
		final Paragraph paragraph = new Paragraph(defaultCell.getLeading() + cellFont.getSize());
		if (executionsByRequest != -1) {
			paragraph.setIndentationLeft(5);
		}
		final Counter parentCounter = getCounterByRequestId(childRequest);
		if (parentCounter != null && parentCounter.getIconName() != null) {
			paragraph.add(new Chunk(getSmallImage(parentCounter.getIconName()), 0, -1));
		}
		paragraph.add(new Phrase(childRequest.getName(), cellFont));
		final PdfPCell requestCell = new PdfPCell();
		requestCell.addElement(paragraph);
		requestCell.setGrayFill(defaultCell.getGrayFill());
		requestCell.setPaddingTop(defaultCell.getPaddingTop());
		addCell(requestCell);

		defaultCell.setHorizontalAlignment(Element.ALIGN_RIGHT);
		if (executionsByRequest != -1) {
			addCell(nbExecutionsFormat.format(executionsByRequest));
		} else {
			final boolean hasChildren = !request.getChildRequestsExecutionsByRequestId().isEmpty();
			if (hasChildren) {
				addCell("");
			}
		}
		writeRequestValues(childRequest, allChildHitsDisplayed);
	}

	private void writeRequestValues(CounterRequest aRequest, boolean allChildHitsDisplayed) {
		final PdfPCell defaultCell = getDefaultCell();
		defaultCell.setHorizontalAlignment(Element.ALIGN_RIGHT);
		addCell(integerFormat.format(aRequest.getMean()));
		addCell(integerFormat.format(aRequest.getMaximum()));
		addCell(integerFormat.format(aRequest.getStandardDeviation()));
		if (aRequest.getCpuTimeMean() >= 0) {
			addCell(integerFormat.format(aRequest.getCpuTimeMean()));
		} else {
			addCell("");
		}
		if (isAllocatedKBytesDisplayed()) {
			if (aRequest.getAllocatedKBytesMean() >= 0) {
				addCell(integerFormat.format(aRequest.getAllocatedKBytesMean()));
			} else {
				addCell("");
			}
		}
		addCell(systemErrorFormat.format(aRequest.getSystemErrorPercentage()));
		if (allChildHitsDisplayed) {
			final boolean childHitsDisplayed = aRequest.hasChildHits();
			if (childHitsDisplayed) {
				addCell(integerFormat.format(aRequest.getChildHitsMean()));
			} else {
				addCell("");
			}
			if (childHitsDisplayed) {
				addCell(integerFormat.format(aRequest.getChildDurationsMean()));
			} else {
				addCell("");
			}
		}
	}

	private boolean isAllocatedKBytesDisplayed() {
		return request.getAllocatedKBytesMean() >= 0;
	}

	private void writeGraph() throws IOException, DocumentException {
		final JRobin jrobin = collector.getJRobin(graphName);
		if (jrobin != null) {
			final byte[] img = jrobin.graph(range, 960, 400);
			final Image image = Image.getInstance(img);
			image.scalePercent(50);

			final PdfPTable table = new PdfPTable(1);
			table.setHorizontalAlignment(Element.ALIGN_CENTER);
			table.setWidthPercentage(100);
			table.getDefaultCell().setBorder(0);
			table.addCell("\n");
			table.addCell(image);
			table.getDefaultCell().setHorizontalAlignment(Element.ALIGN_RIGHT);
			table.addCell(new Phrase(getString("graph_units"), cellFont));
			addToDocument(table);
		} else {
			// just in case request is null and collector.getJRobin(graphName) is null, we must write something in the document
			addToDocument(new Phrase("\n", cellFont));
		}
	}

	private void writeSqlRequestExplainPlan() throws DocumentException {
		try {
			final String explainPlan;
			if (collectorServer == null) {
				explainPlan = DatabaseInformations.explainPlanFor(request.getName());
			} else {
				explainPlan = collectorServer.collectSqlRequestExplainPlan(
						collector.getApplication(), request.getName());
			}
			if (explainPlan != null) {
				final Paragraph paragraph = new Paragraph("", cellFont);
				paragraph.add(new Phrase('\n' + getString("Plan_d_execution") + '\n', boldFont));
				paragraph.add(new Phrase(explainPlan, courierFont));
				addToDocument(paragraph);
			}
		} catch (final Exception e) {
			final Paragraph paragraph = new Paragraph("", cellFont);
			paragraph.add(new Phrase('\n' + getString("Plan_d_execution") + '\n', boldFont));
			paragraph.add(new Phrase(e.toString(), cellFont));
			addToDocument(paragraph);
		}
	}

	private Counter getCounterByRequestId(CounterRequest aRequest) {
		final String myRequestId = aRequest.getId();
		for (final Counter counter : counters) {
			if (counter.isRequestIdFromThisCounter(myRequestId)) {
				return counter;
			}
		}
		return null;
	}

	private boolean isGraphDisplayed() throws IOException {
		return request == null || getCounterByRequestId(request) != null
				&& HtmlCounterReport.isRequestGraphDisplayed(getCounterByRequestId(request))
				// on vérifie aussi que l'instance de jrobin existe pour faire le graph,
				// notamment si les statistiques ont été réinitialisées, ce qui vide les instances de jrobin
				&& collector.getJRobin(request.getId()) != null;
	}

	private Image getSmallImage(String resourceFileName) throws DocumentException, IOException {
		return pdfDocumentFactory.getSmallImage(resourceFileName);
	}
}
