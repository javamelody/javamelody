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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.Font;
import com.lowagie.text.Phrase;
import com.lowagie.text.pdf.PdfPCell;
import com.lowagie.text.pdf.PdfPTable;

import net.bull.javamelody.internal.model.Counter;
import net.bull.javamelody.internal.model.CounterRequest;

/**
 * Partie du rapport pdf pour les dépendances en exécution.
 * @author Emeric Vernat
 */
class PdfRuntimeDependenciesReport extends PdfAbstractReport {
	private final Counter counter;
	private final Font warningCellFont = PdfFonts.WARNING_CELL.getFont();
	private final Font severeCellFont = PdfFonts.SEVERE_CELL.getFont();
	private final Font normalFont = PdfFonts.NORMAL.getFont();
	private final Font cellFont = PdfFonts.TABLE_CELL.getFont();
	private final Font boldCellFont = PdfFonts.BOLD_CELL.getFont();
	private PdfPTable currentTable;
	private List<String> calledBeans;
	private double standardDeviation;

	PdfRuntimeDependenciesReport(Counter counter, Document document) {
		super(document);
		assert counter != null;
		this.counter = counter;
	}

	/**
	 * Retourne les dépendances entre classes selon les requêtes de counter,
	 * selon les méthodes réellement exécutés et monitorées sur la période sélectionnée.<br/>
	 * Le niveau de dépendance entre chaque classe est indiqué par le nombre de méthodes
	 * différentes appelées depuis une classe vers une autre classe
	 * (et non par le nombre d'exécutions et non plus par le nombre d'appels de méthodes).<br/>
	 * Une classe ne peut pas avoir de dépendance avec elle-même.<br/>
	 * Exemple:<br/><pre>
	 * ex :
	 * public class A {
	 *    @Inject
	 *    private B b;
	 *
	 *    public void a() {
	 *       b.b1();
	 *       for (int i = 0; i < 100; i++) {
	 *          b.b2();
	 *       }
	 *       if (false) {
	 *          b.b3();
	 *       }
	 *    }
	 * }
	 * </pre>
	 * Si seulement les classes A et B sont monitorées et si la méthode "a" est appelée au moins
	 * une fois, alors les "runtimeDependencies" contiendront une dépendance de A vers B.
	 * Cette dépendance sera de 2 méthodes (une pour "b1" et une pour "b2").
	 * @return Map
	 */
	Map<String, Map<String, Integer>> getRuntimeDependencies() {
		final Map<String, Set<CounterRequest>> methodsCalledByCallerBeans = getMethodsCalledByCallerBeans();

		final Map<String, Map<String, Integer>> result = new HashMap<>();
		for (final Map.Entry<String, Set<CounterRequest>> entry : methodsCalledByCallerBeans
				.entrySet()) {
			final String callerBean = entry.getKey();
			final Set<CounterRequest> childRequests = entry.getValue();
			final Map<String, Integer> nbMethodsCalledByCalledBean = new HashMap<>();
			for (final CounterRequest childRequest : childRequests) {
				final String calledBean = getClassNameFromRequest(childRequest);
				if (callerBean.equals(calledBean)) {
					// si le bean est lui même, il peut s'appeler autant qu'il veut
					// ce n'est pas une dépendance
					continue;
				}
				Integer nbOfMethodCalls = nbMethodsCalledByCalledBean.get(calledBean);
				if (nbOfMethodCalls == null) {
					nbOfMethodCalls = 1;
				} else {
					// on compte le nombre de méthodes appelées et non le nombre d'exécutions
					nbOfMethodCalls = nbOfMethodCalls + 1;
				}
				nbMethodsCalledByCalledBean.put(calledBean, nbOfMethodCalls);
			}

			// methodsCalled peut être vide soit parce que childRequestsByBeans est vide
			// (il n'y a pas d'appels de méthodes pour ce bean, que des requêtes sql par exemple),
			// soit parce qu'il n'y a que des appels de méthodes vers le bean lui-même
			if (!nbMethodsCalledByCalledBean.isEmpty()) {
				result.put(callerBean, nbMethodsCalledByCalledBean);
			}
		}

		return result;
	}

	private Map<String, Set<CounterRequest>> getMethodsCalledByCallerBeans() {
		assert counter.isBusinessFacadeCounter();
		final List<CounterRequest> requests = counter.getRequests();
		final Map<String, CounterRequest> requestsById = new HashMap<>();
		for (final CounterRequest request : requests) {
			requestsById.put(request.getId(), request);
		}

		// on compte le nombre de méthodes différentes appelées depuis un bean vers un autre bean
		// et non le nombre d'exécutions et non plus le nombre d'appels de méthodes
		final Map<String, Set<CounterRequest>> methodsCalledByCallerBeans = new HashMap<>();
		for (final CounterRequest request : requests) {
			final Set<String> childRequestIds = request.getChildRequestsExecutionsByRequestId()
					.keySet();
			if (childRequestIds.isEmpty()) {
				continue;
			}
			final String callerBean = getClassNameFromRequest(request);
			// c'est un Set donc on ne compte une méthode 'a' appelée depuis une classe qu'une seule fois
			// même si elle est appelée deux fois depuis la même classe
			// ou même à un seul endroit mais avec des exécutions dans une boucle
			Set<CounterRequest> childRequests = methodsCalledByCallerBeans.get(callerBean);
			if (childRequests == null) {
				childRequests = new HashSet<>();
				methodsCalledByCallerBeans.put(callerBean, childRequests);
			}
			for (final String childRequestId : childRequestIds) {
				// on ne regarde que les requêtes du même counter
				// (pas sql, et pas guice si on est dans spring)
				if (counter.isRequestIdFromThisCounter(childRequestId)) {
					final CounterRequest childRequest = requestsById.get(childRequestId);
					// il peut y avoir une non synchronisation temporaire entre la liste des requêtes
					// et les requêtes filles
					if (childRequest != null) {
						childRequests.add(childRequest);
					}
				}
			}
		}
		return methodsCalledByCallerBeans;
	}

	private static String getClassNameFromRequest(CounterRequest request) {
		final int lastIndexOf = request.getName().lastIndexOf('.');
		if (lastIndexOf != -1) {
			return request.getName().substring(0, lastIndexOf);
		}
		return request.getName();
	}

	private static double getStandardDeviation(
			Map<String, Map<String, Integer>> runtimeDependencies) {
		final List<Integer> values = new ArrayList<>();
		int sum = 0;
		for (final Map<String, Integer> beanDependencies : runtimeDependencies.values()) {
			for (final Integer value : beanDependencies.values()) {
				values.add(value);
				sum += value;
			}
		}
		final int mean = sum / values.size();
		int square = 0;
		for (final Integer value : values) {
			final int diff = value - mean;
			square += diff * diff;
		}
		return Math.sqrt((double) square / values.size());
	}

	private List<String> getCalledBeans(Map<String, Map<String, Integer>> runtimeDependencies) {
		final Set<String> calledBeansSet = new HashSet<>();
		for (final Map<String, Integer> values : runtimeDependencies.values()) {
			calledBeansSet.addAll(values.keySet());
		}
		final List<String> result = new ArrayList<>(calledBeansSet);
		Collections.sort(result);
		return result;
	}

	@Override
	void toPdf() throws DocumentException {
		final Map<String, Map<String, Integer>> runtimeDependencies = getRuntimeDependencies();
		if (runtimeDependencies.isEmpty()) {
			addToDocument(new Phrase(getString("Aucune_dependance"), normalFont));
			return;
		}
		addToDocument(new Phrase(getString("runtime_dependencies_desc"), normalFont));
		this.standardDeviation = getStandardDeviation(runtimeDependencies);
		this.calledBeans = getCalledBeans(runtimeDependencies);

		writeHeader();

		final List<String> callerBeans = new ArrayList<>(runtimeDependencies.keySet());
		Collections.sort(callerBeans);
		final PdfPCell defaultCell = getDefaultCell();
		boolean odd = false;
		for (final String callerBean : callerBeans) {
			if (odd) {
				defaultCell.setGrayFill(0.97f);
			} else {
				defaultCell.setGrayFill(1);
			}
			odd = !odd; // NOPMD
			final Map<String, Integer> beanDependencies = runtimeDependencies.get(callerBean);
			writeBeanDependencies(callerBean, beanDependencies);
		}
		addToDocument(currentTable);
	}

	private void writeBeanDependencies(String callerBean, Map<String, Integer> beanDependencies) {
		getDefaultCell().setHorizontalAlignment(Element.ALIGN_LEFT);
		addCell(callerBean, cellFont);
		getDefaultCell().setHorizontalAlignment(Element.ALIGN_RIGHT);
		for (final String calledBean : calledBeans) {
			final Integer dependency = beanDependencies.get(calledBean);
			if (dependency == null) {
				addCell("", cellFont);
			} else {
				final String s = dependency.toString();
				if (dependency > standardDeviation * 2) {
					addCell(s, severeCellFont);
				} else if (dependency > standardDeviation) {
					addCell(s, warningCellFont);
				} else {
					addCell(s, cellFont);
				}
			}
		}
	}

	private void writeHeader() throws DocumentException {
		final List<String> headers = new ArrayList<>();
		headers.add("Beans");
		headers.addAll(calledBeans);
		final int[] relativeWidths = new int[headers.size()];
		Arrays.fill(relativeWidths, 0, headers.size(), 1);
		relativeWidths[0] = 4;

		final PdfPTable table = new PdfPTable(headers.size());
		table.setWidthPercentage(100);
		table.setWidths(relativeWidths);
		table.setHeaderRows(1);
		final PdfPCell defaultCell = table.getDefaultCell();
		defaultCell.setGrayFill(0.9f);
		defaultCell.setHorizontalAlignment(Element.ALIGN_CENTER);
		defaultCell.setVerticalAlignment(Element.ALIGN_MIDDLE);
		defaultCell.setPaddingLeft(0);
		defaultCell.setPaddingRight(0);
		for (final String header : headers) {
			table.addCell(new Phrase(header, boldCellFont));
			// pas la première entête de colonne
			defaultCell.setRotation(90);
		}
		defaultCell.setRotation(0);
		defaultCell.setPaddingLeft(2);
		defaultCell.setPaddingRight(2);
		currentTable = table;
	}

	private PdfPCell getDefaultCell() {
		return currentTable.getDefaultCell();
	}

	private void addCell(String string, Font font) {
		currentTable.addCell(new Phrase(string, font));
	}
}
