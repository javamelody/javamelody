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
package net.bull.javamelody;

import java.awt.BorderLayout;
import java.awt.Component;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.JPanel;
import javax.swing.JTable;
import javax.swing.table.TableColumn;

import net.bull.javamelody.internal.model.Counter;
import net.bull.javamelody.internal.model.CounterRequest;
import net.bull.javamelody.internal.model.RemoteCollector;
import net.bull.javamelody.swing.Utilities;
import net.bull.javamelody.swing.table.MDefaultTableCellRenderer;
import net.bull.javamelody.swing.table.MDoubleTableCellRenderer;
import net.bull.javamelody.swing.table.MIntegerTableCellRenderer;
import net.bull.javamelody.swing.table.MTable;
import net.bull.javamelody.swing.table.MTableScrollPane;

/**
 * Panel du tableau de requêtes dans le détail d'une requête.
 * @author Emeric Vernat
 */
class CounterRequestDetailTablePanel extends CounterRequestAbstractPanel {
	private static final long serialVersionUID = 1L;

	private final CounterRequest request;

	@SuppressWarnings("all")
	private final Map<String, Long> childRequestsExecutions;

	private final class ChildValueTableCellRenderer extends MIntegerTableCellRenderer {
		private static final long serialVersionUID = 1L;

		ChildValueTableCellRenderer() {
			super();
		}

		@Override
		public Component getTableCellRendererComponent(JTable jtable, Object value,
				boolean isSelected, boolean hasFocus, int row, int column) {
			final MTable<CounterRequest> myTable = getTable();
			final CounterRequest counterRequest = myTable.getList()
					.get(myTable.convertRowIndexToModel(row));
			final Integer myValue;
			if (counterRequest.hasChildHits()) {
				myValue = (Integer) value;
			} else {
				myValue = null;
			}
			return super.getTableCellRendererComponent(jtable, myValue, isSelected, hasFocus, row,
					column);
		}
	}

	private final class NameTableCellRenderer extends MDefaultTableCellRenderer {
		private static final int CHILD_MARGIN = 10;

		private static final long serialVersionUID = 1L;

		NameTableCellRenderer() {
			super();
		}

		@Override
		public Component getTableCellRendererComponent(JTable jtable, Object value,
				boolean isSelected, boolean hasFocus, int row, int column) {
			final MTable<CounterRequest> myTable = getTable();
			final CounterRequest counterRequest = myTable.getList()
					.get(myTable.convertRowIndexToModel(row));
			final Counter counter = getCounterByRequestId(counterRequest);
			if (counterRequest.equals(getRequest())) {
				setIcon(getCounterIcon(counter, 0));
			} else {
				setIcon(getCounterIcon(counter, CHILD_MARGIN));
			}
			return super.getTableCellRendererComponent(jtable, value, isSelected, hasFocus, row,
					column);
		}
	}

	private final class NbExecutionsTableCellRenderer extends MDoubleTableCellRenderer {
		private static final long serialVersionUID = 1L;

		NbExecutionsTableCellRenderer() {
			super();
		}

		@Override
		public Component getTableCellRendererComponent(JTable jtable, Object value,
				boolean isSelected, boolean hasFocus, int row, int column) {
			final MTable<CounterRequest> myTable = getTable();
			final CounterRequest counterRequest = myTable.getList()
					.get(myTable.convertRowIndexToModel(row));
			final CounterRequest parentRequest = getRequest();

			final Float executionsByRequest;
			if (counterRequest.equals(parentRequest)) {
				executionsByRequest = null;
			} else {
				final Long nbExecutions = getChildRequestExecutions(counterRequest);
				executionsByRequest = (float) nbExecutions / parentRequest.getHits();
			}

			return super.getTableCellRendererComponent(jtable, executionsByRequest, isSelected,
					hasFocus, row, column);
		}
	}

	CounterRequestDetailTablePanel(RemoteCollector remoteCollector, CounterRequest request) {
		super(remoteCollector);
		final Map<String, CounterRequest> requestsById = mapAllRequestsById();
		this.request = requestsById.get(request.getId());
		this.childRequestsExecutions = this.request.getChildRequestsExecutionsByRequestId();

		final MTableScrollPane<CounterRequest> scrollPane = createScrollPane();
		final List<CounterRequest> requests = new ArrayList<>();
		requests.add(this.request);

		if (!childRequestsExecutions.isEmpty()) {
			for (final Map.Entry<String, Long> entry : childRequestsExecutions.entrySet()) {
				final CounterRequest childRequest = requestsById.get(entry.getKey());
				if (childRequest != null) {
					requests.add(childRequest);
				}
			}
		}
		getTable().setList(requests);
		Utilities.adjustTableHeight(getTable());
		add(scrollPane, BorderLayout.CENTER);

		final JPanel buttonsPanel = createButtonsPanel(true);
		buttonsPanel.add(createXmlJsonButton(request));
		add(buttonsPanel, BorderLayout.SOUTH);
	}

	private MTableScrollPane<CounterRequest> createScrollPane() {
		final MTable<CounterRequest> table = getTable();
		final MTableScrollPane<CounterRequest> tableScrollPane = new MTableScrollPane<>(table);

		table.addColumn("name", getString("Requete"));
		table.setColumnCellRenderer("name", new NameTableCellRenderer());
		if (!childRequestsExecutions.isEmpty()) {
			final TableColumn nbExecutionsColumn = new TableColumn(table.getColumnCount());
			nbExecutionsColumn.setIdentifier(table.getColumnCount());
			nbExecutionsColumn.setHeaderValue(getString("Hits_par_requete"));
			table.addColumn(nbExecutionsColumn);

			nbExecutionsColumn.setCellRenderer(new NbExecutionsTableCellRenderer());
		}
		table.addColumn("mean", getString("Temps_moyen"));
		table.addColumn("maximum", getString("Temps_max"));
		table.addColumn("standardDeviation", getString("Ecart_type"));
		table.addColumn("cpuTimeMean", getString("Temps_cpu_moyen"));
		table.addColumn("systemErrorPercentage", getString("erreur_systeme"));
		table.setColumnCellRenderer("cpuTimeMean", new MIntegerTableCellRenderer() {
			private static final long serialVersionUID = 1L;

			@Override
			public void setValue(Object value) {
				final Integer cpuTimeMean = (Integer) value;
				if (cpuTimeMean >= 0) {
					super.setValue(cpuTimeMean);
				} else {
					super.setValue(null);
				}
			}
		});

		final Counter parentCounter = getCounterByRequestId(request);
		final boolean allChildHitsDisplayed = parentCounter != null
				&& parentCounter.getChildCounterName() != null && request.hasChildHits();
		if (allChildHitsDisplayed) {
			final String childCounterName = parentCounter.getChildCounterName();
			table.addColumn("childHitsMean",
					getFormattedString("hits_fils_moyens", childCounterName));
			table.addColumn("childDurationsMean",
					getFormattedString("temps_fils_moyen", childCounterName));
			final ChildValueTableCellRenderer childValueTableCellRenderer = new ChildValueTableCellRenderer();
			table.setColumnCellRenderer("childHitsMean", childValueTableCellRenderer);
			table.setColumnCellRenderer("childDurationsMean", childValueTableCellRenderer);
		}

		return tableScrollPane;
	}

	private Map<String, CounterRequest> mapAllRequestsById() {
		final Map<String, CounterRequest> result = new HashMap<>();
		for (final Counter counter : getCounters()) {
			for (final CounterRequest counterRequest : counter.getRequests()) {
				result.put(counterRequest.getId(), counterRequest);
			}
		}
		return result;
	}

	final CounterRequest getRequest() {
		return request;
	}

	final Long getChildRequestExecutions(CounterRequest counterRequest) {
		return childRequestsExecutions.get(counterRequest.getId());
	}
}
