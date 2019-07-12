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
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JTable;

import com.lowagie.text.Font;

import net.bull.javamelody.internal.model.Counter;
import net.bull.javamelody.internal.model.CounterRequest;
import net.bull.javamelody.internal.model.RemoteCollector;
import net.bull.javamelody.swing.table.MDefaultTableCellRenderer;
import net.bull.javamelody.swing.table.MTable;
import net.bull.javamelody.swing.table.MTableScrollPane;

/**
 * Panel des utilisations d'une requêtes.
 * @author Emeric Vernat
 */
class CounterRequestUsagesPanel extends CounterRequestAbstractPanel {
	private static final long serialVersionUID = 1L;

	private final class NameTableCellRenderer extends MDefaultTableCellRenderer {
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
			setIcon(getCounterIcon(counter, 0));
			return super.getTableCellRendererComponent(jtable, value, isSelected, hasFocus, row,
					column);
		}
	}

	CounterRequestUsagesPanel(RemoteCollector remoteCollector, CounterRequest request) {
		super(remoteCollector);

		final String graphLabel = truncate(getString("Utilisations_de") + ' ' + request.getName(),
				50);
		setName(graphLabel);

		final JLabel label = new JLabel(
				' ' + getString("Utilisations_de") + ' ' + request.getName());
		label.setBorder(BorderFactory.createEmptyBorder(10, 0, 5, 0));
		label.setFont(label.getFont().deriveFont(Font.BOLD));
		add(label, BorderLayout.NORTH);

		final MTableScrollPane<CounterRequest> scrollPane = createScrollPane();
		final List<CounterRequest> requests = new ArrayList<>();
		for (final Counter counter : getCounters()) {
			for (final CounterRequest counterRequest : counter.getOrderedRequests()) {
				if (counterRequest.containsChildRequest(request.getId())) {
					requests.add(counterRequest);
				}
			}
		}

		getTable().setList(requests);
		add(scrollPane, BorderLayout.CENTER);

		add(createButtonsPanel(true), BorderLayout.SOUTH);
	}

	private MTableScrollPane<CounterRequest> createScrollPane() {
		final MTable<CounterRequest> table = getTable();
		final MTableScrollPane<CounterRequest> tableScrollPane = new MTableScrollPane<>(table);

		table.addColumn("name", getString("Requete"));
		table.setColumnCellRenderer("name", new NameTableCellRenderer());

		return tableScrollPane;
	}

	private static String truncate(String string, int maxLength) {
		return string.substring(0, Math.min(string.length(), maxLength));
	}
}
