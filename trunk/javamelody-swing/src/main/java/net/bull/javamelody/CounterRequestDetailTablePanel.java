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

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.BorderFactory;
import javax.swing.Icon;
import javax.swing.JTable;
import javax.swing.border.Border;
import javax.swing.table.TableColumn;

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
class CounterRequestDetailTablePanel extends MelodyPanel {
	@SuppressWarnings("all")
	static final Border CHILD_BORDER = BorderFactory.createEmptyBorder(0, 10, 0, 0);

	private static final long serialVersionUID = 1L;

	private final CounterRequest request;

	@SuppressWarnings("all")
	private final Map<String, Long> childRequestsExecutions;

	private final Range range;

	@SuppressWarnings("all")
	private final List<Counter> counters;

	private final MTable<CounterRequest> table;

	private final class ChildValueTableCellRenderer extends MIntegerTableCellRenderer {
		private static final long serialVersionUID = 1L;

		ChildValueTableCellRenderer() {
			super();
		}

		@Override
		public Component getTableCellRendererComponent(JTable jtable, Object value,
				boolean isSelected, boolean hasFocus, int row, int column) {
			final MTable<CounterRequest> myTable = getTable();
			final CounterRequest counterRequest = myTable.getList().get(
					myTable.convertRowIndexToModel(row));
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
		private static final long serialVersionUID = 1L;

		@SuppressWarnings("all")
		private final Map<String, Icon> iconByName = new HashMap<String, Icon>();

		NameTableCellRenderer() {
			super();
		}

		@Override
		public Component getTableCellRendererComponent(JTable jtable, Object value,
				boolean isSelected, boolean hasFocus, int row, int column) {
			final MTable<CounterRequest> myTable = getTable();
			final CounterRequest counterRequest = myTable.getList().get(
					myTable.convertRowIndexToModel(row));
			if (counterRequest.equals(getRequest())) {
				setBorder(null);
			} else {
				// TODO ne marche pas ?
				setBorder(CHILD_BORDER);
			}
			final Counter counter = getCounterByRequestId(counterRequest);
			if (counter != null && counter.getIconName() != null) {
				setIcon(getIcon(counter.getIconName()));
			} else {
				setIcon(null);
			}
			return super.getTableCellRendererComponent(jtable, value, isSelected, hasFocus, row,
					column);
		}

		private Icon getIcon(String iconName) {
			Icon icon = iconByName.get(iconName);
			if (icon == null) {
				icon = ImageIconCache.getScaledImageIcon(iconName, 16, 16);
				iconByName.put(iconName, icon);
			}
			return icon;
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
			final CounterRequest counterRequest = myTable.getList().get(
					myTable.convertRowIndexToModel(row));
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

	CounterRequestDetailTablePanel(RemoteCollector remoteCollector, CounterRequest request,
			Range range) throws IOException {
		super(remoteCollector);
		this.request = request;
		this.childRequestsExecutions = request.getChildRequestsExecutionsByRequestId();
		this.range = range;

		this.counters = remoteCollector.getCollector().getRangeCountersToBeDisplayed(range);

		setBorder(BorderFactory.createEmptyBorder(10, 0, 10, 0));

		final MTableScrollPane<CounterRequest> scrollPane = createScrollPane();
		this.table = scrollPane.getTable();
		final List<CounterRequest> requests = new ArrayList<CounterRequest>();
		requests.add(request);

		if (!childRequestsExecutions.isEmpty()) {
			final Map<String, CounterRequest> requestsById = mapAllRequestsById();
			for (final Map.Entry<String, Long> entry : childRequestsExecutions.entrySet()) {
				final CounterRequest childRequest = requestsById.get(entry.getKey());
				if (childRequest != null) {
					requests.add(childRequest);
				}
			}
		}
		table.setList(requests);
		Utilities.adjustTableHeight(table);
		add(scrollPane, BorderLayout.CENTER);

		// TODO
		//		if (doesRequestDisplayUsages(request)) {
		//			writeln("<div align='right' class='noPrint'>");
		//			writeln("<a href='?part=usages&amp;graph=" + request.getId() + "'>");
		//			writeln("<img src='?resource=find.png' alt='#Chercher_utilisations#' ");
		//			writeln("title='#Chercher_utilisations#'/> #Chercher_utilisations#</a></div>");
		//		} else {
		//			writeln("<br/>");
		//		}
	}

	private MTableScrollPane<CounterRequest> createScrollPane() {
		final MTableScrollPane<CounterRequest> tableScrollPane = new MTableScrollPane<CounterRequest>();
		final MTable<CounterRequest> myTable = tableScrollPane.getTable();

		// TODO graphique pour requêtes filles comme dans StatisticTablePanel

		myTable.addColumn("name", I18N.getString("Requete"));
		myTable.setColumnCellRenderer("name", new NameTableCellRenderer());
		if (!childRequestsExecutions.isEmpty()) {
			final TableColumn nbExecutionsColumn = new TableColumn(myTable.getColumnCount());
			nbExecutionsColumn.setIdentifier(myTable.getColumnCount());
			nbExecutionsColumn.setHeaderValue(I18N.getString("Hits_par_requete"));
			myTable.addColumn(nbExecutionsColumn);

			nbExecutionsColumn.setCellRenderer(new NbExecutionsTableCellRenderer());
		}
		myTable.addColumn("mean", I18N.getString("Temps_moyen"));
		myTable.addColumn("maximum", I18N.getString("Temps_max"));
		myTable.addColumn("standardDeviation", I18N.getString("Ecart_type"));
		myTable.addColumn("cpuTimeMean", I18N.getString("Temps_cpu_moyen"));
		myTable.addColumn("systemErrorPercentage", I18N.getString("erreur_systeme"));
		myTable.setColumnCellRenderer("cpuTimeMean", new MIntegerTableCellRenderer() {
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
				&& parentCounter.getChildCounterName() != null;
		if (allChildHitsDisplayed) {
			final String childCounterName = parentCounter.getChildCounterName();
			myTable.addColumn("childHitsMean",
					I18N.getFormattedString("hits_fils_moyens", childCounterName));
			myTable.addColumn("childDurationsMean",
					I18N.getFormattedString("temps_fils_moyen", childCounterName));
			final ChildValueTableCellRenderer childValueTableCellRenderer = new ChildValueTableCellRenderer();
			myTable.setColumnCellRenderer("childHitsMean", childValueTableCellRenderer);
			myTable.setColumnCellRenderer("childDurationsMean", childValueTableCellRenderer);
		}

		myTable.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseClicked(MouseEvent e) {
				if (e.getClickCount() == 2) {
					final CounterRequest counterRequest = myTable.getSelectedObject();
					if (!counterRequest.equals(getRequest())) {
						try {
							showRequestDetail(counterRequest);
						} catch (final IOException ex) {
							showException(ex);
						}
					}
				}
			}
		});

		return tableScrollPane;
	}

	private Map<String, CounterRequest> mapAllRequestsById() {
		final Map<String, CounterRequest> result = new HashMap<String, CounterRequest>();
		for (final Counter counter : counters) {
			for (final CounterRequest counterRequest : counter.getRequests()) {
				result.put(counterRequest.getId(), counterRequest);
			}
		}
		return result;
	}

	final Counter getCounterByRequestId(CounterRequest counterRequest) {
		final String requestId = counterRequest.getId();
		for (final Counter counter : counters) {
			if (counter.isRequestIdFromThisCounter(requestId)) {
				return counter;
			}
		}
		return null;
	}

	final MTable<CounterRequest> getTable() {
		return table;
	}

	final CounterRequest getRequest() {
		return request;
	}

	final Long getChildRequestExecutions(CounterRequest counterRequest) {
		return childRequestsExecutions.get(counterRequest.getId());
	}

	final void showRequestDetail(CounterRequest counterRequest) throws IOException {
		final Counter parentCounter = getCounterByRequestId(counterRequest);
		final CounterRequestDetailPanel panel = new CounterRequestDetailPanel(getRemoteCollector(),
				counterRequest, parentCounter, range);
		MainPanel.addOngletFromChild(this, panel);
	}
}
