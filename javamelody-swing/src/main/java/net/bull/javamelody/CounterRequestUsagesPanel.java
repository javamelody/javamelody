/*
 * Copyright 2008-2012 by Emeric Vernat
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
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.BorderFactory;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JTable;

import net.bull.javamelody.swing.table.MDefaultTableCellRenderer;
import net.bull.javamelody.swing.table.MTable;
import net.bull.javamelody.swing.table.MTableScrollPane;

import com.lowagie.text.Font;

/**
 * Panel des utilisations d'une requêtes.
 * @author Emeric Vernat
 */
class CounterRequestUsagesPanel extends CounterRequestAbstractPanel {
	private static final long serialVersionUID = 1L;

	private final class NameTableCellRenderer extends MDefaultTableCellRenderer {
		private static final long serialVersionUID = 1L;

		@SuppressWarnings("all")
		private final Map<String, ImageIcon> iconByName = new HashMap<String, ImageIcon>();

		NameTableCellRenderer() {
			super();
		}

		private ImageIcon getIcon(String iconName) {
			ImageIcon icon = iconByName.get(iconName);
			if (icon == null) {
				icon = ImageIconCache.getScaledImageIcon(iconName, 16, 16);
				iconByName.put(iconName, icon);
			}
			return icon;
		}

		@Override
		public Component getTableCellRendererComponent(JTable jtable, Object value,
				boolean isSelected, boolean hasFocus, int row, int column) {
			final MTable<CounterRequest> myTable = getTable();
			final CounterRequest counterRequest = myTable.getList().get(
					myTable.convertRowIndexToModel(row));
			final Counter counter = getCounterByRequestId(counterRequest);
			if (counter != null && counter.getIconName() != null) {
				final Icon icon = getIcon(counter.getIconName());
				setIcon(icon);
			} else {
				setIcon(null);
			}
			return super.getTableCellRendererComponent(jtable, value, isSelected, hasFocus, row,
					column);
		}
	}

	CounterRequestUsagesPanel(RemoteCollector remoteCollector, CounterRequest request, Range range)
			throws IOException {
		super(remoteCollector, range);

		final String graphLabel = truncate(
				I18N.getString("Utilisations_de") + ' ' + request.getName(), 50);
		setName(graphLabel);

		final JLabel label = new JLabel(' ' + I18N.getString("Utilisations_de") + ' '
				+ request.getName());
		label.setBorder(BorderFactory.createEmptyBorder(10, 0, 5, 0));
		label.setFont(label.getFont().deriveFont(Font.BOLD));
		add(label, BorderLayout.NORTH);

		final MTableScrollPane<CounterRequest> scrollPane = createScrollPane();
		final List<CounterRequest> requests = new ArrayList<CounterRequest>();
		for (final Counter counter : getCounters()) {
			for (final CounterRequest counterRequest : counter.getOrderedRequests()) {
				if (counterRequest.containsChildRequest(request.getId())) {
					requests.add(counterRequest);
				}
			}
		}

		getTable().setList(requests);
		add(scrollPane, BorderLayout.CENTER);

		add(createButtonsPanel(), BorderLayout.SOUTH);
	}

	private MTableScrollPane<CounterRequest> createScrollPane() {
		final MTable<CounterRequest> table = getTable();
		final MTableScrollPane<CounterRequest> tableScrollPane = new MTableScrollPane<CounterRequest>(
				table);

		table.addColumn("name", I18N.getString("Requete"));
		table.setColumnCellRenderer("name", new NameTableCellRenderer());

		return tableScrollPane;
	}

	private static String truncate(String string, int maxLength) {
		return string.substring(0, Math.min(string.length(), maxLength));
	}
}
