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
import java.awt.Color;
import java.awt.Font;
import java.io.IOException;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;

/**
 * Panel du détail d'une requête.
 * @author Emeric Vernat
 */
class CounterRequestDetailPanel extends MelodyPanel {
	private static final long serialVersionUID = 1L;

	CounterRequestDetailPanel(RemoteCollector remoteCollector, CounterRequest request,
			Counter parentCounter, Range range) throws IOException {
		super(remoteCollector);

		final String graphName = request.getId();
		final String graphLabel = truncate(request.getName(), 50);
		setName(graphLabel);

		final CounterRequestDetailTablePanel counterRequestDetailTablePanel = new CounterRequestDetailTablePanel(
				remoteCollector, request, range);
		add(counterRequestDetailTablePanel, BorderLayout.NORTH);

		if (isRequestGraphDisplayed(parentCounter)) {
			final ChartPanel chartPanel = new ChartPanel(remoteCollector, graphName, graphLabel);
			add(chartPanel, BorderLayout.CENTER);
		}

		if (JdbcWrapper.SINGLETON.getSqlCounter().isRequestIdFromThisCounter(graphName)
				&& !request.getName().toLowerCase().startsWith("alter ")) {
			// inutile d'essayer d'avoir le plan d'exécution des requêtes sql
			// telles que "alter session set ..." (cf issue 152)
			final String sqlRequestExplainPlan = remoteCollector
					.collectSqlRequestExplainPlan(request.getName());
			if (sqlRequestExplainPlan != null) {
				final JPanel panel = createSqlRequestExplainPlanPanel(sqlRequestExplainPlan);
				add(panel, BorderLayout.SOUTH);
			}
		}
	}

	private JPanel createSqlRequestExplainPlanPanel(String sqlRequestExplainPlan) {
		final JTextArea textArea = new JTextArea();
		textArea.setFont(new Font(Font.MONOSPACED, Font.PLAIN, textArea.getFont().getSize() - 1));
		textArea.setEditable(false);
		textArea.setCaretPosition(0);
		// background nécessaire avec la plupart des look and feels dont Nimbus,
		// sinon il reste blanc malgré editable false
		textArea.setBackground(Color.decode("#E6E6E6"));
		textArea.setText(sqlRequestExplainPlan);
		final JPanel panel = new JPanel(new BorderLayout());
		panel.setOpaque(false);
		final JLabel label = new JLabel(I18N.getString("Plan_d_execution"));
		label.setFont(label.getFont().deriveFont(Font.BOLD));
		panel.add(label, BorderLayout.NORTH);
		final JScrollPane scrollPane = new JScrollPane(textArea);
		panel.add(scrollPane, BorderLayout.CENTER);
		return panel;
	}

	static boolean isRequestGraphDisplayed(Counter parentCounter) {
		return !(parentCounter.isErrorCounter() && !parentCounter.isJobCounter())
				&& !parentCounter.isJspOrStrutsCounter();
	}

	private static String truncate(String string, int maxLength) {
		return string.substring(0, Math.min(string.length(), maxLength));
	}
}
