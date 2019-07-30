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
import java.awt.Color;
import java.awt.Font;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import java.text.DecimalFormat;
import java.util.Locale;

import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.ScrollPaneConstants;

import net.bull.javamelody.internal.common.I18N;
import net.bull.javamelody.internal.model.Counter;
import net.bull.javamelody.internal.model.CounterRequest;
import net.bull.javamelody.internal.model.CounterRequestRumData;
import net.bull.javamelody.internal.model.RemoteCollector;
import net.bull.javamelody.swing.MButton;

/**
 * Panel du détail d'une requête.
 * @author Emeric Vernat
 */
class CounterRequestDetailPanel extends MelodyPanel {
	private static final long serialVersionUID = 1L;

	private final CounterRequest request;

	CounterRequestDetailPanel(RemoteCollector remoteCollector, CounterRequest request)
			throws IOException {
		super(remoteCollector);
		assert request != null;
		this.request = request;

		refresh();
	}

	final void refresh() throws IOException {
		removeAll();

		final String graphName = request.getId();
		final String graphLabel = truncate(request.getName(), 50);
		setName(graphLabel);

		final JPanel panel = new JPanel(new BorderLayout());
		final JScrollPane scrollPane = new JScrollPane(panel);
		scrollPane.getVerticalScrollBar().setFocusable(false);
		scrollPane.getHorizontalScrollBar().setFocusable(false);
		scrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);

		final JPanel northPanel = new JPanel(new BorderLayout());
		if (request.getRumData() != null && request.getRumData().getHits() != 0) {
			final JPanel requestRumDataPanel = createRequestRumDataPanel();
			requestRumDataPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 0, 0));
			northPanel.add(requestRumDataPanel, BorderLayout.WEST);
		}
		final RemoteCollector remoteCollector = getRemoteCollector();
		final CounterRequestDetailTablePanel counterRequestDetailTablePanel = new CounterRequestDetailTablePanel(
				remoteCollector, request);
		counterRequestDetailTablePanel.setBorder(BorderFactory.createEmptyBorder(5, 0, 0, 0));
		northPanel.add(counterRequestDetailTablePanel, BorderLayout.SOUTH);
		panel.add(northPanel, BorderLayout.NORTH);

		if (CounterRequestTable.isRequestGraphDisplayed(getCounterByRequestId(request))) {
			final MButton refreshButton = createRefreshButton();
			refreshButton.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					try {
						getRemoteCollector().collectDataIncludingCurrentRequests();
						refresh();
					} catch (final IOException ex) {
						showException(ex);
					}
				}
			});
			final ChartPanel chartPanel = new ChartPanel(remoteCollector, graphName, graphLabel,
					refreshButton);
			panel.add(chartPanel, BorderLayout.CENTER);
		}

		if (JdbcWrapper.SINGLETON.getSqlCounter().isRequestIdFromThisCounter(graphName)
				&& !request.getName().toLowerCase(Locale.US).startsWith("alter ")) {
			// inutile d'essayer d'avoir le plan d'exécution des requêtes sql
			// telles que "alter session set ..." (cf issue 152)
			final String sqlRequestExplainPlan = remoteCollector
					.collectSqlRequestExplainPlan(request.getName());
			if (sqlRequestExplainPlan != null) {
				final JPanel planPanel = createSqlRequestExplainPlanPanel(sqlRequestExplainPlan);
				panel.add(planPanel, BorderLayout.SOUTH);
			}
		} else if (request.getStackTrace() != null) {
			// il n'est pas actuellement possible qu'il y ait à la fois un plan d'exécution et une stack-trace
			final JPanel stackTracePanel = createStackTracePanel(request.getStackTrace());
			panel.add(stackTracePanel, BorderLayout.SOUTH);
		}

		add(scrollPane);
	}

	private JPanel createRequestRumDataPanel() {
		final JPanel rumDataPanel = new JPanel(new GridLayout(1, 4, 40, 0));
		final CounterRequestRumData rumData = request.getRumData();
		final DecimalFormat percentFormat = I18N.createPercentFormat();
		final DecimalFormat integerFormat = I18N.createIntegerFormat();
		final int networkTimeMean = rumData.getNetworkTimeMean();
		final int serverMean = request.getMean();
		final int domProcessingMean = rumData.getDomProcessingMean();
		final int pageRenderingMean = rumData.getPageRenderingMean();
		final int total = networkTimeMean + serverMean + domProcessingMean + pageRenderingMean;
		final double networkPercent = 100d * networkTimeMean / total;
		final double serverPercent = 100d * serverMean / total;
		final double domProcessingPercent = 100d * domProcessingMean / total;
		final double pageRenderingPercent = 100d * pageRenderingMean / total;

		final String networkLabel = I18N.getString("Network") + ": "
				+ integerFormat.format(networkTimeMean) + " ms ("
				+ percentFormat.format(networkPercent) + "%)";
		final String serverLabel = I18N.getString("Server") + " : "
				+ integerFormat.format(serverMean) + " ms (" + percentFormat.format(serverPercent)
				+ "%)";
		final String domProcessingLabel = I18N.getString("DOM_processing") + " : "
				+ integerFormat.format(domProcessingMean) + " ms ("
				+ percentFormat.format(domProcessingPercent) + "%)";
		final String pageRenderingLabel = I18N.getString("Page_rendering") + " : "
				+ integerFormat.format(pageRenderingMean) + " ms ("
				+ percentFormat.format(pageRenderingPercent) + "%)";
		rumDataPanel.add(new JLabel(networkLabel));
		rumDataPanel.add(new JLabel(serverLabel));
		rumDataPanel.add(new JLabel(domProcessingLabel));
		rumDataPanel.add(new JLabel(pageRenderingLabel));
		return rumDataPanel;
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
		final JLabel label = new JLabel(getString("Plan_d_execution"));
		label.setFont(label.getFont().deriveFont(Font.BOLD));
		panel.add(label, BorderLayout.NORTH);
		final JScrollPane scrollPane = new JScrollPane(textArea);
		panel.add(scrollPane, BorderLayout.CENTER);
		return panel;
	}

	private JPanel createStackTracePanel(String stackTrace) {
		final JTextArea textArea = new JTextArea();
		textArea.setFont(textArea.getFont().deriveFont((float) textArea.getFont().getSize() - 1));
		textArea.setEditable(false);
		textArea.setCaretPosition(0);
		// background nécessaire avec la plupart des look and feels dont Nimbus,
		// sinon il reste blanc malgré editable false
		textArea.setBackground(Color.decode("#E6E6E6"));
		textArea.setText(stackTrace);
		final JPanel panel = new JPanel(new BorderLayout());
		panel.setOpaque(false);
		final JLabel label = new JLabel("Stack-trace");
		label.setFont(label.getFont().deriveFont(Font.BOLD));
		panel.add(label, BorderLayout.NORTH);
		final JScrollPane scrollPane = new JScrollPane(textArea);
		panel.add(scrollPane, BorderLayout.CENTER);
		return panel;
	}

	private Counter getCounterByRequestId(CounterRequest counterRequest) {
		return getCollector().getCounterByRequestId(counterRequest);
	}

	private static String truncate(String string, int maxLength) {
		return string.substring(0, Math.min(string.length(), maxLength));
	}
}
