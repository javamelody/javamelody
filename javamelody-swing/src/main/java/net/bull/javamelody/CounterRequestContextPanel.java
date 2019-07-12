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
import java.awt.Desktop;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import javax.swing.JLabel;
import javax.swing.JPanel;

import net.bull.javamelody.internal.common.I18N;
import net.bull.javamelody.internal.common.Parameters;
import net.bull.javamelody.internal.model.Action;
import net.bull.javamelody.internal.model.Collector;
import net.bull.javamelody.internal.model.Counter;
import net.bull.javamelody.internal.model.CounterRequest;
import net.bull.javamelody.internal.model.CounterRequestContext;
import net.bull.javamelody.internal.model.CounterRequestContextData;
import net.bull.javamelody.internal.model.JavaInformations;
import net.bull.javamelody.internal.model.RemoteCollector;
import net.bull.javamelody.internal.model.ThreadInformations;
import net.bull.javamelody.internal.web.pdf.PdfOtherReport;
import net.bull.javamelody.swing.MButton;
import net.bull.javamelody.swing.Utilities;
import net.bull.javamelody.swing.table.MTableScrollPane;

/**
 * Panel du tableau des requêtes courantes.
 * @author Emeric Vernat
 */
class CounterRequestContextPanel extends CounterRequestAbstractPanel {
	private static final long serialVersionUID = 1L;

	private final JPanel buttonsPanel;

	private final JavaInformations javaInformations;

	CounterRequestContextPanel(RemoteCollector remoteCollector,
			List<CounterRequestContext> currentRequests, JavaInformations javaInformations) {
		super(remoteCollector, new CounterRequestForContextTable(remoteCollector));
		this.javaInformations = javaInformations;

		final CounterRequestForContextTable myTable = (CounterRequestForContextTable) getTable();
		myTable.init(
				new CounterRequestContextData(getCounters(), currentRequests, javaInformations));

		final MTableScrollPane<CounterRequest> scrollPane = new MTableScrollPane<>(getTable());

		Utilities.adjustTableHeight(getTable());
		add(scrollPane, BorderLayout.CENTER);

		this.buttonsPanel = createButtonsPanel(false);
		if (Parameters.isSystemActionsEnabled()) {
			final MButton killThreadButton = myTable.createKillThreadButton(this);
			buttonsPanel.add(killThreadButton);
		}

		add(buttonsPanel, BorderLayout.SOUTH);
	}

	CounterRequestContextPanel createDetailsPanel(final List<CounterRequestContext> currentRequests,
			MButton detailsButton) {
		addCurrentRequestsCount(currentRequests.size());

		final MButton pdfButton = createPdfButton();
		pdfButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				try {
					actionPdf(currentRequests);
				} catch (final IOException ex) {
					showException(ex);
				}
			}
		});
		buttonsPanel.add(pdfButton);

		buttonsPanel.add(detailsButton);

		return new CounterRequestContextPanel(getRemoteCollector(), currentRequests,
				javaInformations);
	}

	void addCurrentRequestsCount(int currentRequestsSize) {
		final DecimalFormat integerFormat = I18N.createIntegerFormat();
		final String text = getFormattedString("nb_requete_en_cours",
				integerFormat.format(currentRequestsSize)) + "     ";
		buttonsPanel.add(new JLabel(text), 0);
	}

	final void actionPdf(List<CounterRequestContext> rootContexts) throws IOException {
		final File tempFile = createTempFileForPdf();
		final PdfOtherReport pdfOtherReport = createPdfOtherReport(tempFile);
		try {
			// rq : on prend les rootContexts en argument, car data.getRootContexts() ne contient ici que le 1er
			final Map<JavaInformations, List<CounterRequestContext>> currentRequests = Collections
					.singletonMap(javaInformations, rootContexts);
			writeAllCurrentRequestsAsPart(pdfOtherReport, currentRequests, getCollector());
		} finally {
			pdfOtherReport.close();
		}
		Desktop.getDesktop().open(tempFile);
	}

	final void actionKillThread(ThreadInformations threadInformations) {
		if (threadInformations != null && confirm(
				getFormattedString("confirm_kill_thread", threadInformations.getName()))) {
			try {
				final String message = getRemoteCollector().executeActionAndCollectData(
						Action.KILL_THREAD, null, null, threadInformations.getGlobalThreadId(),
						null, null);
				showMessage(message);
				MainPanel.refreshMainTabFromChild(this);
			} catch (final IOException ex) {
				showException(ex);
			}
		}
	}

	static void writeAllCurrentRequestsAsPart(PdfOtherReport pdfOtherReport,
			Map<JavaInformations, List<CounterRequestContext>> currentRequests, Collector collector)
			throws IOException {
		long timeOfSnapshot = System.currentTimeMillis();
		final List<Counter> counters = collector.getCounters();
		final Map<JavaInformations, List<CounterRequestContext>> allCurrentRequests = new LinkedHashMap<>();
		for (final Map.Entry<JavaInformations, List<CounterRequestContext>> entry : currentRequests
				.entrySet()) {
			final JavaInformations javaInformations = entry.getKey();
			// on clone les contextes car les parentCounters seront remplacés
			final List<CounterRequestContext> rootContexts = entry.getValue();
			if (!rootContexts.isEmpty()) {
				// s'il existe une requête en cours, on récupère l'heure du snapshot plutôt que l'heure courante,
				// sinon les durées écoulées sont surévaluées pour les requêtes terminées désormais
				timeOfSnapshot = rootContexts.get(0).getParentCounter().getStartDate().getTime();
			}
			final List<CounterRequestContext> contexts = new ArrayList<>(rootContexts.size());
			for (final CounterRequestContext context : rootContexts) {
				contexts.add(context.clone());
			}
			allCurrentRequests.put(javaInformations, contexts);
		}

		pdfOtherReport.writeAllCurrentRequestsAsPart(allCurrentRequests, collector, counters,
				timeOfSnapshot);
	}
}
