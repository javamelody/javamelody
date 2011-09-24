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

import java.awt.Color;
import java.awt.Component;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;

import net.bull.javamelody.swing.MButton;
import net.bull.javamelody.swing.Utilities;

/**
 * Panel principal.
 * @author Emeric Vernat
 */
class ScrollingPanel extends MelodyPanel {
	static final ImageIcon PLUS_ICON = ImageIconCache.getImageIcon("bullets/plus.png");
	static final ImageIcon MINUS_ICON = ImageIconCache.getImageIcon("bullets/minus.png");
	private static final String DETAILS_KEY = "Details";

	private static final long serialVersionUID = 1L;

	@SuppressWarnings("all")
	private final Collector collector;
	@SuppressWarnings("all")
	private final List<JavaInformations> javaInformationsList;
	private final URL monitoringUrl;
	// TODO range selon sélection (jour par défaut)
	private final Range range = Period.TOUT.getRange();

	ScrollingPanel(RemoteCollector remoteCollector, URL monitoringUrl) throws IOException {
		super(remoteCollector);
		this.collector = remoteCollector.getCollector();
		this.javaInformationsList = remoteCollector.getJavaInformationsList();
		this.monitoringUrl = monitoringUrl;

		setBorder(BorderFactory.createEmptyBorder(0, 3, 0, 3));
		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		setOpaque(true);
		setBackground(Color.decode("#E6E6E6"));
		add(new ChartsPanel());

		addCounters();

		addSystemInformations();

		addThreadInformations();

		if (isJobEnabled()) {
			addParagraphTitle(I18N.getString("Jobs"), "jobs.png");
			final Counter rangeJobCounter = collector.getRangeCounter(range,
					Counter.JOB_COUNTER_NAME);
			addJobs(rangeJobCounter);
			addCounter(rangeJobCounter);
		}

		if (isCacheEnabled()) {
			addParagraphTitle("Caches", "caches.png");
			addCaches();
		}

		// TODO
		//		writeMessageIfNotNull(message, null, anchorNameForRedirect);
		//		writeDurationAndOverhead();

		add(new JLabel(" "));
		add(new JLabel(" "));

		for (final Component component : getComponents()) {
			((JComponent) component).setAlignmentX(Component.LEFT_ALIGNMENT);
		}
	}

	private void addCounters() throws IOException {
		final List<Counter> counters = collector.getRangeCountersToBeDisplayed(range);
		for (final Counter counter : counters) {
			addCounter(counter);
		}
		if (range.getPeriod() == Period.TOUT && counters.size() > 1) {
			final MButton clearAllCountersButton = new MButton(
					I18N.getString("Reinitialiser_toutes_stats"));
			clearAllCountersButton.setToolTipText(I18N.getString("Vider_toutes_stats"));
			clearAllCountersButton.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					if (confirm(I18N.getString("confirm_vider_toutes_stats"))) {
						actionClearAllCounters();
					}
				}
			});
			final JPanel clearAllCountersPanel = Utilities
					.createButtonsPanel(clearAllCountersButton);
			add(clearAllCountersPanel);
		}
	}

	final void actionClearAllCounters() {
		try {
			final String message = getRemoteCollector().executeActionAndCollectData(
					Action.CLEAR_COUNTER, "all", null, null, null);
			showMessage(message);
			MainPanel.refreshMainTabFromChild(this);
		} catch (final IOException ex) {
			showException(ex);
		}
	}

	private void addCounter(Counter counter) {
		final String counterLabel = I18N.getString(counter.getName() + "Label");
		addParagraphTitle(I18N.getFormattedString("Statistiques_compteur", counterLabel) + " - "
				+ range.getLabel(), counter.getIconName());
		final StatisticsPanel statisticsPanel = new StatisticsPanel(getRemoteCollector(), counter,
				range);
		statisticsPanel.showGlobalRequests();
		add(statisticsPanel);
	}

	private void addSystemInformations() {
		addParagraphTitle(I18N.getString("Informations_systemes"), "systeminfo.png");
		final List<JavaInformations> list = javaInformationsList;
		// TODO mettre propriété système system-actions-enabled dans jnlp
		if (Parameters.isSystemActionsEnabled()) {
			add(new SystemInformationsButtonsPanel(getRemoteCollector(), monitoringUrl));
		}

		final List<JavaInformationsPanel> javaInformationsPanelList = new ArrayList<JavaInformationsPanel>(
				list.size());
		final JPanel westJavaInformationsPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 5, 5));
		westJavaInformationsPanel.setOpaque(false);
		for (final JavaInformations javaInformations : list) {
			final JavaInformationsPanel javaInformationsPanel = new JavaInformationsPanel(
					getRemoteCollector(), javaInformations, monitoringUrl);
			javaInformationsPanel.showSummary();
			javaInformationsPanelList.add(javaInformationsPanel);
			westJavaInformationsPanel.add(javaInformationsPanel);
		}
		final MButton javaInformationsDetailsButton = new MButton(I18N.getString(DETAILS_KEY),
				PLUS_ICON);
		javaInformationsDetailsButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				final boolean repeatHost = list.size() > 1;
				for (final JavaInformationsPanel javaInformationsPanel : javaInformationsPanelList) {
					javaInformationsPanel.showDetails(repeatHost);
					javaInformationsPanel.validate();
				}
				if (javaInformationsDetailsButton.getIcon() == PLUS_ICON) {
					javaInformationsDetailsButton.setIcon(MINUS_ICON);
				} else {
					javaInformationsDetailsButton.setIcon(PLUS_ICON);
				}
			}
		});
		westJavaInformationsPanel.add(javaInformationsDetailsButton);
		add(westJavaInformationsPanel);
		add(new JLabel(" "));
	}

	private void addThreadInformations() {
		addParagraphTitle(I18N.getString("Threads"), "threads.png");
		for (final JavaInformations javaInformations : javaInformationsList) {
			final List<ThreadInformations> threadInformationsList = javaInformations
					.getThreadInformationsList();
			final ThreadInformationsPanel threadInformationsPanel = new ThreadInformationsPanel(
					getRemoteCollector(), threadInformationsList,
					javaInformations.isStackTraceEnabled());
			threadInformationsPanel.setVisible(false);
			final JLabel summaryLabel = new JLabel("<html><b>"
					+ I18N.getFormattedString("Threads_sur", javaInformations.getHost())
					+ ": </b>"
					+ I18N.getFormattedString("thread_count", javaInformations.getThreadCount(),
							javaInformations.getPeakThreadCount(),
							javaInformations.getTotalStartedThreadCount()));
			final MButton detailsButton = new MButton(I18N.getString(DETAILS_KEY), PLUS_ICON);
			detailsButton.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					threadInformationsPanel.setVisible(!threadInformationsPanel.isVisible());
					validate();
					if (detailsButton.getIcon() == PLUS_ICON) {
						detailsButton.setIcon(MINUS_ICON);
					} else {
						detailsButton.setIcon(PLUS_ICON);
					}
				}
			});

			final JPanel flowPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 5, 0));
			flowPanel.setOpaque(false);
			flowPanel.add(summaryLabel);
			flowPanel.add(detailsButton);

			add(flowPanel);
			addThreadDeadlocks(threadInformationsList);
			add(threadInformationsPanel);
		}
	}

	private void addThreadDeadlocks(List<ThreadInformations> threadInformationsList) {
		final List<ThreadInformations> deadlockedThreads = new ArrayList<ThreadInformations>();
		for (final ThreadInformations thread : threadInformationsList) {
			if (thread.isDeadlocked()) {
				deadlockedThreads.add(thread);
			}
		}
		if (!deadlockedThreads.isEmpty()) {
			final StringBuilder sb = new StringBuilder();
			sb.append("  ");
			sb.append(I18N.getString("Threads_deadlocks"));
			String separator = " ";
			for (final ThreadInformations thread : deadlockedThreads) {
				sb.append(separator);
				sb.append(thread.getName());
				separator = ", ";
			}
			final JLabel label = new JLabel(sb.toString());
			label.setForeground(Color.RED);
			label.setFont(label.getFont().deriveFont(Font.BOLD));
			// séparateur avec composants au-dessus
			label.setBorder(BorderFactory.createEmptyBorder(10, 0, 0, 0));
			add(label);
		}
	}

	private void addCaches() {
		for (final JavaInformations javaInformations : javaInformationsList) {
			if (!javaInformations.isCacheEnabled()) {
				continue;
			}
			final List<CacheInformations> cacheInformationsList = javaInformations
					.getCacheInformationsList();
			final CacheInformationsPanel cacheInformationsPanel = new CacheInformationsPanel(
					getRemoteCollector(), cacheInformationsList);
			cacheInformationsPanel.setVisible(false);
			final JLabel summaryLabel = new JLabel("<html><b>"
					+ I18N.getFormattedString("caches_sur", cacheInformationsList.size(),
							javaInformations.getHost()) + "</b>");
			final MButton detailsButton = new MButton(I18N.getString(DETAILS_KEY), PLUS_ICON);
			detailsButton.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					cacheInformationsPanel.setVisible(!cacheInformationsPanel.isVisible());
					validate();
					if (detailsButton.getIcon() == PLUS_ICON) {
						detailsButton.setIcon(MINUS_ICON);
					} else {
						detailsButton.setIcon(PLUS_ICON);
					}
				}
			});

			final JPanel flowPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 5, 0));
			flowPanel.setOpaque(false);
			flowPanel.add(summaryLabel);
			flowPanel.add(detailsButton);

			add(flowPanel);
			add(cacheInformationsPanel);
		}
	}

	private void addJobs(Counter rangeJobCounter) {
		for (final JavaInformations javaInformations : javaInformationsList) {
			if (!javaInformations.isJobEnabled()) {
				continue;
			}
			final List<JobInformations> jobInformationsList = javaInformations
					.getJobInformationsList();
			final JobInformationsPanel jobInformationsPanel = new JobInformationsPanel(
					getRemoteCollector(), jobInformationsList, rangeJobCounter);
			jobInformationsPanel.setVisible(false);
			final JLabel summaryLabel = new JLabel("<html><b>"
					+ I18N.getFormattedString("jobs_sur", jobInformationsList.size(),
							javaInformations.getHost(),
							javaInformations.getCurrentlyExecutingJobCount()) + "</b>");
			final MButton detailsButton = new MButton(I18N.getString(DETAILS_KEY), PLUS_ICON);
			detailsButton.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					jobInformationsPanel.setVisible(!jobInformationsPanel.isVisible());
					validate();
					if (detailsButton.getIcon() == PLUS_ICON) {
						detailsButton.setIcon(MINUS_ICON);
					} else {
						detailsButton.setIcon(PLUS_ICON);
					}
				}
			});

			final JPanel flowPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 5, 0));
			flowPanel.setOpaque(false);
			flowPanel.add(summaryLabel);
			flowPanel.add(detailsButton);

			add(flowPanel);
			add(jobInformationsPanel);
		}
	}

	private boolean isCacheEnabled() {
		for (final JavaInformations javaInformations : javaInformationsList) {
			if (javaInformations.isCacheEnabled()) {
				return true;
			}
		}
		return false;
	}

	private boolean isJobEnabled() {
		for (final JavaInformations javaInformations : javaInformationsList) {
			if (javaInformations.isJobEnabled()) {
				return true;
			}
		}
		return false;
	}

	private void addParagraphTitle(String title, String iconName) {
		final JLabel label = Utilities.createParagraphTitle(title, iconName);
		add(label);
	}
}
