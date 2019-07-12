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
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.IOException;
import java.text.DateFormat;
import java.util.Date;
import java.util.List;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTable;
import javax.swing.SwingConstants;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.TableColumn;

import net.bull.javamelody.internal.common.I18N;
import net.bull.javamelody.internal.common.Parameters;
import net.bull.javamelody.internal.model.Action;
import net.bull.javamelody.internal.model.Counter;
import net.bull.javamelody.internal.model.CounterRequest;
import net.bull.javamelody.internal.model.JobInformations;
import net.bull.javamelody.internal.model.RemoteCollector;
import net.bull.javamelody.swing.MButton;
import net.bull.javamelody.swing.MHyperLink;
import net.bull.javamelody.swing.Utilities;
import net.bull.javamelody.swing.table.MDateTableCellRenderer;
import net.bull.javamelody.swing.table.MDefaultTableCellRenderer;
import net.bull.javamelody.swing.table.MTable;
import net.bull.javamelody.swing.table.MTableScrollPane;

/**
 * Panel des jobs (Quartz).
 * @author Emeric Vernat
 */
class JobInformationsPanel extends MelodyPanel {
	private static final ImageIcon RESUME_ICON = ImageIconCache
			.getScaledImageIcon("control_play_blue.png", 18, 18);
	private static final ImageIcon PAUSE_ICON = ImageIconCache
			.getScaledImageIcon("control_pause_blue.png", 18, 18);
	private static final long serialVersionUID = 1L;

	private final Counter jobCounter;
	private final MTable<JobInformations> table;

	private final class MeanTimeTableCellRenderer extends MDateTableCellRenderer {
		private static final long serialVersionUID = 1L;

		MeanTimeTableCellRenderer() {
			super();
			setDateFormat(I18N.createDurationFormat());
		}

		@Override
		public Component getTableCellRendererComponent(JTable jtable, Object value,
				boolean isSelected, boolean hasFocus, int row, int column) {
			// durée moyenne
			final Date date;
			if (row == -1) {
				date = null;
			} else {
				final MTable<JobInformations> myTable = getTable();
				final JobInformations jobInformations = myTable.getList()
						.get(myTable.convertRowIndexToModel(row));
				final CounterRequest counterRequest = getCounterRequest(jobInformations);
				if (counterRequest.getMean() >= 0) {
					date = new Date(1L * counterRequest.getMean());
				} else {
					date = null;
				}
			}
			return super.getTableCellRendererComponent(jtable, date, isSelected, hasFocus, row,
					column);
		}
	}

	private final class ElapsedTimeTableCellRenderer extends MDateTableCellRenderer {
		private static final long serialVersionUID = 1L;

		ElapsedTimeTableCellRenderer() {
			super();
			setDateFormat(I18N.createDurationFormat());
			setHorizontalTextPosition(LEFT);
			// pas pratique pour la hauteur même avec adjustRowHeight:
			//			setVerticalTextPosition(TOP);
			//			setHorizontalTextPosition(CENTER);
		}

		@Override
		public Component getTableCellRendererComponent(JTable jtable, Object value,
				boolean isSelected, boolean hasFocus, int row, int column) {
			// durée écoulée et barre de progression
			final Date date;
			if (row == -1) {
				date = null;
				setIcon(null);
				setToolTipText(null);
			} else {
				final MTable<JobInformations> myTable = getTable();
				final JobInformations jobInformations = myTable.getList()
						.get(myTable.convertRowIndexToModel(row));
				final long elapsedTime = jobInformations.getElapsedTime();
				if (elapsedTime >= 0) {
					date = new Date(elapsedTime);
					final CounterRequest counterRequest = getCounterRequest(jobInformations);
					final JLabel barLabel = toBar(counterRequest.getMean(), elapsedTime);
					setIcon(barLabel.getIcon());
					setToolTipText(barLabel.getToolTipText());
				} else {
					date = null;
					setIcon(null);
					setToolTipText(null);
				}
			}
			return super.getTableCellRendererComponent(jtable, date, isSelected, hasFocus, row,
					column);
		}
	}

	private class NameTableCellRenderer extends MDefaultTableCellRenderer {
		private static final long serialVersionUID = 1L;

		NameTableCellRenderer() {
			super();
		}

		@Override
		public Component getTableCellRendererComponent(JTable jtable, Object value,
				boolean isSelected, boolean hasFocus, int row, int column) {
			// tooltip selon la description
			if (row == -1) {
				setToolTipText(null);
			} else {
				final MTable<JobInformations> myTable = getTable();
				final JobInformations jobInformations = myTable.getList()
						.get(myTable.convertRowIndexToModel(row));
				final String description = jobInformations.getDescription();
				if (description != null) {
					setToolTipText("<html>" + description.replaceAll("\n", "<br/>"));
				} else {
					setToolTipText(null);
				}
			}
			// et texte selon la valeur (nom du job)
			return super.getTableCellRendererComponent(jtable, value, isSelected, hasFocus, row,
					column);
		}
	}

	private final class StackTraceTableCellRenderer extends MDefaultTableCellRenderer {
		private static final long serialVersionUID = 1L;

		StackTraceTableCellRenderer() {
			super();
			setHorizontalAlignment(SwingConstants.CENTER);
		}

		@Override
		public Component getTableCellRendererComponent(JTable jtable, Object value,
				boolean isSelected, boolean hasFocus, int row, int column) {
			// tooltip et icône selon la stackTrace
			if (row == -1) {
				setToolTipText(null);
				setIcon(null);
			} else {
				final MTable<JobInformations> myTable = getTable();
				final JobInformations jobInformations = myTable.getList()
						.get(myTable.convertRowIndexToModel(row));
				final CounterRequest counterRequest = getCounterRequest(jobInformations);
				final String stackTrace = counterRequest.getStackTrace();
				if (stackTrace == null) {
					setToolTipText(getString("JobWithoutLastException"));
					setIcon(ImageIconCache.getImageIcon("bullets/green.png"));
				} else {
					setIcon(ImageIconCache.getImageIcon("bullets/red.png"));
					setToolTipText("<html>" + stackTrace.replace("[See nested", "\n[See nested")
							.replaceAll("\n", "<br/>"));
				}
			}
			// sans texte
			return super.getTableCellRendererComponent(jtable, null, isSelected, hasFocus, row,
					column);
		}
	}

	private final class RepeatIntervalTableCellRenderer extends MDefaultTableCellRenderer {
		private static final long serialVersionUID = 1L;
		private static final long ONE_DAY_MILLIS = 24L * 60 * 60 * 1000;
		private final DateFormat dateFormat = I18N.createDurationFormat();

		RepeatIntervalTableCellRenderer() {
			super();
			setHorizontalAlignment(SwingConstants.RIGHT);
		}

		@Override
		public Component getTableCellRendererComponent(JTable jtable, Object value,
				boolean isSelected, boolean hasFocus, int row, int column) {
			// texte selon repeatInterval et cronExpression
			final String text;
			if (row == -1) {
				text = null;
			} else {
				final MTable<JobInformations> myTable = getTable();
				final JobInformations jobInformations = myTable.getList()
						.get(myTable.convertRowIndexToModel(row));
				// on n'affiche pas la période si >= 1 jour car ce formateur ne saurait pas l'afficher
				if (jobInformations.getRepeatInterval() > 0
						&& jobInformations.getRepeatInterval() < ONE_DAY_MILLIS) {
					text = dateFormat.format(new Date(jobInformations.getRepeatInterval()));
				} else if (jobInformations.getCronExpression() != null) {
					text = jobInformations.getCronExpression();
				} else {
					text = null;
				}
			}
			return super.getTableCellRendererComponent(jtable, text, isSelected, hasFocus, row,
					column);
		}
	}

	JobInformationsPanel(RemoteCollector remoteCollector, List<JobInformations> jobInformationsList,
			Counter rangeJobCounter) {
		super(remoteCollector);
		assert jobInformationsList != null;
		assert rangeJobCounter != null;
		this.jobCounter = rangeJobCounter;

		final MTableScrollPane<JobInformations> scrollPane = createScrollPane();
		this.table = scrollPane.getTable();
		table.setList(jobInformationsList);
		Utilities.adjustTableHeight(table);

		add(scrollPane, BorderLayout.NORTH);

		final MHyperLink hyperLink = new MHyperLink(" Configuration reference",
				"http://www.quartz-scheduler.org/docs/index.html");
		add(hyperLink, BorderLayout.WEST);

		if (Parameters.isSystemActionsEnabled()) {
			final JPanel buttonsPanel = createButtonsPanel();
			add(buttonsPanel, BorderLayout.EAST);
		}
	}

	private MTableScrollPane<JobInformations> createScrollPane() {
		final MTableScrollPane<JobInformations> tableScrollPane = new MTableScrollPane<>();
		final MTable<JobInformations> myTable = tableScrollPane.getTable();
		myTable.addColumn("group", getString("JobGroup"));
		myTable.addColumn("name", getString("JobName"));
		myTable.addColumn("jobClassName", getString("JobClassName"));
		final TableColumn stackTraceTableColumn = new TableColumn(myTable.getColumnCount());
		stackTraceTableColumn.setIdentifier(myTable.getColumnCount());
		stackTraceTableColumn.setHeaderValue(getString("JobLastException"));
		myTable.addColumn(stackTraceTableColumn);
		final TableColumn meanTimeTableColumn = new TableColumn(myTable.getColumnCount());
		meanTimeTableColumn.setIdentifier(myTable.getColumnCount());
		meanTimeTableColumn.setHeaderValue(getString("JobMeanTime"));
		myTable.addColumn(meanTimeTableColumn);
		myTable.addColumn("elapsedTime", getString("JobElapsedTime"));
		myTable.addColumn("previousFireTime", getString("JobPreviousFireTime"));
		myTable.addColumn("nextFireTime", getString("JobNextFireTime"));
		myTable.addColumn("repeatInterval", getString("JobPeriodOrCronExpression"));
		myTable.addColumn("paused", getString("JobPaused"));

		myTable.setColumnCellRenderer("name", new NameTableCellRenderer());
		stackTraceTableColumn.setCellRenderer(new StackTraceTableCellRenderer());
		meanTimeTableColumn.setCellRenderer(new MeanTimeTableCellRenderer());
		myTable.setColumnCellRenderer("elapsedTime", new ElapsedTimeTableCellRenderer());

		final MDateTableCellRenderer fireTimeTableCellRenderer = new MDateTableCellRenderer();
		fireTimeTableCellRenderer.setDateFormat(I18N.createDateAndTimeFormat());
		myTable.setColumnCellRenderer("previousFireTime", fireTimeTableCellRenderer);
		myTable.setColumnCellRenderer("nextFireTime", fireTimeTableCellRenderer);
		myTable.setColumnCellRenderer("repeatInterval", new RepeatIntervalTableCellRenderer());
		// rq: on n'affiche pas le maximum, l'écart-type ou le pourcentage d'erreurs,
		// uniquement car cela ferait trop de colonnes dans la page

		myTable.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseClicked(MouseEvent e) {
				if (e.getClickCount() == 2) {
					final JobInformations jobInformations = getTable().getSelectedObject();
					final CounterRequest counterRequest = getCounterRequest(jobInformations);
					if (counterRequest.getStackTrace() != null) {
						Utilities.showTextInPopup(JobInformationsPanel.this,
								counterRequest.getName(), counterRequest.getStackTrace());
					}
				}
			}
		});

		return tableScrollPane;
	}

	final CounterRequest getCounterRequest(JobInformations jobInformations) {
		final String jobFullName = jobInformations.getGroup() + '.' + jobInformations.getName();
		// rq: la méthode getCounterRequestByName prend en compte l'éventuelle utilisation du paramètre
		// job-transform-pattern qui peut faire que jobFullName != counterRequest.getName()
		final CounterRequest result = jobCounter.getCounterRequestByName(jobFullName, true);
		// getCounterRequestByName ne peut pas retourner null actuellement
		assert result != null;
		return result;
	}

	private JPanel createButtonsPanel() {
		final MButton pauseJobButton = createPauseJobButton(PAUSE_ICON);
		final MButton resumeJobButton = createResumeJobButton(RESUME_ICON);
		final MButton pauseAllJobsButton = createPauseAllJobsButton(PAUSE_ICON);
		final MButton resumeAllJobsButton = createResumeAllJobsButton(RESUME_ICON);

		getTable().getSelectionModel().addListSelectionListener(new ListSelectionListener() {
			@Override
			public void valueChanged(ListSelectionEvent e) {
				final JobInformations jobInformations = getTable().getSelectedObject();
				pauseJobButton.setEnabled(jobInformations != null);
				resumeJobButton.setEnabled(jobInformations != null);
			}
		});
		pauseJobButton.setEnabled(getTable().getSelectedObject() != null);
		resumeJobButton.setEnabled(getTable().getSelectedObject() != null);
		return Utilities.createButtonsPanel(pauseJobButton, resumeJobButton, pauseAllJobsButton,
				resumeAllJobsButton);
	}

	private MButton createPauseJobButton(final Icon pauseIcon) {
		final MButton pauseJobButton = new MButton(getString("Pause_job"), pauseIcon);
		pauseJobButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				if (confirm(getString("confirm_pause_job"))) {
					final JobInformations jobInformations = getTable().getSelectedObject();
					actionOnJob(Action.PAUSE_JOB, jobInformations.getGlobalJobId());
				}
			}
		});
		return pauseJobButton;
	}

	private MButton createResumeJobButton(final Icon resumeIcon) {
		final MButton resumeJobButton = new MButton(getString("Resume_job"), resumeIcon);
		resumeJobButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				if (confirm(getString("confirm_resume_job"))) {
					final JobInformations jobInformations = getTable().getSelectedObject();
					actionOnJob(Action.RESUME_JOB, jobInformations.getGlobalJobId());
				}
			}
		});
		return resumeJobButton;
	}

	private MButton createPauseAllJobsButton(final Icon pauseIcon) {
		final MButton pauseAllJobsButton = new MButton(getString("Pause_all_jobs"), pauseIcon);
		pauseAllJobsButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				if (confirm(getString("confirm_pause_all_jobs"))) {
					actionOnJob(Action.PAUSE_JOB, "all");
				}
			}
		});
		return pauseAllJobsButton;
	}

	private MButton createResumeAllJobsButton(final Icon resumeIcon) {
		final MButton resumeAllJobsButton = new MButton(getString("Resume_all_jobs"), resumeIcon);
		resumeAllJobsButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				if (confirm(getString("confirm_resume_all_jobs"))) {
					actionOnJob(Action.RESUME_JOB, "all");
				}
			}
		});
		return resumeAllJobsButton;
	}

	static JLabel toBar(int mean, long elapsedTime) {
		return JavaInformationsPanel.toBar("", 100d * elapsedTime / mean);
	}

	final MTable<JobInformations> getTable() {
		return table;
	}

	final void actionOnJob(Action action, String jobId) {
		try {
			final String message = getRemoteCollector().executeActionAndCollectData(action, null,
					null, null, jobId, null);
			showMessage(message);
			MainPanel.refreshMainTabFromChild(this);
		} catch (final IOException ex) {
			showException(ex);
		}
	}
}
