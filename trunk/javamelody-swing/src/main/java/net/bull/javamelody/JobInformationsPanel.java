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
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Desktop;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.IOException;
import java.net.URI;
import java.text.DateFormat;
import java.util.Date;
import java.util.List;

import javax.swing.Icon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTable;
import javax.swing.SwingConstants;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.TableColumn;

import net.bull.javamelody.swing.MButton;
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
	private static final long serialVersionUID = 1L;

	@SuppressWarnings("all")
	private final List<JobInformations> jobInformationsList;
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
				final JobInformations jobInformations = myTable.getList().get(
						myTable.convertRowIndexToModel(row));
				final CounterRequest counterRequest = getCounterRequest(jobInformations);
				if (counterRequest.getMean() >= 0) {
					date = new Date(counterRequest.getMean());
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
				final JobInformations jobInformations = myTable.getList().get(
						myTable.convertRowIndexToModel(row));
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
				final JobInformations jobInformations = myTable.getList().get(
						myTable.convertRowIndexToModel(row));
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
				final JobInformations jobInformations = myTable.getList().get(
						myTable.convertRowIndexToModel(row));
				final CounterRequest counterRequest = getCounterRequest(jobInformations);
				final String stackTrace = counterRequest.getStackTrace();
				if (stackTrace == null) {
					setToolTipText(I18N.getString("JobWithoutLastException"));
					setIcon(ImageIconCache.getImageIcon("bullets/green.png"));
				} else {
					setIcon(ImageIconCache.getImageIcon("bullets/red.png"));
					setToolTipText("<html>"
							+ stackTrace.replace("[See nested", "\n[See nested").replaceAll("\n",
									"<br/>"));
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
				final JobInformations jobInformations = myTable.getList().get(
						myTable.convertRowIndexToModel(row));
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

	JobInformationsPanel(RemoteCollector remoteCollector,
			List<JobInformations> jobInformationsList, Counter rangeJobCounter) {
		super(remoteCollector, new BorderLayout());
		assert jobInformationsList != null;
		assert rangeJobCounter != null;
		this.jobInformationsList = jobInformationsList;
		this.jobCounter = rangeJobCounter;
		this.table = new MTable<JobInformations>();

		addScrollPane();

		final JLabel label = new JLabel(" Configuration reference");
		label.setForeground(Color.BLUE.darker());
		label.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
		label.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseClicked(MouseEvent e) {
				try {
					Desktop.getDesktop().browse(
							new URI("http://www.quartz-scheduler.org/docs/index.html"));
				} catch (final Exception ex) {
					showException(ex);
				}
			}
		});
		add(label, BorderLayout.WEST);

		if (Parameters.isSystemActionsEnabled()) {
			addButtons();
		}
	}

	private void addScrollPane() {
		final MTableScrollPane<JobInformations> tableScrollPane = new MTableScrollPane<JobInformations>(
				table);
		table.addColumn("group", I18N.getString("JobGroup"));
		table.addColumn("name", I18N.getString("JobName"));
		table.addColumn("jobClassName", I18N.getString("JobClassName"));
		final TableColumn stackTraceTableColumn = new TableColumn(table.getColumnCount());
		stackTraceTableColumn.setIdentifier(table.getColumnCount());
		stackTraceTableColumn.setHeaderValue(I18N.getString("JobLastException"));
		table.addColumn(stackTraceTableColumn);
		final TableColumn meanTimeTableColumn = new TableColumn(table.getColumnCount());
		meanTimeTableColumn.setIdentifier(table.getColumnCount());
		meanTimeTableColumn.setHeaderValue(I18N.getString("JobMeanTime"));
		table.addColumn(meanTimeTableColumn);
		table.addColumn("elapsedTime", I18N.getString("JobElapsedTime"));
		table.addColumn("previousFireTime", I18N.getString("JobPreviousFireTime"));
		table.addColumn("nextFireTime", I18N.getString("JobNextFireTime"));
		table.addColumn("repeatInterval", I18N.getString("JobPeriodOrCronExpression"));
		table.addColumn("paused", I18N.getString("JobPaused"));

		table.setColumnCellRenderer("name", new NameTableCellRenderer());
		stackTraceTableColumn.setCellRenderer(new StackTraceTableCellRenderer());
		meanTimeTableColumn.setCellRenderer(new MeanTimeTableCellRenderer());
		table.setColumnCellRenderer("elapsedTime", new ElapsedTimeTableCellRenderer());

		final MDateTableCellRenderer fireTimeTableCellRenderer = new MDateTableCellRenderer();
		fireTimeTableCellRenderer.setDateFormat(I18N.createDateAndTimeFormat());
		table.setColumnCellRenderer("previousFireTime", fireTimeTableCellRenderer);
		table.setColumnCellRenderer("nextFireTime", fireTimeTableCellRenderer);
		table.setColumnCellRenderer("repeatInterval", new RepeatIntervalTableCellRenderer());
		// rq: on n'affiche pas le maximum, l'écart-type ou le pourcentage d'erreurs,
		// uniquement car cela ferait trop de colonnes dans la page

		table.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseClicked(MouseEvent e) {
				if (e.getClickCount() == 2) {
					final JobInformations jobInformations = getTable().getSelectedObject();
					final CounterRequest counterRequest = getCounterRequest(jobInformations);
					if (counterRequest.getStackTrace() != null) {
						ThreadInformationsPanel.showTextInPopup(JobInformationsPanel.this,
								counterRequest.getName(), counterRequest.getStackTrace());

					}
				}
			}
		});

		add(tableScrollPane, BorderLayout.NORTH);

		table.setList(jobInformationsList);
		Utilities.adjustTableHeight(table);
	}

	final CounterRequest getCounterRequest(JobInformations jobInformations) {
		final String jobFullName = jobInformations.getGroup() + '.' + jobInformations.getName();
		// rq: la méthode getCounterRequestByName prend en compte l'éventuelle utilisation du paramètre
		// job-transform-pattern qui peut faire que jobFullName != counterRequest.getName()
		final CounterRequest result = jobCounter.getCounterRequestByName(jobFullName);
		// getCounterRequestByName ne peut pas retourner null actuellement
		assert result != null;
		return result;
	}

	private void addButtons() {
		final Icon pauseIcon = ImageIconCache.getScaledImageIcon("control_pause_blue.png", 18, 18);
		final Icon resumeIcon = ImageIconCache.getScaledImageIcon("control_play_blue.png", 18, 18);
		final MButton pauseJobButton = new MButton(I18N.getString("Pause_job"), pauseIcon);
		pauseJobButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				if (confirm(I18N.getString("confirm_pause_job"))) {
					final JobInformations jobInformations = getTable().getSelectedObject();
					actionOnJob(Action.PAUSE_JOB, jobInformations.getGlobalJobId());
				}
			}
		});
		final MButton resumeJobButton = new MButton(I18N.getString("Resume_job"), resumeIcon);
		resumeJobButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				if (confirm(I18N.getString("confirm_resume_job"))) {
					final JobInformations jobInformations = getTable().getSelectedObject();
					actionOnJob(Action.RESUME_JOB, jobInformations.getGlobalJobId());
				}
			}
		});

		final MButton pauseAllJobsButton = new MButton(I18N.getString("Pause_all_jobs"), pauseIcon);
		pauseAllJobsButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				if (confirm(I18N.getString("confirm_pause_all_jobs"))) {
					actionOnJob(Action.PAUSE_JOB, "all");
				}
			}
		});
		final MButton resumeAllJobsButton = new MButton(I18N.getString("Resume_all_jobs"),
				resumeIcon);
		resumeAllJobsButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				if (confirm(I18N.getString("confirm_resume_all_jobs"))) {
					actionOnJob(Action.RESUME_JOB, "all");
				}
			}
		});
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
		final JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
		buttonPanel.setOpaque(false);
		buttonPanel.add(pauseJobButton);
		buttonPanel.add(resumeJobButton);
		buttonPanel.add(pauseAllJobsButton);
		buttonPanel.add(resumeAllJobsButton);
		add(buttonPanel, BorderLayout.EAST);
	}

	static JLabel toBar(int mean, long elapsedTime) {
		return JavaInformationsPanel.toBar("", 100d * elapsedTime / mean);
	}

	final MTable<JobInformations> getTable() {
		return table;
	}

	final void actionOnJob(Action action, String jobId) {
		try {
			// TODO refresh
			final String message = getRemoteCollector().executeActionAndCollectData(action, null,
					null, null, jobId);
			showMessage(message);
		} catch (final IOException ex) {
			showException(ex);
		}
	}
}
