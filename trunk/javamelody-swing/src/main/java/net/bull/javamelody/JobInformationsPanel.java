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
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.net.URI;
import java.util.Date;
import java.util.List;

import javax.swing.Icon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTable;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingUtilities;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import net.bull.javamelody.table.MDateTableCellRenderer;
import net.bull.javamelody.table.MDefaultTableCellRenderer;
import net.bull.javamelody.table.MTable;
import net.bull.javamelody.table.MTableScrollPane;
import net.bull.javamelody.util.MSwingUtilities;

/**
 * Panel des jobs (Quartz).
 * @author Emeric Vernat
 */
class JobInformationsPanel extends JPanel {
	private static final long serialVersionUID = 1L;

	private final transient List<JobInformations> jobInformationsList;
	private final MTable<JobInformations> table;

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

	JobInformationsPanel(List<JobInformations> jobInformationsList) {
		super(new BorderLayout());
		this.jobInformationsList = jobInformationsList;
		this.table = new MTable<JobInformations>();

		setOpaque(false);

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
					MSwingUtilities.showException(ex);
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
		// TODO stackTrace et mean dépendent de counterRequest
		//		table.addColumn("stackTrace", I18N.getString("JobLastException"));
		//		table.addColumn("mean", I18N.getString("JobMeanTime"));
		table.addColumn("elapsedTime", I18N.getString("JobElapsedTime"));
		table.addColumn("previousFireTime", I18N.getString("JobPreviousFireTime"));
		table.addColumn("nextFireTime", I18N.getString("JobNextFireTime"));
		table.addColumn("repeatInterval", I18N.getString("JobPeriodOrCronExpression"));
		table.addColumn("paused", I18N.getString("JobPaused"));

		table.setColumnCellRenderer("name", new NameTableCellRenderer());

		final MDateTableCellRenderer durationTableCellRenderer = new MDateTableCellRenderer() {
			private static final long serialVersionUID = 1L;

			@Override
			public void setValue(final Object value) {
				final Long elapsedTime = (Long) value;
				if (elapsedTime >= 0) {
					final Date date = new Date(elapsedTime);
					setText(getDateFormat().format(date));
				} else {
					setText(null);
				}
			}
		};
		durationTableCellRenderer.setDateFormat(I18N.createDurationFormat());
		table.setColumnCellRenderer("elapsedTime", durationTableCellRenderer);

		final MDateTableCellRenderer fireTimeTableCellRenderer = new MDateTableCellRenderer();
		fireTimeTableCellRenderer.setDateFormat(I18N.createDateAndTimeFormat());
		table.setColumnCellRenderer("previousFireTime", fireTimeTableCellRenderer);
		table.setColumnCellRenderer("nextFireTime", fireTimeTableCellRenderer);

		//		final CounterRequest counterRequest = getCounterRequest(jobInformations);
		//		// counterRequest ne peut pas être null ici
		//		write("</td> <td align='center'>");
		//		writeStackTrace(counterRequest.getStackTrace());
		//		if (counterRequest.getMean() >= 0) {
		//			write(nextColumnAlignRight);
		//			write(durationFormat.format(new Date(counterRequest.getMean())));
		//		} else {
		//			write("</td><td>&nbsp;");
		//		}
		//		// rq: on n'affiche pas le maximum, l'écart-type ou le pourcentage d'erreurs,
		//		// uniquement car cela ferait trop de colonnes dans la page
		//
		//		write(nextColumnAlignRight);
		//		if (jobInformations.getElapsedTime() >= 0) {
		//			write(durationFormat.format(new Date(jobInformations.getElapsedTime())));
		//			write("<br/>");
		//			writeln(toBar(counterRequest.getMean(), jobInformations.getElapsedTime()));
		//		} else {
		//			write(nbsp);
		//		}
		//		write(nextColumnAlignRight);
		//		// on n'affiche pas la période si >= 1 jour car ce formateur ne saurait pas l'afficher
		//		if (jobInformations.getRepeatInterval() > 0
		//				&& jobInformations.getRepeatInterval() < ONE_DAY_MILLIS) {
		//			write(durationFormat.format(new Date(jobInformations.getRepeatInterval())));
		//		} else if (jobInformations.getCronExpression() != null) {
		//			// writer.write pour ne pas gérer de traductions si l'expression contient '#'
		//			writer.write(htmlEncode(jobInformations.getCronExpression()));
		//		} else {
		//			write(nbsp);
		//		}

		add(tableScrollPane, BorderLayout.NORTH);

		table.setList(jobInformationsList);
		table.setPreferredScrollableViewportSize(new Dimension(-1, table.getPreferredSize().height));
		SwingUtilities.invokeLater(new Runnable() {
			@Override
			public void run() {
				tableScrollPane
						.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER);
			}
		});
	}

	private void addButtons() {
		final Icon pauseIcon = ImageIconCache.getScaledImageIcon("control_pause_blue.png", 18, 18);
		final Icon resumeIcon = ImageIconCache.getScaledImageIcon("control_play_blue.png", 18, 18);
		final MButton pauseJobButton = new MButton(I18N.getString("Pause_job"), pauseIcon);
		pauseJobButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				if (confirm(I18N.getString("confirm_pause_job"))) {
					// TODO
				}
			}
		});
		final MButton resumeJobButton = new MButton(I18N.getString("Resume_job"), resumeIcon);
		resumeJobButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				if (confirm(I18N.getString("confirm_resume_job"))) {
					// TODO
				}
			}
		});

		final MButton pauseAllJobsButton = new MButton(I18N.getString("Pause_all_jobs"), pauseIcon);
		pauseAllJobsButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				if (confirm(I18N.getString("confirm_pause_all_jobs"))) {
					// TODO
				}
			}
		});
		final MButton resumeAllJobsButton = new MButton(I18N.getString("Resume_all_jobs"),
				resumeIcon);
		resumeAllJobsButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				if (confirm(I18N.getString("confirm_resume_all_jobs"))) {
					// TODO
				}
			}
		});
		final MTable<JobInformations> myTable = table;
		myTable.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
			@Override
			public void valueChanged(ListSelectionEvent e) {
				final JobInformations jobInformations = myTable.getSelectedObject();
				pauseJobButton.setEnabled(jobInformations != null);
				resumeJobButton.setEnabled(jobInformations != null);
			}
		});
		pauseJobButton.setEnabled(myTable.getSelectedObject() != null);
		resumeJobButton.setEnabled(myTable.getSelectedObject() != null);
		final JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
		buttonPanel.setOpaque(false);
		buttonPanel.add(pauseJobButton);
		buttonPanel.add(resumeJobButton);
		buttonPanel.add(pauseAllJobsButton);
		buttonPanel.add(resumeAllJobsButton);
		add(buttonPanel, BorderLayout.EAST);
	}

	final boolean confirm(String message) {
		return MSwingUtilities.showConfirmation(this, message);
	}

	final MTable<JobInformations> getTable() {
		return table;
	}
}
