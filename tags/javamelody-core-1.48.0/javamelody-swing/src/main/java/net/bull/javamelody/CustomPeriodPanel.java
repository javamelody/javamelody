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

import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

import javax.swing.JLabel;

import net.bull.javamelody.swing.MButton;
import net.bull.javamelody.swing.MDateField;

/**
 * Panel des boutons principaux.
 * @author Emeric Vernat
 */
class CustomPeriodPanel extends MelodyPanel {
	private static final long serialVersionUID = 1L;

	private final MDateField startDateField = new MDateField();
	private final MDateField endDateField = new MDateField();

	CustomPeriodPanel(RemoteCollector remoteCollector, Range selectedRange) {
		super(remoteCollector, new FlowLayout(FlowLayout.CENTER));

		addCustomPeriodPanel(selectedRange);
	}

	void requestFocusInStartField() {
		startDateField.requestFocus();
	}

	private void addCustomPeriodPanel(Range selectedRange) {
		final DateFormat dateFormat = I18N.createDateFormat();
		final String dateFormatPattern;
		if (getString("dateFormatPattern").length() == 0) {
			final String pattern = ((SimpleDateFormat) dateFormat).toPattern();
			dateFormatPattern = pattern.toLowerCase(I18N.getCurrentLocale());
		} else {
			dateFormatPattern = getString("dateFormatPattern");
		}

		if (selectedRange.getStartDate() != null) {
			startDateField.setValue(selectedRange.getStartDate());
		}
		if (selectedRange.getEndDate() != null) {
			endDateField.setValue(selectedRange.getEndDate());
		}
		startDateField
				.setPreferredSize(new Dimension(80, startDateField.getPreferredSize().height));
		endDateField.setPreferredSize(new Dimension(80, endDateField.getPreferredSize().height));
		final MButton okButton = new MButton(getString("ok"));
		okButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				actionChangeCustomPeriod();
			}
		});

		add(new JLabel(getString("startDate")));
		add(startDateField);
		add(new JLabel(getString("endDate")));
		add(endDateField);
		add(new JLabel('(' + dateFormatPattern + ')'));
		add(okButton);
	}

	private void actionChangePeriod(Range newRange) {
		MainPanel.getParentMainPanelFromChild(this).changeRange(newRange);
	}

	final void actionChangeCustomPeriod() {
		final Date startDate = startDateField.getValue();
		final Date endDate = endDateField.getValue();
		if (startDate == null) {
			startDateField.requestFocus();
		} else if (endDate == null) {
			endDateField.requestFocus();
		}
		if (startDate == null || endDate == null) {
			final String message = getString("dates_mandatory");
			showMessage(message);
		} else {
			final Range newRange = Range.createCustomRange(startDate, endDate);
			startDateField.setValue(newRange.getStartDate());
			endDateField.setValue(newRange.getEndDate());
			actionChangePeriod(newRange);

			setVisible(false);
		}
	}
}
