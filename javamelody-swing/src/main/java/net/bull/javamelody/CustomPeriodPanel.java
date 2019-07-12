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

import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

import javax.swing.JLabel;

import net.bull.javamelody.internal.common.I18N;
import net.bull.javamelody.internal.model.Range;
import net.bull.javamelody.internal.model.RemoteCollector;
import net.bull.javamelody.swing.MButton;
import net.bull.javamelody.swing.MDateField;

/**
 * Panel de saisie de la période personnalisée.
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
