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

import java.awt.Component;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.io.IOException;
import java.text.DateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import javax.swing.DefaultComboBoxModel;
import javax.swing.DefaultListCellRenderer;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JList;

import net.bull.javamelody.internal.common.I18N;
import net.bull.javamelody.internal.model.Range;
import net.bull.javamelody.internal.model.RemoteCollector;

/**
 * Panel des choix de la période par version.
 * @author Emeric Vernat
 */
class DeploymentPeriodPanel extends MelodyPanel {
	private static final long serialVersionUID = 1L;

	private final JComboBox<String> versionComboBox = new JComboBox<>();
	private Map<String, Range> rangeByWebappVersions;

	DeploymentPeriodPanel(RemoteCollector remoteCollector, Range selectedRange) throws IOException {
		super(remoteCollector, new FlowLayout(FlowLayout.CENTER));

		addDeploymentPeriodPanel(selectedRange);
	}

	Map<String, Range> getWebappVersions() {
		return rangeByWebappVersions;
	}

	void requestFocusInVersionField() {
		versionComboBox.requestFocus();
	}

	private void addDeploymentPeriodPanel(Range selectedRange) throws IOException {
		final Map<String, Date> webappVersions = getRemoteCollector().collectWebappVersions();
		final Map<String, Range> webappRanges = new LinkedHashMap<>();
		Date previousDate = null;
		String selectedVersion = null;
		for (final Map.Entry<String, Date> entry : webappVersions.entrySet()) {
			final String version = entry.getKey();
			final Date date = entry.getValue();
			final Range range;
			if (previousDate == null) {
				range = Range.createCustomRange(date, new Date());
			} else {
				range = Range.createCustomRange(date, previousDate);
			}
			previousDate = date;
			webappRanges.put(version, range);
			if (selectedRange != null && selectedRange.getValue().equals(range.getValue())) {
				selectedVersion = version;
			}
		}
		this.rangeByWebappVersions = webappRanges;

		final List<String> versions = new ArrayList<>();
		versions.add(" ");
		versions.addAll(webappVersions.keySet());
		versionComboBox.setModel(new DefaultComboBoxModel<>(versions.toArray(new String[0])));
		versionComboBox.setFont(versionComboBox.getFont().deriveFont(Font.BOLD));
		if (selectedVersion != null) {
			versionComboBox.setSelectedItem(selectedVersion);
		}

		versionComboBox.setRenderer(new DefaultListCellRenderer() {
			private static final long serialVersionUID = 1L;

			@Override
			public Component getListCellRendererComponent(JList<?> list, Object value, int index,
					boolean isSelected, boolean cellHasFocus) {
				final String version = (String) value;
				final String label = getVersionLabel(version);
				return super.getListCellRendererComponent(list, label, index, isSelected,
						cellHasFocus);
			}
		});
		versionComboBox.addItemListener(new ItemListener() {
			@Override
			public void itemStateChanged(ItemEvent e) {
				if (e.getStateChange() == ItemEvent.SELECTED) {
					final String version = (String) e.getItem();
					actionChangeDeploymentPeriod(version);
				}
			}
		});

		add(new JLabel(getString("Version")));
		add(versionComboBox);

	}

	String getVersionLabel(String version) {
		final Range range = rangeByWebappVersions.get(version);
		if (range == null) {
			return " ";
		}
		final DateFormat dateFormat = I18N.createDateFormat();
		final String startDateLabel = I18N.getString("startDate")
				.toLowerCase(I18N.getCurrentLocale());
		final String endDateLabel = I18N.getString("endDate");
		final String label;
		if (range.getEndDate().getTime() > System.currentTimeMillis()) {
			label = version + ' ' + startDateLabel + ' ' + dateFormat.format(range.getStartDate());
		} else {
			label = version + ' ' + startDateLabel + ' ' + dateFormat.format(range.getStartDate())
					+ ' ' + endDateLabel + ' ' + dateFormat.format(range.getEndDate());
		}
		return label;
	}

	private void actionChangePeriod(Range newRange) {
		MainPanel.getParentMainPanelFromChild(this).changeRange(newRange);
	}

	final void actionChangeDeploymentPeriod(String version) {
		final Range newRange = rangeByWebappVersions.get(version);
		if (newRange == null) {
			return;
		}
		actionChangePeriod(newRange);

		setVisible(false);
	}
}
