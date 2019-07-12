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
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import java.io.StringWriter;
import java.util.List;

import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;

import net.bull.javamelody.internal.model.JavaInformations;
import net.bull.javamelody.internal.model.RemoteCollector;
import net.bull.javamelody.internal.model.ThreadInformations;
import net.bull.javamelody.internal.web.html.HtmlThreadInformationsReport;
import net.bull.javamelody.swing.MButton;

/**
 * Panel du dump de threads.
 * @author Emeric Vernat
 */
class ThreadsDumpPanel extends MelodyPanel {
	private static final long serialVersionUID = 1L;

	private final JavaInformations javaInformations;
	@SuppressWarnings("all")
	private final List<ThreadInformations> threadInformationsList;

	ThreadsDumpPanel(RemoteCollector remoteCollector, JavaInformations javaInformations)
			throws IOException {
		super(remoteCollector);
		assert javaInformations != null;
		this.javaInformations = javaInformations;
		this.threadInformationsList = javaInformations.getThreadInformationsList();

		final String threadsDump = getThreadsDump();

		setName(getString("Threads"));
		final JTextArea textArea = new JTextArea();
		textArea.setFont(new Font(Font.MONOSPACED, Font.PLAIN, textArea.getFont().getSize() - 1));
		textArea.setEditable(false);
		textArea.setText(threadsDump);
		textArea.setCaretPosition(0);
		add(new JScrollPane(textArea), BorderLayout.CENTER);

		final JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT, 0, 5));
		buttonPanel.setOpaque(false);
		final MButton clipBoardButton = new MButton(getString("Copier_dans_presse-papiers"));
		clipBoardButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				textArea.selectAll();
				textArea.copy();
				textArea.setCaretPosition(0);
			}
		});
		buttonPanel.add(clipBoardButton);
		add(buttonPanel, BorderLayout.SOUTH);
	}

	private String getThreadsDump() throws IOException {
		final StringWriter writer = new StringWriter();
		writer.write("===== " + getFormattedString("Threads_sur", javaInformations.getHost())
				+ " =====");
		writer.write("\n\n");
		final HtmlThreadInformationsReport htmlThreadInformationsReport = new HtmlThreadInformationsReport(
				threadInformationsList, true, writer);
		htmlThreadInformationsReport.writeThreadsDump();
		return writer.toString();
	}
}
