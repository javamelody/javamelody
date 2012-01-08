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

import net.bull.javamelody.swing.MButton;

/**
 * Panel du dump de threads.
 * @author Emeric Vernat
 */
class ThreadsDumpPanel extends MelodyPanel {
	private static final long serialVersionUID = 1L;

	@SuppressWarnings("all")
	private final List<ThreadInformations> threadInformationsList;

	ThreadsDumpPanel(RemoteCollector remoteCollector,
			List<ThreadInformations> threadInformationsList) throws IOException {
		super(remoteCollector);
		assert threadInformationsList != null;
		this.threadInformationsList = threadInformationsList;

		final String threadsDump = getThreadsDump();

		setName(I18N.getString("Threads"));
		final JTextArea textArea = new JTextArea();
		textArea.setFont(new Font(Font.MONOSPACED, Font.PLAIN, textArea.getFont().getSize() - 1));
		textArea.setEditable(false);
		textArea.setText(threadsDump);
		textArea.setCaretPosition(0);
		add(new JScrollPane(textArea), BorderLayout.CENTER);

		final JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT, 0, 5));
		buttonPanel.setOpaque(false);
		// TODO traduction
		final MButton clipBoardButton = new MButton("Copier dans presse-papiers");
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
		final HtmlThreadInformationsReport htmlThreadInformationsReport = new HtmlThreadInformationsReport(
				threadInformationsList, true, writer);
		htmlThreadInformationsReport.writeThreadsDump();
		return writer.toString();
	}
}
