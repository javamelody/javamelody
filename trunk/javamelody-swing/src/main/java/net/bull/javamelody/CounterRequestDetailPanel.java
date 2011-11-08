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
import java.io.IOException;

/**
 * Panel du détail d'une requête.
 * @author Emeric Vernat
 */
class CounterRequestDetailPanel extends MelodyPanel {
	private static final long serialVersionUID = 1L;

	CounterRequestDetailPanel(RemoteCollector remoteCollector, CounterRequest request)
			throws IOException {
		super(remoteCollector);
		final String graphName = request.getId();
		final String graphLabel = truncate(request.getName(), 50);
		final ChartPanel chartPanel = new ChartPanel(remoteCollector, graphName, graphLabel);
		add(chartPanel, BorderLayout.CENTER);
		setName(graphLabel);
	}

	private static String truncate(String string, int maxLength) {
		return string.substring(0, Math.min(string.length(), maxLength));
	}
}
