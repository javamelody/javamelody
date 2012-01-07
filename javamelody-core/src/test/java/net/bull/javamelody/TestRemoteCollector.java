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

import java.io.IOException;
import java.net.URL;
import java.util.Collections;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

/**
 * Test unitaire de la classe Action.
 * @author Emeric Vernat
 */
public class TestRemoteCollector {
	/** Check. */
	@Before
	public void setUp() {
		Utils.initialize();
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testExecute() throws IOException {
		Utils.setProperty(Parameters.PARAMETER_SYSTEM_PREFIX + "mockLabradorRetriever", "true");
		final List<URL> urls = Collections.singletonList(new URL("http://localhost:8090/test"));
		final RemoteCollector remoteCollector = new RemoteCollector("test", urls);

		remoteCollector.executeActionAndCollectData(Action.CLEAR_COUNTER, "all", null, null, null);
		remoteCollector.executeActionAndCollectData(Action.INVALIDATE_SESSION, null, "nothing",
				null, null);
		remoteCollector
				.executeActionAndCollectData(Action.KILL_THREAD, null, null, "nothing", null);
		remoteCollector.executeActionAndCollectData(Action.PAUSE_JOB, null, null, null, "nothing");
	}
}
