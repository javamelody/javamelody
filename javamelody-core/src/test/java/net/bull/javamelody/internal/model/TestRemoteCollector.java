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
package net.bull.javamelody.internal.model;

import static org.junit.Assert.assertNotNull;

import java.io.IOException;
import java.net.URL;
import java.util.Collections;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import net.bull.javamelody.Utils;
import net.bull.javamelody.internal.common.Parameters;

/**
 * Test unitaire de la classe Action.
 * @author Emeric Vernat
 */
public class TestRemoteCollector {
	private RemoteCollector remoteCollector;

	/** Check.
	 * @throws IOException e */
	@Before
	public void setUp() throws IOException {
		Utils.initialize();
		Utils.setProperty(Parameters.PARAMETER_SYSTEM_PREFIX + "mockLabradorRetriever", "true");
		final List<URL> urls = Collections.singletonList(new URL("http://localhost:8090/test"));
		this.remoteCollector = new RemoteCollector("test", urls);
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testExecute() throws IOException {
		remoteCollector.executeActionAndCollectData(Action.CLEAR_COUNTER, "all", null, null, null,
				null);
		remoteCollector.executeActionAndCollectData(Action.INVALIDATE_SESSION, null, "nothing",
				null, null, null);
		remoteCollector.executeActionAndCollectData(Action.KILL_THREAD, null, null, "nothing", null,
				null);
		remoteCollector.executeActionAndCollectData(Action.PAUSE_JOB, null, null, null, "nothing",
				null);
		remoteCollector.executeActionAndCollectData(Action.CLEAR_CACHE, null, null, null, null,
				"nothing");
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testGetJRobin() throws IOException {
		remoteCollector.collectJRobin("cpu", 50, 50);
		remoteCollector.collectJRobins(50, 50);
		remoteCollector.collectOtherJRobins(50, 50);
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testForSwing() throws IOException {
		remoteCollector.setCookies("testcookies");
		remoteCollector.setURLs(remoteCollector.getURLs());
		remoteCollector.disableAggregation();
		remoteCollector.executeActionAndCollectData(Action.CLEAR_COUNTER, "all", null, null, null,
				null);
		remoteCollector.collectDataIncludingCurrentRequests();
		assertNotNull("getCurrentRequests", remoteCollector.getCurrentRequests());
		assertNotNull("collectWebappVersions", remoteCollector.collectWebappVersions());
		assertNotNull("collectWebappDependencies", remoteCollector.collectWebappDependencies());
	}
}
