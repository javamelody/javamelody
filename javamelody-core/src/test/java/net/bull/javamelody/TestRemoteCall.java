/*
 * Copyright 2008-2014 by Emeric Vernat
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

import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.net.URL;

import org.junit.Before;
import org.junit.Test;

/**
 * Test unitaire de la classe RemoteCall.
 * @author Emeric Vernat
 */
public class TestRemoteCall {
	/** Check. */
	@Before
	public void setUp() {
		Utils.initialize();
	}

	/** Test. 
	 * @throws IOException e */
	@Test
	public void testCollect() throws IOException {
		Utils.setProperty(Parameters.PARAMETER_SYSTEM_PREFIX + "mockLabradorRetriever", "true");
		final String url = "http://dummy";
		final RemoteCall remoteCall = new RemoteCall(url);
		remoteCall.collectJavaInformations();
		remoteCall.collectGraphLastValue("cpu");
		remoteCall.collectMBeanAttribute("java.lang:type=OperatingSystem.ProcessCpuTime");
		assertEquals("getURL", new RemoteCall(new URL(url)).getURL().toString(), url);
	}
}
