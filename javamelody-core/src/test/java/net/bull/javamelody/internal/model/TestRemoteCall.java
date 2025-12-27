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

import static org.easymock.EasyMock.createNiceMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.io.IOException;
import java.net.URL;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jakarta.servlet.ServletContext;
import net.bull.javamelody.Utils;
import net.bull.javamelody.internal.common.Parameters;

/**
 * Test unitaire de la classe RemoteCall.
 * @author Emeric Vernat
 */
class TestRemoteCall {
	/** Check. */
	@BeforeEach
	void setUp() {
		Utils.initialize();
	}

	/** Test.
	 * @throws IOException e */
	@Test
	void testCollect() throws IOException {
		Utils.setProperty(Parameters.PARAMETER_SYSTEM_PREFIX + "mockLabradorRetriever", "true");
		final ServletContext context = createNiceMock(ServletContext.class);
		expect(context.getMajorVersion()).andReturn(5).anyTimes();
		expect(context.getMinorVersion()).andReturn(0).anyTimes();
		expect(context.getServletContextName()).andReturn("test webapp").anyTimes();
		expect(context.getServerInfo()).andReturn("mockJetty").anyTimes();
		expect(context.getContextPath()).andReturn("/test").anyTimes();
		replay(context);
		Parameters.initialize(context);
		final String url = "http://dummy";
		final RemoteCall remoteCall = new RemoteCall(url);
		remoteCall.collectJavaInformations();
		remoteCall.collectGraphLastValue("cpu");
		remoteCall.collectMBeanAttribute("java.lang:type=OperatingSystem.ProcessCpuTime");
		remoteCall.executeActionAndCollectData(Action.GC, null, null, null, null, null);
		assertEquals(url, new RemoteCall(new URL(url)).getURL().toString(), "getURL");
		verify(context);
		assertNotNull(new RemoteCall("http://dummy?p=1"), "");
	}
}
