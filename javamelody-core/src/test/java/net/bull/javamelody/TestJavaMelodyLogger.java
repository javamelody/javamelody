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

import static org.easymock.EasyMock.createNiceMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;

import javax.servlet.http.HttpServletRequest;

import org.junit.Before;
import org.junit.Test;

/**
 * Test unitaire des classes implémentant JavaMelodyLogger.
 * @author Emeric Vernat
 */
public class TestJavaMelodyLogger {

	/** Initialisation. */
	@Before
	public void setUp() {
		Utils.initialize();
	}

	/** Test. */
	@Test
	public void testDebugInfoAndWarn() {
		LOG.debug("test debug");
		LOG.debug("test debug", new IllegalStateException("test debug"));
		LOG.info("test info", new IllegalStateException("test info"));
		LOG.warn("test warn", new IllegalStateException("test warn"));
	}

	/** Test. */
	@Test
	public void testLoggers() {
		final JavaMelodyLogger javaLogger = new JavaLogger();
		final JavaMelodyLogger log4jLogger = new Log4JLogger();
		final JavaMelodyLogger logbackLogger = new LogbackLogger();
		logs(javaLogger);
		logs(log4jLogger);
		logs(logbackLogger);
	}

	private void logs(JavaMelodyLogger logger) {
		logger.debug("test debug");
		logger.debug("test debug", new IllegalStateException("test debug"));
		logger.info("test info");
		logger.info("test info", new IllegalStateException("test info"));
		logger.warn("test warn", new IllegalStateException("test warn"));

		final HttpServletRequest request = createNiceMock(HttpServletRequest.class);
		expect(request.getRemoteAddr()).andReturn("remote addr").anyTimes();
		expect(request.getRequestURI()).andReturn("/test/request").anyTimes();
		expect(request.getContextPath()).andReturn("/test").anyTimes();
		expect(request.getQueryString()).andReturn("param1=1").anyTimes();
		expect(request.getMethod()).andReturn("GET").anyTimes();

		replay(request);
		logger.logHttpRequest(request, "test", 1000, false, 10000, "javamelody");
		verify(request);
	}
}
