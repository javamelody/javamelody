/*
 * Copyright 2008-2017 by Emeric Vernat
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
package net.bull.javamelody.internal.web;

import static org.easymock.EasyMock.createNiceMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.nio.charset.StandardCharsets;

import javax.servlet.ServletOutputStream;
import javax.servlet.WriteListener;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.junit.Test;

import net.bull.javamelody.internal.model.Counter;

/**
 * Test for RumInjector.
 * @author Emeric Vernat
 */
public class TestRumInjector {

	private static class NullServletOutputStream extends ServletOutputStream {
		NullServletOutputStream() {
			super();
		}

		@Override
		public void write(int b) throws IOException {
			// nothing
		}

		@Override
		public void setWriteListener(WriteListener writeListener) {
			// nothing
		}

		@Override
		public boolean isReady() {
			return true;
		}
	}

	/**
	 * Test.
	 */
	@Test
	public void testIsRumResource() {
		assertTrue("isRumResource", RumInjector.isRumResource("boomerang.min.js"));
		assertFalse("isRumResource", RumInjector.isRumResource("notboomerang"));
	}

	/**
	 * Test.
	 * @throws IOException e
	 */
	@Test
	public void testCreateRumResponseWrapper() throws IOException {
		final String requestName = "test GET";

		final HttpServletRequest httpRequest = createNiceMock(HttpServletRequest.class);
		final HttpServletResponse httpResponse = createNiceMock(HttpServletResponse.class);
		expect(httpRequest.getHeader("accept")).andReturn(null);
		replay(httpRequest);
		replay(httpResponse);
		final HttpServletResponse result = RumInjector.createRumResponseWrapper(httpRequest,
				httpResponse, requestName);
		verify(httpRequest);
		verify(httpResponse);
		assertFalse("createRumResponseWrapper",
				result instanceof HtmlInjectorServletResponseWrapper);

		final HttpServletRequest httpRequest2 = createNiceMock(HttpServletRequest.class);
		final HttpServletResponse httpResponse2 = createNiceMock(HttpServletResponse.class);
		expect(httpRequest2.getHeader("accept")).andReturn("text/html").anyTimes();
		expect(httpResponse2.getContentType()).andReturn("text/html").anyTimes();
		expect(httpResponse2.getOutputStream()).andReturn(new NullServletOutputStream()).anyTimes();
		expect(httpResponse2.getCharacterEncoding()).andReturn(StandardCharsets.ISO_8859_1.name())
				.anyTimes();
		replay(httpRequest2);
		replay(httpResponse2);
		final HttpServletResponse result2 = RumInjector.createRumResponseWrapper(httpRequest2,
				httpResponse2, requestName);
		final ServletOutputStream outputStream = result2.getOutputStream();
		outputStream.write(' ');
		outputStream.write("<!-- begin test -->".getBytes("UTF-8"));
		final String htmlContent = "<html><body>test</body></html>";
		outputStream.write(htmlContent.getBytes("UTF-8"));
		result2.setContentType("text/html");
		outputStream.write("<!-- end test -->".getBytes("UTF-8"));
		verify(httpRequest2);
		verify(httpResponse2);
		assertTrue("createRumResponseWrapper",
				result2 instanceof HtmlInjectorServletResponseWrapper);
	}

	/**
	 * Test.
	 */
	@Test
	public void testAddRumHit() {
		final Counter httpCounter = new Counter(Counter.HTTP_COUNTER_NAME, "dbweb.png");
		// test null requestName
		addRumHit(httpCounter, null, null, null, null, null);

		final String requestName = "test";
		// test non-parseable values
		addRumHit(httpCounter, requestName, null, null, null, null);
		addRumHit(httpCounter, requestName, "a", "b", "c", "d");

		// test valid values without existing request
		addRumHit(httpCounter, requestName, "100", "200", "300", "400");

		// test with existing request and creating rumData
		httpCounter.addRequest(requestName, 100, 10, 10, false, 1);
		addRumHit(httpCounter, requestName, "100", "200", "300", "400");

		// test with existing request and rumData
		addRumHit(httpCounter, requestName, "100", "200", "300", "400");

		// test aberrant values
		addRumHit(httpCounter, requestName, "-100", "200", "300", "400");
		addRumHit(httpCounter, requestName, "300001", "200", "300", "400");
		addRumHit(httpCounter, requestName, "100", "-200", "300", "400");
		addRumHit(httpCounter, requestName, "100", "300101", "300", "400");
		addRumHit(httpCounter, requestName, "100", "200", "-300", "400");
		addRumHit(httpCounter, requestName, "100", "200", "300001", "400");
		addRumHit(httpCounter, requestName, "100", "200", "300", "-400");
		addRumHit(httpCounter, requestName, "100", "200", "300", "300001");
	}

	private void addRumHit(Counter httpCounter, String requestName, String serverTime,
			String timeToFirstByte, String domProcessing, String pageRendering) {
		final HttpServletRequest httpRequest = createNiceMock(HttpServletRequest.class);
		expect(httpRequest.getParameter("requestName")).andReturn(requestName);
		expect(httpRequest.getParameter("serverTime")).andReturn(serverTime).anyTimes();
		expect(httpRequest.getParameter("timeToFirstByte")).andReturn(timeToFirstByte).anyTimes();
		expect(httpRequest.getParameter("domProcessing")).andReturn(domProcessing).anyTimes();
		expect(httpRequest.getParameter("pageRendering")).andReturn(pageRendering).anyTimes();
		replay(httpRequest);
		RumInjector.addRumHit(httpRequest, httpCounter);
		verify(httpRequest);
	}
}
