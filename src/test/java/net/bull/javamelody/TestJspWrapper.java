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

import static org.easymock.EasyMock.createNiceMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.expectLastCall;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import java.io.IOException;

import javax.servlet.RequestDispatcher;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.junit.Before;
import org.junit.Test;

/**
 * Test unitaire de la classe JspWrapper.
 * @author Emeric Vernat
 */
public class TestJspWrapper {
	/**
	 * Initialisation.
	 */
	@Before
	public void setUp() {
		Utils.initialize();
	}

	/**
	 * Test.
	 * @throws ServletException e
	 * @throws IOException e
	 */
	@Test
	public void testJspWrapper() throws ServletException, IOException {
		assertNotNull("getJspCounter", JspWrapper.getJspCounter());

		final HttpServletRequest request = createNiceMock(HttpServletRequest.class);
		final HttpServletResponse response = createNiceMock(HttpServletResponse.class);
		final RequestDispatcher requestDispatcher = createNiceMock(RequestDispatcher.class);
		final RequestDispatcher requestDispatcherWithError = createNiceMock(RequestDispatcher.class);
		final RequestDispatcher requestDispatcherWithException = createNiceMock(RequestDispatcher.class);
		final String url1 = "test.jsp";
		final String url2 = "test.jsp?param=test2";
		final String url3 = "test.jsp?param=test3";
		final String url4 = null;
		expect(request.getRequestDispatcher(url1)).andReturn(requestDispatcher);
		expect(request.getRequestDispatcher(url2)).andReturn(requestDispatcherWithError);
		requestDispatcherWithError.forward(request, response);
		expectLastCall().andThrow(new UnknownError("erreur dans forward"));
		expect(request.getRequestDispatcher(url3)).andReturn(requestDispatcherWithException);
		requestDispatcherWithException.forward(request, response);
		expectLastCall().andThrow(new IllegalStateException("erreur dans forward"));
		expect(request.getRequestDispatcher(url4)).andReturn(null);
		final HttpServletRequest wrappedRequest = JspWrapper.createHttpRequestWrapper(request);

		replay(request);
		replay(response);
		replay(requestDispatcher);
		replay(requestDispatcherWithError);
		replay(requestDispatcherWithException);
		final RequestDispatcher wrappedRequestDispatcher = wrappedRequest
				.getRequestDispatcher(url1);
		wrappedRequestDispatcher.toString();
		wrappedRequestDispatcher.include(wrappedRequest, response);
		final RequestDispatcher wrappedRequestDispatcher2 = wrappedRequest
				.getRequestDispatcher(url2);
		try {
			wrappedRequestDispatcher2.forward(request, response);
		} catch (final UnknownError e) {
			assertNotNull("ok", e);
		}
		final RequestDispatcher wrappedRequestDispatcher3 = wrappedRequest
				.getRequestDispatcher(url3);
		try {
			wrappedRequestDispatcher3.forward(request, response);
		} catch (final IllegalStateException e) {
			assertNotNull("ok", e);
		}
		final RequestDispatcher wrappedRequestDispatcher4 = wrappedRequest
				.getRequestDispatcher(url4);
		assertNull("getRequestDispatcher(null)", wrappedRequestDispatcher4);
		verify(request);
		verify(response);
		verify(requestDispatcher);
		// verify ne marche pas ici car on fait une Error, verify(requestDispatcherWithError);
		verify(requestDispatcherWithException);
	}
}
