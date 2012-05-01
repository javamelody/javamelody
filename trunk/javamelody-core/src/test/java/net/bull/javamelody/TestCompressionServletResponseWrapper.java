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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Locale;

import javax.servlet.ServletOutputStream;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletResponse;

import org.junit.Before;
import org.junit.Test;

/**
 * Test unitaire des classes CompressionServletResponseWrapper, CompressionResponseStream
 * et FilterServletOutputStream.
 * @author Emeric Vernat
 */
public class TestCompressionServletResponseWrapper {
	/** Check. */
	@Before
	public void setUp() {
		Utils.initialize();
	}

	static class HttpResponse implements HttpServletResponse {
		private final ServletOutputStream outputStream = new FilterServletOutputStream(
				new ByteArrayOutputStream());

		@Override
		public ServletOutputStream getOutputStream() throws IOException {
			return outputStream;
		}

		@Override
		public PrintWriter getWriter() throws IOException {
			return null;
		}

		@Override
		public void addCookie(Cookie cookie) {
			// rien
		}

		@Override
		public void addDateHeader(String name, long date) {
			// rien
		}

		@Override
		public void addHeader(String name, String value) {
			// rien
		}

		@Override
		public void addIntHeader(String name, int value) {
			// rien
		}

		@Override
		public boolean containsHeader(String name) {
			return false;
		}

		@Override
		public String encodeRedirectURL(String url) {
			return null;
		}

		/** @deprecated déprécié
		 * @param url String
		 * @return String */
		@Override
		@Deprecated
		public String encodeRedirectUrl(String url) {
			return null;
		}

		@Override
		public String encodeURL(String url) {
			return null;
		}

		/** @deprecated déprécié
		 * @param url String
		 * @return String */
		@Override
		@Deprecated
		public String encodeUrl(String url) {
			return null;
		}

		@Override
		public void sendError(int sc) throws IOException {
			// rien
		}

		@Override
		public void sendError(int sc, String msg) throws IOException {
			// rien
		}

		@Override
		public void sendRedirect(String location) throws IOException {
			// rien
		}

		@Override
		public void setDateHeader(String name, long date) {
			// rien
		}

		@Override
		public void setHeader(String name, String value) {
			// rien
		}

		@Override
		public void setIntHeader(String name, int value) {
			// rien
		}

		@Override
		public void setStatus(int sc) {
			// rien
		}

		/** @deprecated déprécié
		 * @param sc int
		 * @param sm String */
		@Override
		@Deprecated
		public void setStatus(int sc, String sm) {
			// rien
		}

		@Override
		public void flushBuffer() throws IOException {
			// rien
		}

		@Override
		public int getBufferSize() {
			return 0;
		}

		@Override
		public String getCharacterEncoding() {
			return null;
		}

		@Override
		public String getContentType() {
			return null;
		}

		@Override
		public Locale getLocale() {
			return null;
		}

		@Override
		public boolean isCommitted() {
			return false;
		}

		@Override
		public void reset() {
			// rien
		}

		@Override
		public void resetBuffer() {
			// rien
		}

		@Override
		public void setBufferSize(int size) {
			// rien
		}

		@Override
		public void setCharacterEncoding(String charset) {
			// rien
		}

		@Override
		public void setContentLength(int len) {
			// rien
		}

		@Override
		public void setContentType(String type) {
			// rien
		}

		@Override
		public void setLocale(Locale loc) {
			// rien
		}
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testCompressionServletResponseWrapper() throws IOException {
		final CompressionServletResponseWrapper wrapper = new CompressionServletResponseWrapper(
				new HttpResponse(), 1024);
		wrapper.setStatus(HttpServletResponse.SC_NOT_FOUND);
		assertEquals("status", HttpServletResponse.SC_NOT_FOUND, wrapper.getCurrentStatus());
		wrapper.sendError(HttpServletResponse.SC_BAD_GATEWAY);
		assertEquals("status", HttpServletResponse.SC_BAD_GATEWAY, wrapper.getCurrentStatus());
		wrapper.sendError(HttpServletResponse.SC_SERVICE_UNAVAILABLE, "message");
		assertEquals("status", HttpServletResponse.SC_SERVICE_UNAVAILABLE,
				wrapper.getCurrentStatus());
		assertNotNull("outputStream", wrapper.createOutputStream());
		assertNotNull("writer", wrapper.getWriter());
		wrapper.flushBuffer();
		wrapper.close();
		wrapper.setContentLength(0);
		wrapper.finishResponse();
		boolean ok = false;
		try {
			wrapper.getOutputStream();
		} catch (final Exception e) {
			ok = true;
		}
		assertTrue("exception", ok);

		final CompressionServletResponseWrapper wrapper2 = new CompressionServletResponseWrapper(
				new HttpResponse(), 1024);
		assertNotNull("outputStream", wrapper2.getOutputStream());
		wrapper2.flushBuffer();
		wrapper2.close();
		boolean ok2 = false;
		try {
			wrapper2.getWriter();
		} catch (final Exception e) {
			ok2 = true;
		}
		assertTrue("exception", ok2);
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testCompressionResponseStream() throws IOException {
		final CompressionResponseStream stream = new CompressionResponseStream(new HttpResponse(),
				1024);
		stream.write(1);
		stream.write(new byte[8]);
		stream.write(new byte[8], 1, 7);
		stream.write(new byte[8], 1, 0);
		stream.flush();
		stream.close();

		final CompressionResponseStream zipStream = new CompressionResponseStream(
				new HttpResponse(), 8);
		zipStream.write(new byte[16], 0, 16);
		zipStream.flush();
		zipStream.close();
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testFilterServletOutputStream() throws IOException {
		final FilterServletOutputStream outputStream = new FilterServletOutputStream(
				new ByteArrayOutputStream());
		outputStream.write(1);
		outputStream.write(new byte[8]);
		outputStream.write(new byte[8], 1, 7);
		outputStream.flush();
		outputStream.close();
	}
}
