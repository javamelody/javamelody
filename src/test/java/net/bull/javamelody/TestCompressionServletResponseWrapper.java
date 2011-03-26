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

		public ServletOutputStream getOutputStream() throws IOException {
			return outputStream;
		}

		public PrintWriter getWriter() throws IOException {
			return null;
		}

		public void addCookie(Cookie cookie) {
			// rien
		}

		public void addDateHeader(String name, long date) {
			// rien
		}

		public void addHeader(String name, String value) {
			// rien
		}

		public void addIntHeader(String name, int value) {
			// rien
		}

		public boolean containsHeader(String name) {
			return false;
		}

		public String encodeRedirectURL(String url) {
			return null;
		}

		/** @deprecated déprécié
		 * @param url String
		 * @return String */
		@Deprecated
		public String encodeRedirectUrl(String url) {
			return null;
		}

		public String encodeURL(String url) {
			return null;
		}

		/** @deprecated déprécié
		 * @param url String
		 * @return String */
		@Deprecated
		public String encodeUrl(String url) {
			return null;
		}

		public void sendError(int sc) throws IOException {
			// rien
		}

		public void sendError(int sc, String msg) throws IOException {
			// rien
		}

		public void sendRedirect(String location) throws IOException {
			// rien
		}

		public void setDateHeader(String name, long date) {
			// rien
		}

		public void setHeader(String name, String value) {
			// rien
		}

		public void setIntHeader(String name, int value) {
			// rien
		}

		public void setStatus(int sc) {
			// rien
		}

		/** @deprecated déprécié
		 * @param sc int
		 * @param sm String */
		@Deprecated
		public void setStatus(int sc, String sm) {
			// rien
		}

		public void flushBuffer() throws IOException {
			// rien
		}

		public int getBufferSize() {
			return 0;
		}

		public String getCharacterEncoding() {
			return null;
		}

		public String getContentType() {
			return null;
		}

		public Locale getLocale() {
			return null;
		}

		public boolean isCommitted() {
			return false;
		}

		public void reset() {
			// rien
		}

		public void resetBuffer() {
			// rien
		}

		public void setBufferSize(int size) {
			// rien
		}

		public void setCharacterEncoding(String charset) {
			// rien
		}

		public void setContentLength(int len) {
			// rien
		}

		public void setContentType(String type) {
			// rien
		}

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
