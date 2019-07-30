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
package net.bull.javamelody.internal.web;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.zip.GZIPOutputStream;

import javax.servlet.ServletOutputStream;
import javax.servlet.WriteListener;
import javax.servlet.http.HttpServletResponse;

/**
 * Implémentation de ServletOutputStream qui fonctionne avec le {@link CompressionServletResponseWrapper}.
 * @author Emeric Vernat
 */
class CompressionResponseStream extends ServletOutputStream {
	private final int compressionThreshold;
	private final HttpServletResponse response;
	private OutputStream stream;

	/**
	 * Construit un servlet output stream associé avec la réponse spécifiée.
	 * @param response HttpServletResponse
	 * @param compressionThreshold int
	 */
	CompressionResponseStream(HttpServletResponse response, int compressionThreshold) {
		super();
		assert response != null;
		assert compressionThreshold >= 0;
		this.response = response;
		this.compressionThreshold = compressionThreshold;
		this.stream = new ByteArrayOutputStream(compressionThreshold);
	}

	/** {@inheritDoc} */
	@Override
	public void close() throws IOException {
		if (stream instanceof ByteArrayOutputStream) {
			final byte[] bytes = ((ByteArrayOutputStream) stream).toByteArray();
			response.getOutputStream().write(bytes);
			stream = response.getOutputStream();
		}
		stream.close();
	}

	/**
	 * Flushe les données bufferisées de cet output stream.
	 * @throws IOException e
	 */
	@Override
	public void flush() throws IOException {
		stream.flush();
	}

	private void checkBufferSize(int length) throws IOException {
		// check if we are buffering too large of a file
		if (stream instanceof ByteArrayOutputStream) {
			final ByteArrayOutputStream baos = (ByteArrayOutputStream) stream;
			if (baos.size() + length > compressionThreshold) {
				// files too large to keep in memory are sent to the client
				flushToGZIP();
			}
		}
	}

	private void flushToGZIP() throws IOException {
		if (stream instanceof ByteArrayOutputStream) {
			// indication de compression,
			// on utilise setHeader et non addHeader pour être compatible avec PJL compression filter
			// en particulier dans le plugin grails vis à vis de l'autre plugin grails UiPerformance
			response.setHeader("Content-Encoding", "gzip");
			response.setHeader("Vary", "Accept-Encoding");

			// make new gzip stream using the response output stream (content-encoding is in constructor)
			final GZIPOutputStream gzipstream = new GZIPOutputStream(response.getOutputStream(),
					compressionThreshold);
			// get existing bytes
			final byte[] bytes = ((ByteArrayOutputStream) stream).toByteArray();
			gzipstream.write(bytes);
			// we are no longer buffering, send content via gzipstream
			stream = gzipstream;
		}
	}

	/** {@inheritDoc} */
	@Override
	public void write(int i) throws IOException {
		// make sure we aren't over the buffer's limit
		checkBufferSize(1);
		// write the byte to the temporary output
		stream.write(i);
	}

	/** {@inheritDoc} */
	@Override
	public void write(byte[] bytes) throws IOException {
		write(bytes, 0, bytes.length);
	}

	/** {@inheritDoc} */
	@Override
	public void write(byte[] bytes, int off, int len) throws IOException {
		if (len == 0) {
			return;
		}

		// make sure we aren't over the buffer's limit
		checkBufferSize(len);
		// write the content to the buffer
		stream.write(bytes, off, len);
	}

	@Override
	public boolean isReady() {
		return true;
	}

	@Override
	public void setWriteListener(WriteListener writeListener) {
		// nothing to do
	}
}
