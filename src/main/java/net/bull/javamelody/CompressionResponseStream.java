/*
 * Copyright 2008-2009 by Emeric Vernat, Bull
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

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.zip.GZIPOutputStream;

import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServletResponse;

/**
 * Implémentation de ServletOutputStream qui fonctionne avec le CompressionServletResponseWrapper.
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
			// indication de compression
			response.addHeader("Content-Encoding", "gzip");
			response.addHeader("Vary", "Accept-Encoding");

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
}
