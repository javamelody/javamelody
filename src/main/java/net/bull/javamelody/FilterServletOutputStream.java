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

import java.io.IOException;
import java.io.OutputStream;

import javax.servlet.ServletOutputStream;

/**
 * Output stream pour un filtre implémentant ServletOutputStream à partir d'un OutputStream.
 * @author Emeric Vernat
 */
class FilterServletOutputStream extends ServletOutputStream {
	private final OutputStream stream;

	/**
	 * Constructeur.
	 * @param output OutputStream
	 */
	FilterServletOutputStream(OutputStream output) {
		super();
		assert output != null;
		stream = output;
	}

	/** {@inheritDoc} */
	@Override
	public void close() throws IOException {
		stream.close();
	}

	/** {@inheritDoc} */
	@Override
	public void flush() throws IOException {
		stream.flush();
	}

	/** {@inheritDoc} */
	@Override
	public void write(int b) throws IOException {
		stream.write(b);
	}

	/** {@inheritDoc} */
	@Override
	public void write(byte[] bytes) throws IOException {
		stream.write(bytes);
	}

	/** {@inheritDoc} */
	@Override
	public void write(byte[] bytes, int off, int len) throws IOException {
		stream.write(bytes, off, len);
	}
}
