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

import javax.servlet.http.HttpServletResponse;

/**
 * Implémentation de ServletOutputStream qui fonctionne avec le CounterServletResponseWrapper.
 * @author Emeric Vernat
 */
class CounterResponseStream extends FilterServletOutputStream {
	private int dataLength;

	/**
	 * Construit un servlet output stream associé avec la réponse spécifiée.
	 * @param response HttpServletResponse
	 * @throws IOException   Erreur d'entrée/sortie
	 */
	CounterResponseStream(HttpServletResponse response) throws IOException {
		super(response.getOutputStream());
	}

	/**
	 * Construit un servlet output stream associé avec l'output stream spécifiée.
	 * @param output OutputStream
	 */
	CounterResponseStream(OutputStream output) {
		super(output);
	}

	/**
	 * Retourne la valeur de la propriété dataLength.
	 * @return int
	 */
	public int getDataLength() {
		return dataLength;
	}

	/**
	 * Réinitialiser dataLength à 0.
	 */
	public void reset() {
		dataLength = 0;
	}

	/** {@inheritDoc} */
	@Override
	public void write(int i) throws IOException {
		super.write(i);
		dataLength += 1;
	}

	/** {@inheritDoc} */
	@Override
	public void write(byte[] bytes) throws IOException {
		super.write(bytes);
		final int len = bytes.length;
		dataLength += len;
	}

	/** {@inheritDoc} */
	@Override
	public void write(byte[] bytes, int off, int len) throws IOException {
		super.write(bytes, off, len);
		dataLength += len;
	}
}
