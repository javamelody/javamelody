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

import java.io.IOException;

import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServletResponse;

/**
 * Implémentation de FilterServletResponseWrapper qui fonctionne avec le CompressionServletResponseStream.
 * @author Emeric Vernat
 */
class CompressionServletResponseWrapper extends FilterServletResponseWrapper {
	private final int compressionThreshold;

	/**
	 * Constructeur qui crée un adapteur de ServletResponse wrappant la response spécifiée.
	 * @param response HttpServletResponse
	 * @param compressionThreshold int
	 */
	CompressionServletResponseWrapper(HttpServletResponse response, int compressionThreshold) {
		super(response);
		assert compressionThreshold >= 0;
		this.compressionThreshold = compressionThreshold;
	}

	/** {@inheritDoc} */
	@Override
	public ServletOutputStream createOutputStream() {
		return new CompressionResponseStream((HttpServletResponse) getResponse(),
				compressionThreshold);
	}

	/**
	 * Termine et ferme la response.
	 * @throws IOException e
	 */
	void finishResponse() throws IOException {
		close();
	}

	/**
	 * Ne fait rien
	 * @param length int
	 */
	@Override
	public void setContentLength(int length) {
		// ne fait rien
	}
}
