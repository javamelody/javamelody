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

import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServletResponse;

/**
 * Implémentation de FilterServletResponseWrapper qui fonctionne avec le CounterResponseStream,
 * pour calculer la taille du flux de réponse.
 * @author Emeric Vernat
 */
class CounterServletResponseWrapper extends FilterServletResponseWrapper {
	/**
	 * Constructeur qui crée un adapteur de HttpServletResponse wrappant la response spécifiée.
	 * @param response HttpServletResponse
	 */
	CounterServletResponseWrapper(HttpServletResponse response) {
		super(response);
		assert response != null;
	}

	/**
	 * Retourne la taille en octets du flux écrit dans la réponse.
	 * @return int
	 */
	public int getDataLength() {
		return getCounterResponseStream() == null ? 0 : getCounterResponseStream().getDataLength();
	}

	/** {@inheritDoc} */
	@Override
	public void reset() {
		super.reset();
		resetStream();
	}

	/** {@inheritDoc} */
	@Override
	public void resetBuffer() {
		super.resetBuffer();
		resetStream();
	}

	private void resetStream() {
		if (getCounterResponseStream() != null) {
			getCounterResponseStream().reset();
		}
	}

	private CounterResponseStream getCounterResponseStream() {
		return (CounterResponseStream) getStream();
	}

	/** {@inheritDoc} */
	@Override
	public ServletOutputStream createOutputStream() throws IOException {
		return new CounterResponseStream((HttpServletResponse) getResponse());
	}
}
