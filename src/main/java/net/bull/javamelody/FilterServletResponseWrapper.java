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
import java.io.OutputStreamWriter;
import java.io.PrintWriter;

import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpServletResponseWrapper;

/**
 * Implémentation de HttpServletResponseWrapper permettant d'encapsuler l'outputStream,
 * par exemple pour calculer la taille du flux ou pour le compresser.
 * @author Emeric Vernat
 */
abstract class FilterServletResponseWrapper extends HttpServletResponseWrapper {
	private ServletOutputStream stream;
	private PrintWriter writer;
	private int status;

	/**
	 * Constructeur.
	 * @param response HttpServletResponse
	 */
	FilterServletResponseWrapper(HttpServletResponse response) {
		super(response);
		assert response != null;
	}

	/**
	 * @return ServletOutputStream
	 */
	ServletOutputStream getStream() {
		return stream;
	}

	/** {@inheritDoc} */
	@Override
	public void reset() {
		super.reset();
		status = 0;
	}

	/**
	 * Retourne le status définit par setStatus ou sendError.
	 * @return int
	 */
	public int getStatus() {
		return status;
	}

	/** {@inheritDoc} */
	@Override
	public void setStatus(int status) {
		super.setStatus(status);
		this.status = status;
	}

	/** {@inheritDoc} */
	@Override
	public void sendError(int error) throws IOException {
		super.sendError(error);
		this.status = error;
	}

	/** {@inheritDoc} */
	@Override
	public void sendError(int error, String message) throws IOException {
		super.sendError(error, message);
		this.status = error;
	}

	/**
	 * Crée et retourne un ServletOutputStream pour écrire le contenu dans la response associée.
	 * @return ServletOutputStream
	 * @throws IOException   Erreur d'entrée/sortie
	 */
	protected abstract ServletOutputStream createOutputStream() throws IOException;

	/** {@inheritDoc} */
	@Override
	public ServletOutputStream getOutputStream() throws IOException {
		if (writer != null) {
			throw new IllegalStateException("getWriter() has already been called for this response");
		}

		if (stream == null) {
			stream = createOutputStream();
			assert stream != null;
		}
		return stream;
	}

	/** {@inheritDoc} */
	@Override
	public PrintWriter getWriter() throws IOException {
		if (writer == null) {
			if (stream != null) {
				throw new IllegalStateException(
						"getOutputStream() has already been called for this response");
			}

			final ServletOutputStream outputStream = getOutputStream();
			final String charEnc = getResponse().getCharacterEncoding();
			// HttpServletResponse.getCharacterEncoding() shouldn't return null
			// according the spec, so feel free to remove that "if"
			PrintWriter result;
			if (charEnc == null) {
				result = new PrintWriter(outputStream);
			} else {
				result = new PrintWriter(new OutputStreamWriter(outputStream, charEnc));
			}
			writer = result;
		}
		return writer;
	}

	/** {@inheritDoc} */
	@Override
	public void flushBuffer() throws IOException {
		if (writer != null) {
			writer.flush();
		} else if (stream != null) {
			stream.flush();
		}
	}

	/**
	 * Ferme le flux.
	 * @throws IOException e
	 */
	void close() throws IOException {
		if (writer != null) {
			writer.close();
		} else if (stream != null) {
			stream.close();
		}
	}
}
