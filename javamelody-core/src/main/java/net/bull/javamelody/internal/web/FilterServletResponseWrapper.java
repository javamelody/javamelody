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

import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;

import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpServletResponseWrapper;

/**
 * Implémentation de {@link HttpServletResponseWrapper} permettant d'encapsuler l'outputStream,
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

	HttpServletResponse getHttpServletResponse() {
		return (HttpServletResponse) getResponse();
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
		stream = null;
		writer = null;
	}

	/**
	 * Retourne le status définit par setStatus ou sendError.
	 * @return int
	 */
	public int getCurrentStatus() {
		// cette méthode s'appele getCurrentStatus pour ne pas interférer avec getStatus
		// dans servlet api 3.0, tout en restant compatible avec servlet api 2.5
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
			throw new IllegalStateException(
					"getWriter() has already been called for this response");
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

			try {
				getOutputStream();
			} catch (final IllegalStateException e) {
				// issue 488: if a filter has called getWriter() before the MonitoringFilter, we can't call getOutputStream()
				writer = super.getWriter();
				return writer;
			}

			final ServletOutputStream outputStream = getOutputStream();
			final String charEnc = getHttpServletResponse().getCharacterEncoding();
			// HttpServletResponse.getCharacterEncoding() shouldn't return null
			// according the spec, so feel free to remove that "if"
			final PrintWriter result;
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
		} else {
			// writes status code and headers when no content (issue #836)
			super.flushBuffer();
		}
	}

	public void flushStream() throws IOException {
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
	public void close() throws IOException {
		if (writer != null) {
			writer.close();
		} else if (stream != null) {
			stream.close();
		}
	}
}
