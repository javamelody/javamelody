/*
 * Copyright 2008-2014 by Emeric Vernat
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
