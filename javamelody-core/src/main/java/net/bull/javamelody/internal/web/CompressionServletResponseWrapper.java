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

import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServletResponse;

/**
 * Implémentation de {@link FilterServletResponseWrapper} qui fonctionne avec le {@link CompressionResponseStream}.
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
		return new CompressionResponseStream(getHttpServletResponse(), compressionThreshold);
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
