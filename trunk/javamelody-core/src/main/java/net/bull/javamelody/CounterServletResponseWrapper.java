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
