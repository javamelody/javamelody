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

import javax.servlet.http.HttpServletResponse;

/**
 * Implémentation de ServletOutputStream qui fonctionne avec le HtmlInjectorServletResponseWrapper.
 * @author Emeric Vernat
 */
class HtmlInjectorResponseStream extends FilterServletOutputStream {
	private final HttpServletResponse response;
	private final HtmlToInject htmlToInject;
	private final byte[] beforeTag;
	private boolean injectionCanceled;

	interface HtmlToInject {
		/**
		 * @return Html content to inject.
		 */
		String getContent();

		/**
		 * @return Portion of html to inject content before.
		 */
		String getBeforeTag();
	}

	/**
	 * Construit un servlet output stream associé avec la réponse spécifiée.
	 * @param response HttpServletResponse
	 * @param htmlToInject HtmlToInject
	 * @throws IOException   Erreur d'entrée/sortie
	 */
	HtmlInjectorResponseStream(HttpServletResponse response, HtmlToInject htmlToInject)
			throws IOException {
		super(response.getOutputStream());
		this.response = response;
		this.htmlToInject = htmlToInject;
		// HttpServletResponse.getCharacterEncoding() shouldn't return null according the spec.
		// And response.getCharacterEncoding() may not be explicit yet,
		// but we suppose that it does not make any difference on the beforeTag.
		this.beforeTag = htmlToInject.getBeforeTag().getBytes(response.getCharacterEncoding());
	}

	void cancelInjection() {
		injectionCanceled = true;
	}

	// not worth it
	//	/** {@inheritDoc} */
	//	@Override
	//	public void write(int i) throws IOException {
	//		super.write(i);
	//	}

	/** {@inheritDoc} */
	@Override
	public void write(byte[] bytes) throws IOException {
		write(bytes, 0, bytes.length);
	}

	/** {@inheritDoc} */
	@Override
	public void write(byte[] bytes, int off, int len) throws IOException {
		// if httpResponse.setContentType(x) has been called with !x.contains("text/html"),
		// then no need to continue scanning for the beforeTag
		if (injectionCanceled) {
			super.write(bytes, off, len);
		} else {
			final int index = indexOf(bytes, beforeTag, off, len);
			if (index == -1) {
				// beforeTag not found yet
				super.write(bytes, off, len);
			} else {
				// beforeTag found: inject content.
				super.write(bytes, off, index);
				final String content = htmlToInject.getContent();
				// HttpServletResponse.getCharacterEncoding() shouldn't return null according the spec
				super.write(content.getBytes(response.getCharacterEncoding()));
				super.write(bytes, off + index, len - index);
			}
		}
	}

	private static int indexOf(byte[] sourceBytes, byte[] targetBytes, int sourceOffset,
			int sourceLength) {
		final byte first = targetBytes[0];
		final int max = sourceOffset + sourceLength - targetBytes.length;

		for (int i = sourceOffset; i <= max; i++) {
			// Look for first byte
			while (i <= max && sourceBytes[i] != first) {
				i++;
			}

			if (i <= max) {
				// Found first byte, now look at the rest of sourceBytes
				int j = i + 1;
				final int end = j + targetBytes.length - 1;
				for (int k = 1; j < end && sourceBytes[j] == targetBytes[k]; k++) {
					j++;
				}

				if (j == end) {
					// Found whole bytes
					return i - sourceOffset;
				}
			}
		}
		return -1;
	}
}
