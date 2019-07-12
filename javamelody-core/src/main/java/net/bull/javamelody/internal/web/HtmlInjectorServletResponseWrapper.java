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
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import net.bull.javamelody.internal.web.HtmlInjectorResponseStream.HtmlToInject;

/**
 * Implémentation de FilterServletResponseWrapper qui fonctionne avec le HtmlInjectorResponseStream.
 * @author Emeric Vernat
 */
class HtmlInjectorServletResponseWrapper extends FilterServletResponseWrapper {
	private static final String INJECTOR_WRAPPED_REQUEST_KEY = "javamelody.injectorWrapped";

	private final HtmlToInject htmlToInject;

	/**
	 * Constructeur qui crée un adapteur de HttpServletResponse wrappant la response spécifiée.
	 * @param request HttpServletRequest
	 * @param response HttpServletResponse
	 * @param htmlToInject HtmlToInject
	 */
	HtmlInjectorServletResponseWrapper(HttpServletRequest request, HttpServletResponse response,
			HtmlToInject htmlToInject) {
		super(response);
		assert request != null;
		assert response != null;
		assert htmlToInject != null;
		this.htmlToInject = htmlToInject;
		request.setAttribute(INJECTOR_WRAPPED_REQUEST_KEY, Boolean.TRUE);
	}

	static boolean acceptsRequest(HttpServletRequest request) {
		// we need to inject only in html pages
		// and we only accept wrapping once
		// (against filtering of request and then of include/forward for the same request for example)
		final String accept = request.getHeader("accept");
		return accept != null && accept.contains("text/html")
				&& request.getAttribute(INJECTOR_WRAPPED_REQUEST_KEY) == null;
	}

	/** {@inheritDoc} */
	@Override
	public ServletOutputStream createOutputStream() throws IOException {
		if (!isContentTypeHtml()) {
			return getHttpServletResponse().getOutputStream();
		}
		return new HtmlInjectorResponseStream(getHttpServletResponse(), htmlToInject);
	}

	/** {@inheritDoc} */
	@Override
	public void setContentType(String contentType) {
		super.setContentType(contentType);
		if (!isContentTypeHtml() && getStream() instanceof HtmlInjectorResponseStream) {
			((HtmlInjectorResponseStream) getStream()).cancelInjection();
		}
	}

	private boolean isContentTypeHtml() {
		// if contentType is null we suppose that it is text/html like a browser would do
		final String contentType = getContentType();
		return contentType == null || contentType.contains("text/html");
	}
}
