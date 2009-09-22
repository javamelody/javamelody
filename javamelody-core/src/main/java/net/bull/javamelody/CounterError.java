/*
 * Copyright 2008-2009 by Emeric Vernat, Bull
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

import java.io.Serializable;
import java.util.Date;

import javax.servlet.http.HttpServletRequest;

/**
 * Objet correspondant à une erreur dans filtre http ou dans logs à un instant t, avec son message, sa stackTrace
 * éventuelle et son utilisateur courant s'il est défini.
 * @author Emeric Vernat
 */
class CounterError implements Serializable {
	static final String REQUEST_KEY = "monitoring.request";
	private static final long serialVersionUID = 5690702786722045646L;
	@SuppressWarnings("all")
	private static final ThreadLocal<HttpServletRequest> HTTP_SERVLET_REQUEST_CONTEXT = new ThreadLocal<HttpServletRequest>();
	private final long time;
	private final String remoteUser;
	private final String httpRequest;
	private final String message;
	private final String stackTrace;

	CounterError(String message, String stackTrace) {
		super();
		assert message != null;
		this.time = System.currentTimeMillis();
		this.message = message;
		this.stackTrace = stackTrace;
		final HttpServletRequest currentRequest = getCurrentRequest();
		if (currentRequest == null) {
			this.remoteUser = null;
			this.httpRequest = null;
		} else {
			this.remoteUser = currentRequest.getRemoteUser();
			this.httpRequest = (String) currentRequest.getAttribute(REQUEST_KEY);
		}
	}

	/**
	 * Définit la requête http (peut être nulle) pour le thread courant.
	 * @param request HttpServletRequest
	 */
	static void bindRequest(HttpServletRequest request) {
		if (request != null) {
			HTTP_SERVLET_REQUEST_CONTEXT.set(request);
		}
	}

	/**
	 * Retourne la requête http pour le thread courant ou null si elle n'a pas été définie.
	 * @return HttpServletRequest
	 */
	private static HttpServletRequest getCurrentRequest() {
		return HTTP_SERVLET_REQUEST_CONTEXT.get();
	}

	/**
	 * Enlève le lien entre la requête http et le thread courant.
	 */
	static void unbindRequest() {
		HTTP_SERVLET_REQUEST_CONTEXT.remove();
	}

	long getTime() {
		return time;
	}

	Date getDate() {
		return new Date(time);
	}

	String getRemoteUser() {
		return remoteUser;
	}

	String getHttpRequest() {
		return httpRequest;
	}

	String getMessage() {
		return message;
	}

	String getStackTrace() {
		return stackTrace;
	}
}
