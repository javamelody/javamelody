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

import static net.bull.javamelody.HttpParameters.PERIOD_PARAMETER;

import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * Gestionnaire de cookie http persistent.
 * @author Emeric Vernat
 */
class HttpCookieManager {
	private static final String PERIOD_COOKIE_NAME = "javamelody.period";

	// période par défaut : jour
	private static Range defaultRange = Period.JOUR.getRange();

	Range getRange(HttpServletRequest req, HttpServletResponse resp) {
		final Range range;
		if (req.getParameter(PERIOD_PARAMETER) == null) {
			// pas de paramètre period dans la requête, on cherche le cookie
			final Cookie cookie = getCookieByName(req, PERIOD_COOKIE_NAME);
			if (cookie == null) {
				// pas de cookie, période par défaut
				range = defaultRange;
			} else {
				range = Range.parse(cookie.getValue());
			}
		} else {
			range = Range.parse(req.getParameter(PERIOD_PARAMETER));
			// un paramètre period est présent dans la requête :
			// l'utilisateur a choisi une période, donc on fixe le cookie
			addCookie(req, resp, PERIOD_COOKIE_NAME, range.getValue());
		}
		return range;
	}

	Cookie getCookieByName(HttpServletRequest req, String cookieName) {
		final Cookie[] cookies = req.getCookies();
		if (cookies != null) {
			for (final Cookie cookie : cookies) {
				if (cookieName.equals(cookie.getName())) {
					return cookie;
				}
			}
		}
		return null;
	}

	void addCookie(HttpServletRequest req, HttpServletResponse resp, String cookieName,
			String cookieValue) {
		if (!"added".equals(req.getAttribute(cookieName))) {
			final Cookie cookie = new Cookie(cookieName, cookieValue);
			// cookie persistant, valide pendant 30 jours
			cookie.setMaxAge(30 * 24 * 60 * 60);
			// inutile d'envoyer ce cookie aux autres URLs que le monitoring
			cookie.setPath(req.getRequestURI());
			resp.addCookie(cookie);
			req.setAttribute(cookieName, "added");
		}
	}

	String getCookiesAsString(HttpServletRequest req) {
		final Cookie[] cookies = req.getCookies();
		if (cookies != null) {
			final StringBuilder sb = new StringBuilder();
			for (int i = 0; i < cookies.length; i++) {
				final Cookie cookie = cookies[i];
				sb.append(cookie.getName()).append('=').append(cookie.getValue());
				if (i < cookies.length - 1) {
					sb.append("; ");
				}
			}
			return sb.toString();
		}
		return null;
	}

	static void setDefaultRange(Range range) {
		defaultRange = range;
	}
}
