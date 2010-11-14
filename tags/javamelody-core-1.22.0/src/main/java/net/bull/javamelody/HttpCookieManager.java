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

	Range getRange(HttpServletRequest req, HttpServletResponse resp) {
		final Range range;
		if (req.getParameter(PERIOD_PARAMETER) == null) {
			// pas de paramètre period dans la requête, on cherche le cookie
			final Cookie cookie = getCookieByName(req, PERIOD_COOKIE_NAME);
			if (cookie == null) {
				// pas de cookie, période jour par défaut
				range = Period.JOUR.getRange();
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
}
