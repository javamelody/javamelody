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
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.regex.Pattern;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import net.bull.javamelody.Parameter;
import net.bull.javamelody.internal.common.LOG;
import net.bull.javamelody.internal.model.Base64Coder;

/**
 * Authentification http des rapports.
 * @author Emeric Vernat
 */
public class HttpAuth {
	private static final long AUTH_FAILURES_MAX = 10;

	private static final long LOCK_DURATION = 60L * 60 * 1000;

	private final Pattern allowedAddrPattern;
	/**
	 * List of authorized people, when using the "authorized-users" parameter.
	 */
	private final List<String> authorizedUsers;

	private final AtomicInteger authFailuresCount = new AtomicInteger();

	private Date firstFailureDate;

	public HttpAuth() {
		super();
		this.allowedAddrPattern = getAllowedAddrPattern();
		this.authorizedUsers = getAuthorizedUsers();
	}

	private static Pattern getAllowedAddrPattern() {
		if (Parameter.ALLOWED_ADDR_PATTERN.getValue() != null) {
			return Pattern.compile(Parameter.ALLOWED_ADDR_PATTERN.getValue());
		}
		return null;
	}

	private static List<String> getAuthorizedUsers() {
		// security based on user / password (BASIC auth)
		final String authUsersInParam = Parameter.AUTHORIZED_USERS.getValue();
		if (authUsersInParam != null && !authUsersInParam.trim().isEmpty()) {
			final List<String> authorizedUsers = new ArrayList<String>();
			// we split on new line or on comma
			for (final String authUser : authUsersInParam.split("[\n,]")) {
				final String authUserTrim = authUser.trim();
				if (!authUserTrim.isEmpty()) {
					authorizedUsers.add(authUserTrim);
					LOG.debug("Authorized user: " + authUserTrim.split(":", 2)[0]);
				}
			}
			return authorizedUsers;
		}
		return null;
	}

	public boolean isAllowed(HttpServletRequest httpRequest, HttpServletResponse httpResponse)
			throws IOException {
		if (!isRequestAllowed(httpRequest)) {
			LOG.debug("Forbidden access to monitoring from " + httpRequest.getRemoteAddr());
			httpResponse.sendError(HttpServletResponse.SC_FORBIDDEN, "Forbidden access");
			return false;
		}
		if (!isUserAuthorized(httpRequest)) {
			// Not allowed, so report he's unauthorized
			httpResponse.setHeader("WWW-Authenticate", "BASIC realm=\"JavaMelody\"");
			if (isLocked()) {
				httpResponse.sendError(HttpServletResponse.SC_UNAUTHORIZED,
						"Unauthorized (locked)");
			} else {
				httpResponse.sendError(HttpServletResponse.SC_UNAUTHORIZED, "Unauthorized");
			}
			return false;
		}
		return true;
	}

	private boolean isRequestAllowed(HttpServletRequest httpRequest) {
		return allowedAddrPattern == null
				|| allowedAddrPattern.matcher(httpRequest.getRemoteAddr()).matches();
	}

	/**
	 * Check if the user is authorized, when using the "authorized-users" parameter
	 * @param httpRequest HttpServletRequest
	 * @return true if the user is authorized
	 */
	private boolean isUserAuthorized(HttpServletRequest httpRequest) {
		if (authorizedUsers == null) {
			return true;
		}
		// Get Authorization header
		final String auth = httpRequest.getHeader("Authorization");
		if (auth == null) {
			return false; // no auth
		}
		if (!auth.toUpperCase(Locale.ENGLISH).startsWith("BASIC ")) {
			return false; // we only do BASIC
		}
		// Get encoded "user:password", comes after "BASIC "
		final String userpassEncoded = auth.substring("BASIC ".length());
		// Decode it
		final String userpassDecoded = Base64Coder.decodeString(userpassEncoded);

		final boolean authOk = authorizedUsers.contains(userpassDecoded);
		return checkLockAgainstBruteForceAttack(authOk);
	}

	private boolean checkLockAgainstBruteForceAttack(boolean authOk) {
		if (firstFailureDate == null) {
			if (!authOk) {
				// auth failed for the first time, insert coin to try again
				firstFailureDate = new Date();
				authFailuresCount.set(1);
			}
		} else {
			if (isLocked()) {
				// if too many failures, lock auth attemps for some time
				if (System.currentTimeMillis() - firstFailureDate.getTime() < LOCK_DURATION) {
					return false;
				}
				// lock is expired, reset
				firstFailureDate = null;
				authFailuresCount.set(0);
				return checkLockAgainstBruteForceAttack(authOk);
			}
			if (authOk) {
				// no more failure, reset
				firstFailureDate = null;
				authFailuresCount.set(0);
			} else {
				// one more failure, insert coin to try again
				authFailuresCount.incrementAndGet();
			}
		}
		return authOk;
	}

	private boolean isLocked() {
		return authFailuresCount.get() > AUTH_FAILURES_MAX;
	}
}
