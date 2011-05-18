/*
 * Copyright 2008-2010 by Emeric Vernat
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

import java.util.logging.Level;

import javax.servlet.http.HttpServletRequest;

import org.slf4j.LoggerFactory;

/**
 * Logs des requêtes http exécutées et logs internes.
 * @author Emeric Vernat
 */
final class LOG {
	static final boolean LOG4J_ENABLED = isLog4jEnabled();
	static final boolean LOGBACK_ENABLED = isLogbackEnabled();
	private static final String INTERNAL_LOGGER_NAME = "net.bull.javamelody";

	private LOG() {
		super();
	}

	@SuppressWarnings("unused")
	static void logHttpRequest(HttpServletRequest httpRequest, String requestName, long duration,
			boolean systemError, int responseSize, String filterName) {
		// dans les 3 cas, on ne construit le message de log
		// que si le logger est configuré pour écrire le niveau INFO
		if (LOGBACK_ENABLED) {
			logback(httpRequest, duration, systemError, responseSize, filterName);
		} else if (LOG4J_ENABLED) {
			log4j(httpRequest, duration, systemError, responseSize, filterName);
		} else {
			final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(filterName);
			if (logger.isLoggable(Level.INFO)) {
				logger.info(buildLogMessage(httpRequest, duration, systemError, responseSize));
			}
		}
	}

	private static void log4j(HttpServletRequest httpRequest, long duration, boolean systemError,
			int responseSize, String filterName) {
		// la variable logger doit être dans une méthode à part pour ne pas faire ClassNotFoundException
		// si log4j non présent (mais variable préférable pour performance)
		final org.apache.log4j.Logger logger = org.apache.log4j.Logger.getLogger(filterName);
		if (logger.isInfoEnabled()) {
			logger.info(buildLogMessage(httpRequest, duration, systemError, responseSize));
		}
	}

	private static void logback(HttpServletRequest httpRequest, long duration, boolean systemError,
			int responseSize, String filterName) {
		// la variable logger doit être dans une méthode à part pour ne pas faire ClassNotFoundException
		// si logback non présent (mais variable préférable pour performance)
		final org.slf4j.Logger logger = org.slf4j.LoggerFactory.getLogger(filterName);
		if (logger.isInfoEnabled()) {
			logger.info(buildLogMessage(httpRequest, duration, systemError, responseSize));
		}
	}

	private static String buildLogMessage(HttpServletRequest httpRequest, long duration,
			boolean systemError, int responseSize) {
		final StringBuilder msg = new StringBuilder();
		msg.append("remoteAddr = ").append(httpRequest.getRemoteAddr());
		final String forwardedFor = httpRequest.getHeader("X-Forwarded-For");
		if (forwardedFor != null) {
			msg.append(", forwardedFor = ").append(forwardedFor);
		}
		msg.append(", request = ").append(
				httpRequest.getRequestURI().substring(httpRequest.getContextPath().length()));
		if (httpRequest.getQueryString() != null) {
			msg.append('?').append(httpRequest.getQueryString());
		}
		msg.append(' ').append(httpRequest.getMethod());
		msg.append(": ").append(duration).append(" ms");
		if (systemError) {
			msg.append(", erreur");
		}
		msg.append(", ").append(responseSize / 1024).append(" Ko");
		return msg.toString();
	}

	static void debug(String msg) {
		if (LOGBACK_ENABLED) {
			org.slf4j.LoggerFactory.getLogger(INTERNAL_LOGGER_NAME).debug(msg);
		} else if (LOG4J_ENABLED) {
			final org.apache.log4j.Logger logger = org.apache.log4j.Logger
					.getLogger(INTERNAL_LOGGER_NAME);
			logger.debug(msg);
		} else {
			final java.util.logging.Logger logger = java.util.logging.Logger
					.getLogger(INTERNAL_LOGGER_NAME);
			logger.log(Level.FINE, msg);
		}
	}

	static void debug(String msg, Throwable throwable) {
		if (LOGBACK_ENABLED) {
			org.slf4j.LoggerFactory.getLogger(INTERNAL_LOGGER_NAME).debug(msg, throwable);
		} else if (LOG4J_ENABLED) {
			final org.apache.log4j.Logger logger = org.apache.log4j.Logger
					.getLogger(INTERNAL_LOGGER_NAME);
			logger.debug(msg, throwable);
		} else {
			final java.util.logging.Logger logger = java.util.logging.Logger
					.getLogger(INTERNAL_LOGGER_NAME);
			logger.log(Level.FINE, msg, throwable);
		}
	}

	static void info(String msg, Throwable throwable) {
		if (LOGBACK_ENABLED) {
			org.slf4j.LoggerFactory.getLogger(INTERNAL_LOGGER_NAME).info(msg, throwable);
		} else if (LOG4J_ENABLED) {
			final org.apache.log4j.Logger logger = org.apache.log4j.Logger
					.getLogger(INTERNAL_LOGGER_NAME);
			logger.info(msg, throwable);
		} else {
			final java.util.logging.Logger logger = java.util.logging.Logger
					.getLogger(INTERNAL_LOGGER_NAME);
			logger.log(Level.INFO, msg, throwable);
		}
	}

	static void warn(String msg, Throwable throwable) {
		try {
			if (LOGBACK_ENABLED) {
				org.slf4j.LoggerFactory.getLogger(INTERNAL_LOGGER_NAME).warn(msg, throwable);
			} else if (LOG4J_ENABLED) {
				final org.apache.log4j.Logger logger = org.apache.log4j.Logger
						.getLogger(INTERNAL_LOGGER_NAME);
				logger.warn(msg, throwable);
			} else {
				final java.util.logging.Logger logger = java.util.logging.Logger
						.getLogger(INTERNAL_LOGGER_NAME);
				logger.log(Level.WARNING, msg, throwable);
			}
		} catch (final Throwable t) { // NOPMD
			// au pire (cette méthode ne doit pas lancer d'erreur vu où elle est appelée)
			t.printStackTrace(System.err);
		}
	}

	private static boolean isLog4jEnabled() {
		try {
			Class.forName("org.apache.log4j.Logger");
			// test avec AppenderSkeleton nécessaire car log4j-over-slf4j contient la classe
			// org.apache.log4j.Logger mais pas org.apache.log4j.AppenderSkeleton
			Class.forName("org.apache.log4j.AppenderSkeleton");
			return true;
		} catch (final ClassNotFoundException e) {
			return false;
		}
	}

	private static boolean isLogbackEnabled() {
		try {
			Class.forName("ch.qos.logback.classic.Logger");
			// on vérifie aussi LoggerContext car il peut arriver que getILoggerFactory ne soit pas ok (jonas)
			return Class.forName("ch.qos.logback.classic.LoggerContext").isAssignableFrom(
					LoggerFactory.getILoggerFactory().getClass());
		} catch (final ClassNotFoundException e) {
			return false;
		}
	}
}
