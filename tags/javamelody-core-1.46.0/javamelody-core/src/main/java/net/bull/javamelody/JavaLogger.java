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

import java.util.logging.Level;

import javax.servlet.http.HttpServletRequest;

/**
 * JavaMelodyLogger pour java.util.logging.
 * @author Emeric Vernat
 */
class JavaLogger implements JavaMelodyLogger {
	private static final java.util.logging.Logger LOGGER = java.util.logging.Logger
			.getLogger(INTERNAL_LOGGER_NAME);

	/** {@inheritDoc} */
	@Override
	public void debug(String msg) {
		LOGGER.log(Level.FINE, msg);
	}

	/** {@inheritDoc} */
	@Override
	public void debug(String msg, Throwable throwable) {
		LOGGER.log(Level.FINE, msg, throwable);
	}

	/** {@inheritDoc} */
	@Override
	public void info(String msg, Throwable throwable) {
		LOGGER.log(Level.INFO, msg, throwable);
	}

	/** {@inheritDoc} */
	@Override
	public void warn(String msg, Throwable throwable) {
		LOGGER.log(Level.WARNING, msg, throwable);
	}

	/** {@inheritDoc} */
	@Override
	public void logHttpRequest(HttpServletRequest httpRequest, String requestName, long duration,
			boolean systemError, int responseSize, String loggerName) {
		final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(loggerName);
		if (logger.isLoggable(Level.INFO)) {
			logger.info(LOG.buildLogMessage(httpRequest, duration, systemError, responseSize));
		}
	}
}
