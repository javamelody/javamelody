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

import javax.servlet.http.HttpServletRequest;

/**
 * JavaMelodyLogger pour Log4J.
 * @author Emeric Vernat
 */
class Log4JLogger implements JavaMelodyLogger {
	private static final org.apache.log4j.Logger LOGGER = org.apache.log4j.Logger
			.getLogger(INTERNAL_LOGGER_NAME);

	/** {@inheritDoc} */
	@Override
	public void info(String msg, Throwable throwable) {
		LOGGER.info(msg, throwable);
	}

	/** {@inheritDoc} */
	@Override
	public void warn(String msg, Throwable throwable) {
		LOGGER.warn(msg, throwable);
	}

	/** {@inheritDoc} */
	@Override
	public void debug(String msg) {
		LOGGER.debug(msg);
	}

	/** {@inheritDoc} */
	@Override
	public void debug(String msg, Throwable throwable) {
		LOGGER.debug(msg, throwable);
	}

	/** {@inheritDoc} */
	@Override
	public void logHttpRequest(HttpServletRequest httpRequest, String requestName, long duration,
			boolean systemError, int responseSize, String loggerName) {
		final org.apache.log4j.Logger logger = org.apache.log4j.Logger.getLogger(loggerName);
		if (logger.isInfoEnabled()) {
			logger.info(LOG.buildLogMessage(httpRequest, duration, systemError, responseSize));
		}
	}
}
