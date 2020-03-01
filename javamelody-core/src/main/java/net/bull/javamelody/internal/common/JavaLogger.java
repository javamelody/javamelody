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
package net.bull.javamelody.internal.common;

import java.util.logging.Level;

import javax.servlet.http.HttpServletRequest;

import net.bull.javamelody.JavaMelodyLogger;

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
	public void info(String msg) {
		LOGGER.log(Level.INFO, msg);
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
			boolean systemError, int responseStatus, long responseSize, String loggerName) {
		final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(loggerName);
		if (logger.isLoggable(Level.INFO)) {
			logger.info(LOG.buildLogMessage(httpRequest, duration, systemError, responseStatus,
					responseSize));
		}
	}
}
