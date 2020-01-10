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

import javax.servlet.http.HttpServletRequest;

import org.slf4j.Logger;

import net.bull.javamelody.JavaMelodyLogger;

/**
 * {@link JavaMelodyLogger} pour Logback.
 * @author Emeric Vernat
 */
class LogbackLogger implements JavaMelodyLogger {
	private static final Logger LOGGER = org.slf4j.LoggerFactory.getLogger(INTERNAL_LOGGER_NAME);

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
	public void info(String msg) {
		LOGGER.info(msg);
	}

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
	public void logHttpRequest(HttpServletRequest httpRequest, String requestName, long duration,
			boolean systemError, int responseStatus, long responseSize, String loggerName) {
		final Logger logger = org.slf4j.LoggerFactory.getLogger(loggerName);
		if (logger.isInfoEnabled()) {
			logger.info(LOG.buildLogMessage(httpRequest, duration, systemError, responseStatus,
					responseSize));
		}
	}
}
