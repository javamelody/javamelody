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
package net.bull.javamelody;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.LoggerContext;
import ch.qos.logback.classic.PatternLayout;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.UnsynchronizedAppenderBase;

/**
 * Appender pour les logs de logback, configuré automatiquement par {@link MonitoringFilter}.
 * @author Emeric Vernat
 */
public class LogbackAppender extends UnsynchronizedAppenderBase<ILoggingEvent> {
	private static final String MESSAGE_PATTERN = "%-5level %logger{36} - %msg%nopex%n";
	private static final String EXCEPTION_PATTERN = "%ex";
	private static final Level THRESHOLD = Level.WARN;

	private static final LogbackAppender SINGLETON = new LogbackAppender();

	private final PatternLayout exceptionLayout = new PatternLayout();

	private final PatternLayout messageLayout = new PatternLayout();

	/**
	 * Constructeur.
	 */
	public LogbackAppender() {
		super();
		final LoggerContext lc = getDefaultContext();
		messageLayout.setContext(lc);
		messageLayout.setPattern(MESSAGE_PATTERN);
		messageLayout.start();

		exceptionLayout.setContext(lc);
		exceptionLayout.setPattern(EXCEPTION_PATTERN);
		exceptionLayout.start();

		setContext(lc);
		start();
	}

	private static LoggerContext getDefaultContext() {
		return (LoggerContext) LoggerFactory.getILoggerFactory();
	}

	static LogbackAppender getSingleton() {
		return SINGLETON;
	}

	void register() {
		getDefaultContext().getLogger(Logger.ROOT_LOGGER_NAME).addAppender(this);
	}

	void deregister() {
		getDefaultContext().getLogger(Logger.ROOT_LOGGER_NAME).detachAppender(this);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	protected void append(ILoggingEvent event) {
		// inutile de vérifier que l'appender est bien "started",
		// car il est démarré dans le constructeur et si cela ne fonctionne pas il n'y a pas d'instance
		if (event.getLevel().isGreaterOrEqual(THRESHOLD)) {
			final String output = messageLayout.doLayout(event);
			String stackTrace = exceptionLayout.doLayout(event);
			if (stackTrace.isEmpty()) {
				stackTrace = null;
			}
			LoggingHandler.addErrorLogToCounter(output, stackTrace);
		}
	}
}
