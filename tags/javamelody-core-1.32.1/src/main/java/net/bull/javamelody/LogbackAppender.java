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
			if (stackTrace.length() == 0) {
				stackTrace = null;
			}
			LoggingHandler.addErrorLogToCounter(output, stackTrace);
		}
	}
}
