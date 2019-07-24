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

import java.io.Serializable;

import org.apache.logging.log4j.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.core.Appender;
import org.apache.logging.log4j.core.Filter;
import org.apache.logging.log4j.core.Layout;
import org.apache.logging.log4j.core.LogEvent;
import org.apache.logging.log4j.core.LoggerContext;
import org.apache.logging.log4j.core.appender.AbstractAppender;
import org.apache.logging.log4j.core.config.AbstractConfiguration;
import org.apache.logging.log4j.core.config.LoggerConfig;
import org.apache.logging.log4j.core.filter.ThresholdFilter;
import org.apache.logging.log4j.core.layout.PatternLayout;

/**
 * Appender pour les logs de log4j2, configuré automatiquement par {@link MonitoringFilter}.
 * @author Emeric Vernat
 */
public class Log4J2Appender extends AbstractAppender {
	private static final String MESSAGE_PATTERN = "%-5p [%c] %m%n";
	private static final Level THRESHOLD = Level.WARN;

	private static final String APPENDER_NAME = Log4J2Appender.class.getName();
	private static final PatternLayout LAYOUT = PatternLayout.newBuilder()
			.withPattern(MESSAGE_PATTERN).withNoConsoleNoAnsi(true).build();
	private static final ThresholdFilter FILTER = ThresholdFilter.createFilter(THRESHOLD, null,
			null);

	private static final Log4J2Appender SINGLETON = new Log4J2Appender();

	/**
	 * Constructeur.
	 */
	public Log4J2Appender() {
		this(APPENDER_NAME, FILTER, LAYOUT);
	}

	/**
	 * Constructeur.
	 * @param name String
	 * @param filter Filter
	 * @param layout Layout
	 */
	@SuppressWarnings("deprecation")
	public Log4J2Appender(final String name, final Filter filter,
			final Layout<? extends Serializable> layout) {
		super(name, filter, layout, true);
	}

	static Log4J2Appender getSingleton() {
		return SINGLETON;
	}

	void register() {
		if (LogManager.getContext(false) instanceof LoggerContext) {
			final LoggerContext ctx = (LoggerContext) LogManager.getContext(false);
			if (ctx.getConfiguration() instanceof AbstractConfiguration) {
				final AbstractConfiguration config = (AbstractConfiguration) ctx.getConfiguration();
				final Appender appender = getSingleton();
				appender.start();
				config.addAppender(appender);
				final Logger rootLogger = LogManager.getRootLogger();
				final LoggerConfig loggerConfig = config.getLoggerConfig(rootLogger.getName());
				loggerConfig.addAppender(appender, null, null);
				ctx.updateLoggers();
			}
		}
	}

	void deregister() {
		if (LogManager.getContext(false) instanceof LoggerContext) {
			final LoggerContext ctx = (LoggerContext) LogManager.getContext(false);
			if (ctx.getConfiguration() instanceof AbstractConfiguration) {
				final AbstractConfiguration config = (AbstractConfiguration) ctx.getConfiguration();
				final Appender appender = getSingleton();
				appender.stop();
				config.removeAppender(appender.getName());
				final Logger rootLogger = LogManager.getRootLogger();
				final LoggerConfig loggerConfig = config.getLoggerConfig(rootLogger.getName());
				loggerConfig.removeAppender(appender.getName());
				ctx.updateLoggers();
			}
		}
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void append(final LogEvent event) {
		final Throwable throwable = event.getThrown();
		final String message = getLayout().toSerializable(event).toString();
		LoggingHandler.addErrorLogToCounter(message, throwable);
	}
}
