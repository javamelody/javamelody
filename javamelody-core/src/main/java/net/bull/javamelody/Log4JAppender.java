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

import org.apache.log4j.AppenderSkeleton;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.log4j.PatternLayout;
import org.apache.log4j.spi.LoggingEvent;

/**
 * Appender pour les logs de log4j, configuré automatiquement par {@link MonitoringFilter}.
 * @author Emeric Vernat
 */
public class Log4JAppender extends AppenderSkeleton {
	private static final String MESSAGE_PATTERN = "%-5p [%c] %m%n";
	private static final Level THRESHOLD = Level.WARN;

	private static final Log4JAppender SINGLETON = new Log4JAppender();

	/**
	 * Constructeur.
	 */
	public Log4JAppender() {
		super();
		setLayout(new PatternLayout(MESSAGE_PATTERN));
		setThreshold(THRESHOLD);
		setName(getClass().getName());
	}

	static Log4JAppender getSingleton() {
		return SINGLETON;
	}

	void register() {
		Logger.getRootLogger().addAppender(this);
	}

	void deregister() {
		Logger.getRootLogger().removeAppender(this);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	protected void append(LoggingEvent event) {
		final Throwable throwable;
		if (event.getThrowableInformation() == null) {
			throwable = null;
		} else {
			throwable = event.getThrowableInformation().getThrowable();
		}
		LoggingHandler.addErrorLogToCounter(getLayout().format(event), throwable);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void close() {
		// rien à faire
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public boolean requiresLayout() {
		return false;
	}
}
