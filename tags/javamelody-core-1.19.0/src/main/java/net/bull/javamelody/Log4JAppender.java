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
	private static final Level MINIMUM_LEVEL = Level.WARN;

	private static final Log4JAppender SINGLETON = new Log4JAppender();

	/**
	 * Constructeur.
	 */
	public Log4JAppender() {
		super();
		setLayout(new PatternLayout(MESSAGE_PATTERN));
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
		if (event.getLevel().isGreaterOrEqual(MINIMUM_LEVEL)) {
			final Throwable throwable;
			if (event.getThrowableInformation() == null) {
				throwable = null;
			} else {
				throwable = event.getThrowableInformation().getThrowable();
			}
			LoggingHandler.addErrorLogToCounter(getLayout().format(event), throwable);
		}
	}

	/**
	 * {@inheritDoc}
	 */
	public void close() {
		// rien à faire
	}

	/**
	 * {@inheritDoc}
	 */
	public boolean requiresLayout() {
		return false;
	}
}
