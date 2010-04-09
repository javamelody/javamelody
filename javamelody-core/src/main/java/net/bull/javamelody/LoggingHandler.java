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

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Collections;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogManager;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

/**
 * Handler pour les logs de java.util.logging, configuré automatiquement par MonitoringFilter.
 * @author Emeric Vernat
 */
public class LoggingHandler extends Handler {
	private static final Level MINIMUM_LEVEL = Level.WARNING;

	// Cette variable LOG_COUNTER conserve un état qui est global au filtre et à l'application (donc thread-safe).
	// On utilise un counter static pour le cas où logging (ou log4j) serait reconfiguré après la configuration
	// faite par le filtre. Il suffirait dans ce cas de déclarer LoggingHandler (ou Log4JAppender) dans le fichier
	// logging.properties (ou log4j.xml/log4j;properties) utilisé pour obtenir le même counter statique.
	private static final Counter LOG_COUNTER = new Counter(Counter.LOG_COUNTER_NAME, "log.png");
	static { // bloc d'initialisation statique
		LOG_COUNTER.setMaxRequestsCount(500);
	}

	private static final LoggingHandler SINGLETON = new LoggingHandler();

	/**
	 * Constructeur.
	 */
	public LoggingHandler() { // NOPMD
		super();
		// pas de level, pas de filter, pas de formatter car inutiles et trop lents
	}

	static LoggingHandler getSingleton() {
		return SINGLETON;
	}

	static Counter getLogCounter() {
		return LOG_COUNTER;
	}

	static void addErrorLogToCounter(String message, Throwable throwable) {
		if (throwable == null) {
			addErrorLogToCounter(message, (String) null);
		} else {
			final StringWriter stackTrace = new StringWriter(200);
			throwable.printStackTrace(new PrintWriter(stackTrace));
			addErrorLogToCounter(message, stackTrace.toString());
		}
	}

	static void addErrorLogToCounter(String message, String throwableStackTrace) {
		LOG_COUNTER.addRequestForSystemError(message, -1, -1, throwableStackTrace);
	}

	void register() {
		for (final String name : Collections.list(LogManager.getLogManager().getLoggerNames())) {
			Logger.getLogger(name).addHandler(this);
		}
	}

	void deregister() {
		for (final String name : Collections.list(LogManager.getLogManager().getLoggerNames())) {
			Logger.getLogger(name).removeHandler(this);
		}
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void publish(LogRecord record) {
		// ici on préfère ne pas appeler isLoggable(record) pour éviter un lock synchronized sur getLevel
		if (record.getLevel().intValue() < MINIMUM_LEVEL.intValue()) {
			return;
		}
		addErrorLogToCounter(record.getLevel().getName() + ": " + record.getMessage(), record
				.getThrown());
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
	public void flush() {
		// rien à faire
	}
}
