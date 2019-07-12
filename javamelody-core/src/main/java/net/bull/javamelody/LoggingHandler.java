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

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Collections;
import java.util.ConcurrentModificationException;
import java.util.List;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogManager;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

import net.bull.javamelody.internal.model.Counter;

/**
 * Handler pour les logs de java.util.logging, configuré automatiquement par {@link MonitoringFilter}.
 * @author Emeric Vernat
 */
public class LoggingHandler extends Handler {
	private static final Level THRESHOLD = Level.WARNING;

	// Cette variable LOG_COUNTER conserve un état qui est global au filtre et à l'application (donc thread-safe).
	// On utilise un counter static pour le cas où logging (ou log4j) serait reconfiguré après la configuration
	// faite par le filtre. Il suffirait dans ce cas de déclarer LoggingHandler (ou Log4JAppender) dans le fichier
	// logging.properties (ou log4j.xml/log4j.properties) utilisé pour obtenir le même counter statique.
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
		if (LOG_COUNTER.isDisplayed()) {
			LOG_COUNTER.addRequestForSystemError(message, -1, -1, -1, throwableStackTrace);
		}
	}

	void register() {
		for (final String name : getLoggerNames()) {
			Logger.getLogger(name).addHandler(this);
		}
	}

	void deregister() {
		for (final String name : getLoggerNames()) {
			Logger.getLogger(name).removeHandler(this);
		}
	}

	private List<String> getLoggerNames() {
		while (true) {
			try {
				return Collections.list(LogManager.getLogManager().getLoggerNames());
			} catch (final ConcurrentModificationException e) {
				// retry
				// (issue 370 and http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=6935026 )
				continue;
			}
		}
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void publish(LogRecord record) {
		// ici on préfère ne pas appeler isLoggable(record) pour éviter un lock synchronized sur getLevel
		if (record.getLevel().intValue() < THRESHOLD.intValue()) {
			return;
		}
		addErrorLogToCounter(record.getLevel().getName() + ": " + record.getMessage(),
				record.getThrown());
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
