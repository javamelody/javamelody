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

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;

import javax.servlet.http.HttpServletRequest;

/**
 * Logs des requêtes http exécutées et logs internes.
 * @author Emeric Vernat
 */
final class LOG {
	static final boolean LOG4J_ENABLED = isLog4jEnabled();
	static final boolean LOGBACK_ENABLED = isLogbackEnabled();

	static final int MAX_DEBUGGING_LOGS_COUNT = 50;

	private static final JavaMelodyLogger JAVA_MELODY_LOGGER = getJavaMelodyLogger();

	//CHECKSTYLE:OFF
	private static final LinkedList<String> DEBUGGING_LOGS = new LinkedList<String>(); // NOPMD

	//CHECKSTYLE:ON

	private LOG() {
		super();
	}

	static void logHttpRequest(HttpServletRequest httpRequest, String requestName, long duration,
			boolean systemError, int responseSize, String filterName) {
		// dans les 3 implémentations, on ne construit le message de log
		// que si le logger est configuré pour écrire le niveau INFO
		JAVA_MELODY_LOGGER.logHttpRequest(httpRequest, requestName, duration, systemError,
				responseSize, filterName);
	}

	public static String buildLogMessage(HttpServletRequest httpRequest, long duration,
			boolean systemError, int responseSize) {
		final StringBuilder msg = new StringBuilder();
		msg.append("remoteAddr = ").append(httpRequest.getRemoteAddr());
		final String forwardedFor = httpRequest.getHeader("X-Forwarded-For");
		if (forwardedFor != null) {
			msg.append(", forwardedFor = ").append(forwardedFor);
		}
		msg.append(", request = ").append(
				httpRequest.getRequestURI().substring(httpRequest.getContextPath().length()));
		if (httpRequest.getQueryString() != null) {
			msg.append('?').append(httpRequest.getQueryString());
		}
		msg.append(' ').append(httpRequest.getMethod());
		msg.append(": ").append(duration).append(" ms");
		if (systemError) {
			msg.append(", erreur");
		}
		msg.append(", ").append(responseSize / 1024).append(" Ko");
		return msg.toString();
	}

	static void debug(String msg) {
		JAVA_MELODY_LOGGER.debug(msg);
		addDebuggingLog("DEBUG", msg);
	}

	static void debug(String msg, Throwable throwable) {
		JAVA_MELODY_LOGGER.debug(msg, throwable);
		addDebuggingLog("DEBUG", msg);
	}

	static void info(String msg, Throwable throwable) {
		JAVA_MELODY_LOGGER.info(msg, throwable);
		addDebuggingLog("INFO", msg);
	}

	static void warn(String msg, Throwable throwable) {
		try {
			JAVA_MELODY_LOGGER.warn(msg, throwable);
			addDebuggingLog("WARN", msg);
		} catch (final Throwable t) { // NOPMD
			// au pire (cette méthode ne doit pas lancer d'erreur vu où elle est appelée)
			t.printStackTrace(System.err);
		}
	}

	static List<String> getDebuggingLogs() {
		synchronized (DEBUGGING_LOGS) {
			return new ArrayList<String>(DEBUGGING_LOGS);
		}
	}

	private static void addDebuggingLog(String level, String msg) {
		synchronized (DEBUGGING_LOGS) {
			DEBUGGING_LOGS.addLast(new Date().toString() + '\t' + level + '\t' + msg);
			while (DEBUGGING_LOGS.size() > MAX_DEBUGGING_LOGS_COUNT) {
				DEBUGGING_LOGS.removeFirst();
			}
		}
	}

	private static boolean isLog4jEnabled() {
		try {
			Class.forName("org.apache.log4j.Logger");
			// test avec AppenderSkeleton nécessaire car log4j-over-slf4j contient la classe
			// org.apache.log4j.Logger mais pas org.apache.log4j.AppenderSkeleton
			Class.forName("org.apache.log4j.AppenderSkeleton");
			return true;
		} catch (final Throwable e) { // NOPMD
			// catch Throwable et non catch ClassNotFoundException
			// pour certaines configurations de JBoss qui inclut Log4J (cf issue 166)
			return false;
		}
	}

	private static boolean isLogbackEnabled() {
		try {
			Class.forName("ch.qos.logback.classic.Logger");
			final Class<?> loggerFactoryClass = Class.forName("org.slf4j.LoggerFactory");
			final Method method = loggerFactoryClass.getMethod("getILoggerFactory");
			final Object obj = method.invoke(null);

			// on vérifie aussi LoggerContext car il peut arriver que getILoggerFactory ne soit pas ok (jonas)
			return Class.forName("ch.qos.logback.classic.LoggerContext").isAssignableFrom(
					obj.getClass());
		} catch (final Throwable e) { // NOPMD
			return false;
		}
	}

	private static JavaMelodyLogger getJavaMelodyLogger() {
		if (LOGBACK_ENABLED) {
			return new LogbackLogger();
		} else if (LOG4J_ENABLED) {
			return new Log4JLogger();
		} else {
			return new JavaLogger();
		}
	}
}
