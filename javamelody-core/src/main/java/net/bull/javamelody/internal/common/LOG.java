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

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;

import javax.servlet.http.HttpServletRequest;

import net.bull.javamelody.JavaMelodyLogger;
import net.bull.javamelody.Parameter;

/**
 * Logs des requêtes http exécutées et logs internes.
 * @author Emeric Vernat
 */
public final class LOG {
	public static final boolean LOG4J_ENABLED = isLog4jEnabled();
	public static final boolean LOG4J2_ENABLED = isLog4j2Enabled();
	public static final boolean LOGBACK_ENABLED = isLogbackEnabled();

	public static final int MAX_DEBUGGING_LOGS_COUNT = 50;

	private static final JavaMelodyLogger JAVA_MELODY_LOGGER = getJavaMelodyLogger();

	private static final LinkedList<String> DEBUGGING_LOGS = new LinkedList<String>(); // NOPMD

	private LOG() {
		super();
	}

	public static void logHttpRequest(HttpServletRequest httpRequest, String requestName,
			long duration, boolean systemError, int responseStatus, long responseSize,
			String filterName) {
		// dans les 3 implémentations, on ne construit le message de log
		// que si le logger est configuré pour écrire le niveau INFO
		JAVA_MELODY_LOGGER.logHttpRequest(httpRequest, requestName, duration, systemError,
				responseStatus, responseSize, filterName);
	}

	public static String buildLogMessage(HttpServletRequest httpRequest, long duration,
			boolean systemError, int responseStatus, long responseSize) {
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
			msg.append(", error ").append(responseStatus);
		}
		msg.append(", ").append(responseSize / 1024L).append(" Kb");
		return msg.toString();
	}

	public static void debug(String msg) {
		JAVA_MELODY_LOGGER.debug(msg);
		addDebuggingLog("DEBUG", msg);
	}

	public static void debug(String msg, Throwable throwable) {
		JAVA_MELODY_LOGGER.debug(msg, throwable);
		addDebuggingLog("DEBUG", msg);
	}

	public static void info(String msg, Throwable throwable) {
		JAVA_MELODY_LOGGER.info(msg, throwable);
		addDebuggingLog("INFO", msg);
	}

	public static void info(String msg) {
		JAVA_MELODY_LOGGER.info(msg);
		addDebuggingLog("INFO", msg);
	}

	public static void warn(String msg, Throwable throwable) {
		try {
			JAVA_MELODY_LOGGER.warn(msg, throwable);
			addDebuggingLog("WARN", msg);
		} catch (final Throwable t) { // NOPMD
			// au pire (cette méthode ne doit pas lancer d'erreur vu où elle est appelée)
			t.printStackTrace(System.err);
		}
	}

	public static List<String> getDebuggingLogs() {
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
			// test avec LocationInfo nécessaire car log4j-over-slf4j v1.7.6 inclut désormais l'AppenderSkeleton
			Class.forName("org.apache.log4j.spi.LocationInfo");
			return true;
		} catch (final Throwable e) { // NOPMD
			// catch Throwable et non catch ClassNotFoundException
			// pour certaines configurations de JBoss qui inclut Log4J (cf issue 166)
			return false;
		}
	}

	private static boolean isLog4j2Enabled() {
		try {
			Class.forName("org.apache.logging.log4j.Logger");
			// v2.4.1 is needed, so check ConfigurationBuilder which exists since v2.4
			Class.forName("org.apache.logging.log4j.core.config.builder.api.ConfigurationBuilder");
			return true;
		} catch (final Throwable e) { // NOPMD
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
			return Class.forName("ch.qos.logback.classic.LoggerContext")
					.isAssignableFrom(obj.getClass());
		} catch (final Throwable e) { // NOPMD
			return false;
		}
	}

	private static JavaMelodyLogger getJavaMelodyLogger() {
		// si le paramètre logger-class est défini, on en prend une instance comme JavaMelodyLogger
		final String loggerClass = Parameter.LOGGER_CLASS.getValue();
		if (loggerClass != null) {
			final ClassLoader tccl = Thread.currentThread().getContextClassLoader();
			try {
				return JavaMelodyLogger.class.cast(tccl.loadClass(loggerClass).newInstance());
			} catch (final Exception e) {
				throw new IllegalStateException(e);
			}
		}

		// sinon, on prend selon ce qui est présent Logback ou Log4J ou java.util.logging
		if (LOGBACK_ENABLED) {
			return new LogbackLogger();
		} else if (LOG4J2_ENABLED) {
			return new Log4J2Logger();
		} else if (LOG4J_ENABLED) {
			return new Log4JLogger();
		} else {
			return new JavaLogger();
		}
	}
}
